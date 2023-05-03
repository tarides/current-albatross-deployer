module Git = Current_git
module Docker = Current_docker.Default
module Deployer = Current_albatross_deployer

let pipeline () =
  let open Current.Syntax in
  let src_mirage_4 =
    Git.clone
      ~schedule:(Current_cache.Schedule.v ~valid_for:(Duration.of_hour 1) ())
      ~gref:"next" "https://github.com/TheLortex/mirage-www.git"
  in
  let src_mirage_3 =
    Git.clone
      ~schedule:(Current_cache.Schedule.v ~valid_for:(Duration.of_hour 1) ())
      ~gref:"master" "https://github.com/mirage/mirage-skeleton.git"
  in
  let get_ip =
    Deployer.get_ip
      ~blacklist:[ Ipaddr.V4.of_string_exn "10.0.0.1" ]
      ~prefix:(Ipaddr.V4.Prefix.of_string_exn "10.0.0.0/24")
  in
  let config_mirage_4 =
    let+ unikernel =
      Deployer.Unikernel.of_git ~mirage_version:`Mirage_4
        ~config_file:(Current.return (Fpath.v "src/config.ml"))
        src_mirage_4
    in
    {
      Deployer.Config.Pre.service = "website";
      unikernel;
      args =
        (fun ip ->
          [
            "--ipv4=" ^ Ipaddr.V4.to_string ip ^ "/24";
            "--ipv4-gateway=10.0.0.1";
            "-l";
            "error";
          ]);
      memory = 256;
      network = "br0";
      cpu = 0;
    }
  in
  let config_mirage_3 =
    let+ unikernel =
      Deployer.Unikernel.of_git ~mirage_version:`Mirage_3
        ~config_file:(Current.return (Fpath.v "tutorial/noop/config.ml"))
        src_mirage_3
    in
    {
      Deployer.Config.Pre.service = "website";
      unikernel;
      args =
        (fun ip ->
          [
            "--ipv4=" ^ Ipaddr.V4.to_string ip ^ "/24";
            "--ipv4-gateway=10.0.0.1";
            "-l";
            "error";
          ]);
      memory = 256;
      network = "br0";
      cpu = 0;
    }
  in
  let config_mirage_4 =
    let+ ip = get_ip config_mirage_4 and+ config = config_mirage_4 in
    Deployer.Config.v config ip
  in
  let config_mirage_3 =
    let+ ip = get_ip config_mirage_3 and+ config = config_mirage_3 in
    Deployer.Config.v config ip
  in
  (* Deployments *)
  let deploy_4 = Deployer.deploy_albatross ~label:"website.4" config_mirage_4 in
  let deploy_3 = Deployer.deploy_albatross ~label:"website.3" config_mirage_3 in

  let publish_4 =
    Deployer.publish ~service:"website.4"
      ~ports:[ { Deployer.Port.source = 8888; target = 80 } ]
      deploy_4
  in
  let publish_3 =
    Deployer.publish ~service:"website.3"
      ~ports:[ { Deployer.Port.source = 8889; target = 80 } ]
      deploy_3
  in
  let deployments = Current.list_seq [ publish_4; publish_3 ] in
  Current.all
    [
      Deployer.collect deployments;
      Deployer.monitor deploy_4 |> Deployer.is_running;
      Deployer.monitor deploy_3 |> Deployer.is_running;
    ]

let () =
  Logs.(set_level (Some Info));
  Logs.set_reporter (Logs_fmt.reporter ())

let main config mode =
  let engine = Current.Engine.create ~config pipeline in
  let routes = Current_web.routes engine in
  let site =
    Current_web.Site.v ~has_role:Current_web.Site.allow_all
      ~name:"OCurrent Deployer" routes
  in
  Lwt.choose
    [
      Current.Engine.thread engine;
      (* The main thread evaluating the pipeline. *)
      Current_web.run ~mode site;
    ]
  |> Lwt_main.run
  |> function
  | Ok _ as x -> x
  | Error (`Msg e) -> Error e

(* Command-line parsing *)

open Cmdliner

let cmd =
  let doc = "build and deploy services from Git" in
  Cmd.v (Cmd.info "deploy" ~doc)
    Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner)

let () = exit Cmd.(eval_result cmd)
