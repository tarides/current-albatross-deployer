module Git = Current_git
module Docker = Current_docker.Default
module E = Current_deployer

let pipeline () =
  let open Current.Syntax in
  let src =
    Git.clone
      ~schedule:(Current_cache.Schedule.v ~valid_for:(Duration.of_hour 1) ())
      ~gref:"next" "https://github.com/TheLortex/mirage-www.git"
  in
  let get_ip =
    E.get_ip
      ~blacklist:[ Ipaddr.V4.of_string_exn "10.0.0.1" ]
      ~prefix:(Ipaddr.V4.Prefix.of_string_exn "10.0.0.0/24")
  in
  (* 3: *)
  let config =
    let+ unikernel =
      Current_deployer.Unikernel.of_git ~mirage_version:`Mirage_4
        ~config_file:(Current.return (Fpath.v "src/config.ml"))
        src
    in
    {
      E.Config.Pre.service = "website";
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
    }
  in
  let ip = get_ip config in
  let config =
    let+ ip = ip and+ config = config in
    E.Config.v config ip
  in
  (* Deployments *)
  let deploy = E.deploy_albatross ~label:"website" config in

  (* Publish: staged *)
  let publish =
    E.publish ~service:"website"
      ~ports:[ { Current_deployer.Port.source = 8888; target = 80 } ]
      deploy
  in
  let deployments = Current.list_seq [ publish ] in
  Current.all [ E.collect deployments; E.monitor deploy |> E.is_running ]

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

(* Command-line parsing *)

open Cmdliner

let cmd =
  let doc = "build and deploy services from Git" in
  ( Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner),
    Term.info "deploy" ~doc )

let () = Term.(exit @@ eval cmd)
