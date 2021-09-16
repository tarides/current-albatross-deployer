module Git = Current_git
module Docker = Current_docker.Default
open Lwt.Syntax

module Teleporter : sig
  module type S = sig
    type t

    val digest : t -> string

    val marshal : t -> string

    val unmarshal : string -> t

    val pp : t Fmt.t
  end

  type t

  type 'a staged = {
    live : 'a;
    (* value kept alive by the stager *)
    current : 'a; (* current value like if the stager didn't exist *)
  }

  val v : unit -> t

  val stage_auto :
    t ->
    id:string ->
    (module S with type t = 'a) ->
    'a Current.t ->
    'a Current.t staged

  (* promote the stages when the current has a value *)
  val activate : t -> unit Current.t -> unit Current.t
end = struct
  type t = { pool : unit Current.Pool.t; activator : unit -> unit }

  type 'a staged = {
    live : 'a;
    (* value kept alive by the stager *)
    current : 'a; (* current value like if the stager didn't exist *)
  }

  let v () =
    let condition = Lwt_condition.create () in
    let pool =
      Current.Pool.of_fn ~label:"teleporter" (fun ~priority:_ ~switch:_ ->
          let cancel = Lwt_condition.create () in
          let ressource =
            Lwt.pick [ Lwt_condition.wait condition; Lwt_condition.wait cancel ]
          in
          ( ressource,
            fun () -> Lwt.return (Lwt_condition.broadcast condition ()) ))
    in
    { pool; activator = Lwt_condition.broadcast condition }

  module type S = sig
    type t

    val digest : t -> string

    val marshal : t -> string

    val unmarshal : string -> t

    val pp : t Fmt.t
  end

  module Op (M : S) = struct
    type nonrec t = t

    module Key = struct
      type t = string

      let digest = Fun.id
    end

    module Value = M
    module Outcome = M

    let pp f (_, v) = M.pp f v

    let id = "stager"

    let auto_cancel = true

    let latched = true

    let run { pool; _ } job _ key =
      let* () = Current.Job.start_with ~pool ~level:Harmless job in
      Current.Job.log job "Stager update validated";
      Lwt.return_ok key
  end

  let stage_auto (type a) t ~id (module M : S with type t = a) v =
    let module Stager = Current_cache.Generic (Op (M)) in
    let live =
      let open Current.Syntax in
      Current.component "stager"
      |> let> v = v in
         Stager.run t id v
    in
    { current = v; live }

  module OpActivator = struct
    type nonrec t = t

    let id = "activator"

    let pp f () = Fmt.string f "activator"

    module Key = struct
      type t = unit

      let digest () = Random.int 1_000_000_000 |> string_of_int
    end

    module Value = Current.Unit

    let auto_cancel = true

    let build { activator; _ } job () =
      let* () = Current.Job.start ~level:Dangerous job in
      Current.Job.log job "Stager update !";
      activator ();
      Lwt.return_ok ()
  end

  module Activator = Current_cache.Make (OpActivator)

  let activate t v =
    let open Current.Syntax in
    Current.component "activator"
    |> let> () = v in
       Activator.get t ()
end

module E = Current_deployer.E

let pipeline () =
  let open Current.Syntax in
  let git =
    Git.Local.v Fpath.(v "/home/lucas/mirage/egarim/dns-resolver.git/")
  in
  let src = Git.Local.commit_of_ref git "refs/heads/master" in
  let dockerfile = Current.return (`File (Fpath.v "Dockerfile")) in
  let image =
    Docker.build (`Git src)
      ~build_args:[ "--build-arg"; "TARGET=hvt" ]
      ~dockerfile ~label:"docker-build" ~pull:true
  in
  (* 3: forward *)
  let config_forward =
    let+ image = image in
    let unikernel =
      Current_deployer.Unikernel.of_docker ~image
        ~location:(Fpath.v "/unikernel.hvt")
    in
    {
      E.Config.Pre.service = "service-forwarder";
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
      network = "br1";
    }
  in
  let ip_forward = E.get_ip config_forward in
  let config_forward =
    let+ ip_forward = ip_forward and+ config_forward = config_forward in
    E.Config.v config_forward ip_forward
  in
  (* 2: dkim signer *)
  let config_signer =
    let+ image = image and+ ip_forward = ip_forward in
    let unikernel =
      Current_deployer.Unikernel.of_docker ~image
        ~location:(Fpath.v "/unikernel.hvt")
    in
    {
      E.Config.Pre.service = "service-signer";
      unikernel;
      args =
        (fun ip ->
          [
            "--ipv4=" ^ Ipaddr.V4.to_string ip ^ "/24";
            "--ipv4-gateway=" ^ Ipaddr.V4.to_string ip_forward
            (*fake stuff here*);
          ]);
      memory = 256;
      network = "br1";
    }
  in
  let ip_signer = E.get_ip config_signer in
  let config_signer =
    let+ ip_signer = ip_signer and+ config_signer = config_signer in
    E.Config.v config_signer ip_signer
  in
  (* 1: entry *)
  let config_entry =
    let+ image = image and+ ip_signer = ip_signer in
    let unikernel =
      Current_deployer.Unikernel.of_docker ~image
        ~location:(Fpath.v "/unikernel.hvt")
    in
    {
      E.Config.Pre.service = "service-entry";
      unikernel;
      args =
        (fun ip ->
          [
            "--ipv4=" ^ Ipaddr.V4.to_string ip ^ "/24";
            "--ipv4-gateway=" ^ Ipaddr.V4.to_string ip_signer
            (*fake stuff here*);
          ]);
      memory = 256;
      network = "br1";
    }
  in
  let ip_entry = E.get_ip config_entry in
  let config_entry =
    let+ ip_entry = ip_entry and+ config_entry = config_entry in
    E.Config.v config_entry ip_entry
  in

  (* Deployments *)
  let deploy_forward = E.deploy_albatross config_forward in
  let deploy_signer = E.deploy_albatross config_signer in
  let deploy_entry = E.deploy_albatross config_entry in
  (* Publish: staged *)
  let teleporter = Teleporter.v () in
  let staged_forward =
    Teleporter.stage_auto teleporter ~id:"forward"
      (module E.Info)
      deploy_forward
  in
  let staged_signer =
    Teleporter.stage_auto teleporter ~id:"signer" (module E.Info) deploy_signer
  in
  let staged_entry =
    Teleporter.stage_auto teleporter ~id:"entry" (module E.Info) deploy_entry
  in
  let publish_no_ports =
    [
      ("forward-live", staged_forward.live);
      ("forward-current", staged_forward.current);
      ("signer-live", staged_signer.live);
      ("signer-current", staged_signer.current);
    ]
    |> List.map (fun (service, current) -> E.publish ~service current)
  in
  let publish_entry_live =
    E.publish ~service:"entry-live"
      ~ports:[ { Current_deployer.Port.source = 25; target = 25 } ]
      staged_entry.live
  in
  let publish_entry_current =
    E.publish ~service:"entry-current"
      ~ports:[ { Current_deployer.Port.source = 2525; target = 25 } ]
      staged_entry.current
  in
  let all_deployments =
    Current.list_seq
      (publish_entry_live :: publish_entry_current :: publish_no_ports)
  in
  Current.all
    [
      E.collect all_deployments;
      [ deploy_forward; deploy_entry; deploy_signer ]
      |> List.map Current.ignore_value
      |> Current.all
      |> Teleporter.activate teleporter;
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

(* Command-line parsing *)

open Cmdliner

let cmd =
  let doc = "build and deploy services from Git" in
  ( Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner),
    Term.info "deploy" ~doc )

let () = Term.(exit @@ eval cmd)
