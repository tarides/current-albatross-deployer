(** Deploy the classic hello unikernel via the albatrossd TLS endpoint *)

open Current.Syntax
open Current_albatross_deployer
module Git = Current_git

let repo : Git.Commit.t Current.t =
  Git.clone
    ~schedule:(Current_cache.Schedule.v ())
    ~gref:"main" "https://github.com/mirage/mirage-skeleton.git"

let unikernel : Unikernel.t Current.t =
  let mirage_version = `Mirage_4 in
  let config_file = Fpath.v "tutorial/hello/config.ml" |> Current.return in
  Unikernel.of_git ~mirage_version ~config_file repo

let name = "hello.tls"

let config_pre : Config.Pre.t Current.t =
  let+ unikernel = unikernel in
  {
    Config.Pre.service = name;
    unikernel;
    args = (fun _ -> []);
    memory = 256;
    network = None;
    cpu = 0;
  }

let config : Config.t Current.t =
  let ip = Ipaddr.V4.any in
  let+ config = config_pre in
  Config.v config ip

let pipeline tls () =
  let mode = `Tls tls in
  (* Deploy via TLS *)
  let deployed = deploy_albatross ~mode config in
  let is_running = monitor ~poll_rate:1. deployed |> is_running in
  (* Publication is still via iptables socket *)
  let published = publish ~service:name ~ports:[] deployed in
  (* Collection collects unused unikernels via TLS and IPs via socket *)
  let collection = Current.list_seq [ published ] |> collect ~mode in
  Current.all [ is_running; collection ]

let () = Prometheus_unix.Logging.init ()

let main config mode tls =
  let engine = Current.Engine.create ~config (pipeline tls) in
  let site =
    Current_web.Site.(v ~refresh_pipeline:5 ~has_role:allow_all)
      ~name
      (Current_web.routes engine)
  in
  Lwt_main.run
  @@ Lwt.choose [ Current.Engine.thread engine; Current_web.run ~mode site ]

(* Parsing *)

open Cmdliner

let tls =
  let host =
    let doc = "Host of the remote albatross TLS endpoint" in
    Arg.(value & opt string "127.0.0.1" & info ~doc [ "alba-host" ])
  in
  let port =
    let doc = "Port of the remote albatross TLS endpoint" in
    Arg.(value & opt int 1025 & info ~doc [ "alba-port" ])
  in
  let ca =
    let doc = "CA certificate for the client" in
    Arg.(required & opt (some string) None & info ~doc [ "ca" ])
  in
  let ca_key =
    let doc = "Private key for the client" in
    Arg.(required & opt (some string) None & info ~doc [ "ca-key" ])
  in
  let server_ca =
    let doc = "CA certificate for the server" in
    Arg.(required & opt (some string) None & info ~doc [ "server-ca" ])
  in
  let pack host port ca ca_key server_ca =
    { host; port; ca; ca_key; server_ca }
  in
  Term.(const pack $ host $ port $ ca $ ca_key $ server_ca)

let cmd =
  let info = Cmd.info name in
  Cmd.v info
    Term.(
      term_result
        (const main $ Current.Config.cmdliner $ Current_web.cmdliner $ tls))

let () = exit @@ Cmd.eval cmd
