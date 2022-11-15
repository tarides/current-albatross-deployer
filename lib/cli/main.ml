open Lwt.Syntax

let run_with_socket fn =
  let program =
    let* socket = Iptables_client.connect () in
    Lwt.finalize (fun () -> fn socket) (fun () -> Iptables_client.close socket)
  in
  Lwt_main.run program

open Cmdliner

let print_result pp = function
  | Error `Eof -> Fmt.pr "Unexpected end of file"
  | Error `Toomuch -> Fmt.pr "Got too many bytes in the answer"
  | Error `Exception -> Fmt.pr "Somewhere, an exception"
  | Error (`Parse v) -> Fmt.pr "Answer parse error: %s" v
  | Ok v -> Fmt.pr "%a" pp v

let ip_list =
  Term.(
    const (fun () ->
        run_with_socket (fun socket ->
            Iptables_client.IpManager.list ~socket ())
        |> print_result Fmt.(list Iptables_daemon_api.Types.Ip.pp))
    $ const ())

let ipv4_prefix = Arg.conv (Ipaddr.V4.Prefix.of_string, Ipaddr.V4.Prefix.pp)
let ipv4 = Arg.conv (Ipaddr.V4.of_string, Ipaddr.V4.pp)
let tag = Arg.(required @@ opt (some string) None @@ info [ "tag" ])
let prefix = Arg.(required @@ opt (some ipv4_prefix) None @@ info [ "prefix" ])
let blacklist = Arg.(value @@ opt_all ipv4 [] @@ info [ "except" ])

let ip_query =
  Term.(
    const (fun tag prefix blacklist ->
        run_with_socket (fun socket ->
            Iptables_client.IpManager.request ~socket (tag, prefix, blacklist))
        |> print_result (fun f -> function
             | Error `Full -> Fmt.pf f "No ip available"
             | Ok v -> Iptables_daemon_api.Types.Ip.pp f v))
    $ tag $ prefix $ blacklist)

let ip_free =
  Term.(
    const (fun tag ->
        run_with_socket (fun socket ->
            Iptables_client.IpManager.remove ~socket tag)
        |> print_result (fun f -> function
             | Error `Not_found -> Fmt.pf f "Tag not found"
             | Ok v -> Iptables_daemon_api.Types.Ip.pp f v))
    $ tag)

let deployment_list =
  Term.(
    const (fun () ->
        run_with_socket (fun socket ->
            Iptables_client.Deployments.list ~socket ())
        |> print_result Fmt.(list Iptables_daemon_api.Types.DeploymentInfo.pp))
    $ const ())

let service = Arg.(required @@ opt (some string) None @@ info [ "service" ])
let ip = Arg.(required @@ opt (some ipv4) None @@ info [ "ip" ])

let port_redirection =
  Arg.conv ~docv:"SOURCE:TARGET"
    ( (fun str ->
        match String.split_on_char ':' str with
        | [ source; target ] -> (
            try
              let source = int_of_string source in
              let target = int_of_string target in
              Ok { Iptables_daemon_api.Types.PortRedirection.source; target }
            with Failure _ -> Error (`Msg "Could not parse one of the ports"))
        | _ -> Error (`Msg "Not a pair of values")),
      Iptables_daemon_api.Types.PortRedirection.pp )

let ports = Arg.(value @@ opt_all port_redirection [] @@ info [ "p" ])

let deployment_add =
  Term.(
    const (fun ip name ports ->
        (* TODO: a bit flaky here*)
        let ip = { Iptables_daemon_api.Types.Ip.ip; tag = name } in
        let info =
          { Iptables_daemon_api.Types.DeploymentInfo.ip; name; ports }
        in
        run_with_socket (fun socket ->
            Iptables_client.Deployments.create ~socket info)
        |> print_result (fun f -> function
             | Error (`Port_already_allocated n) ->
                 Fmt.pf f "Port %d is already allocated" n
             | Ok () -> Fmt.pf f "OK"))
    $ ip $ service $ ports)

let deployment_remove =
  Term.(
    const (fun service ->
        run_with_socket (fun socket ->
            Iptables_client.Deployments.remove ~socket service)
        |> print_result (fun f -> function
             | Error `Not_found -> Fmt.pf f "Service not found"
             | Ok v -> Iptables_daemon_api.Types.DeploymentInfo.pp f v))
    $ service)

let () =
  Term.(
    exit
    @@ eval_choice
         Term.(const (), Term.info "iptables-cli")
         [
           (ip_list, Term.info ~doc:"list allocated IPs" "ip-list");
           ( ip_query,
             Term.info ~doc:"query a new IP associated with tag" "ip-query" );
           (ip_free, Term.info ~doc:"free IP" "ip-free");
           (deployment_list, Term.info ~doc:"list deployments" "deployment-list");
           ( deployment_add,
             Term.info
               ~doc:
                 "add new deployment and register port redirection. Can also \
                  be used to update deployment."
               "deployment-add" );
           ( deployment_remove,
             Term.info ~doc:"remove deployment" "deployment-remove" );
         ])
