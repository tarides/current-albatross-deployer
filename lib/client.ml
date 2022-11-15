
open Lwt.Syntax

module IpManager = Iptables_client.IpManager
module Deployments = Iptables_client.Deployments

module Albatross = struct
  let connect name (cmd : Vmm_commands.t) =
    let sock, next = Vmm_commands.endpoint cmd in
    let sockaddr = Lwt_unix.ADDR_UNIX (Vmm_core.socket_path sock) in
    let* result = Vmm_lwt.connect Lwt_unix.PF_UNIX sockaddr in
    match result with
    | None ->
        Logs.err (fun m ->
            m "couldn't connect to %a" Vmm_lwt.pp_sockaddr sockaddr);
        Lwt.return (Error (`Msg "Albatross connection failed"))
    | Some fd -> (
        let header = Vmm_commands.header name in
        let* result = Vmm_lwt.write_wire fd (header, `Command cmd) in
        match result with
        | Error `Exception -> Lwt.return (Error (`Msg "Albatross write failed"))
        | Ok () -> Lwt.return (Ok (fd, next)))

  let show_unikernel name =
    let ( let** ) = Lwt_result.bind in
    let** fd, _ = connect name (`Unikernel_cmd `Unikernel_info) in
    Lwt.finalize
      (fun () ->
        let** _, response =
          Vmm_lwt.read_wire fd
          |> Lwt_result.map_err (fun _ -> `Msg "todo: map error")
        in
        match response with
        | `Success (`Unikernel_info v) -> Lwt.return_ok v
        | _ -> Lwt.return_error (`Msg "unexpected response"))
      (fun () -> Vmm_lwt.safe_close fd)

  let list_unikernels () = show_unikernel Vmm_core.Name.root

  let destroy_unikernel name =
    let ( let** ) = Lwt_result.bind in
    let** fd, next = connect name (`Unikernel_cmd `Unikernel_destroy) in
    assert (next = `End);
    let+ () = Vmm_lwt.safe_close fd in
    Ok ()

  let spawn_unikernel ~path ~name ~memory ~network ~args () =
    let ( let** ) = Lwt_result.bind in
    let* buffer = Lwt_io.open_file ~mode:Input path in
    let* content =
      Lwt.finalize
        (fun () -> Lwt_io.read buffer)
        (fun () -> Lwt_io.close buffer)
    in
    let image = Cstruct.of_string content in
    let** fd, next =
      connect name
        (`Unikernel_cmd
          (`Unikernel_create
            {
              Vmm_core.Unikernel.typ = `Solo5;
              compressed = false;
              image;
              fail_behaviour = `Quit;
              cpuid = 0;
              memory;
              block_devices = [];
              bridges = [ ("service", Some network, None) ];
              argv = Some args;
            }))
    in
    assert (next = `End);
    let+ () = Vmm_lwt.safe_close fd in
    Ok ()
end

let job = Lwt.return_unit

let connect = Iptables_client.connect

let close = Iptables_client.close

