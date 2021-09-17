open Lwt.Infix

module Wire = struct
  let safe_close fd =
    Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)

  let connect () =
    let sockaddr = Lwt_unix.ADDR_UNIX "/var/run/current-deployer/current-deployerd.sock" in
    let c = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
    Lwt_unix.set_close_on_exec c;
    Lwt.catch
      (fun () -> Lwt_unix.(connect c sockaddr) >|= fun () -> c)
      (fun e ->
        Logs.warn (fun m ->
            m "error %s connecting to socket /var/run/current-deployer/current-deployerd.sock"
              (Printexc.to_string e));
        safe_close c >|= fun () -> raise e)

  let write ~socket data =
    let open Lwt.Infix in
    let write_raw buf =
      let rec w off l =
        Lwt.catch
          (fun () ->
            Lwt_unix.send socket buf off l [] >>= fun n ->
            if n = l then Lwt.return () else w (off + n) (l - n))
          (fun e ->
            Logs.err (fun m ->
                m "exception %s while writing" (Printexc.to_string e));
            safe_close socket >|= fun () -> raise e)
      in
      w 0 (Bytes.length buf)
    in
    let dlen = Cstruct.create 4 in
    Cstruct.BE.set_uint32 dlen 0 (Int32.of_int (Cstruct.length data));
    let bytes = Cstruct.(to_bytes (append dlen data)) in
    write_raw bytes

  let read ~socket =
    let open Lwt.Infix in
    let buf = Bytes.create 4 in
    let rec r b i l =
      Lwt.catch
        (fun () ->
          Lwt_unix.read socket b i l >>= function
          | 0 ->
              Logs.debug (fun m -> m "end of file while reading");
              Lwt.return (Error `Eof)
          | n when n == l -> Lwt.return (Ok ())
          | n when n < l -> r b (i + n) (l - n)
          | _ ->
              Logs.err (fun m -> m "read too much, shouldn't happen)");
              Lwt.return (Error `Toomuch))
        (fun e ->
          let err = Printexc.to_string e in
          Logs.err (fun m -> m "exception %s while reading" err);
          safe_close socket >|= fun () -> Error `Exception)
    in
    r buf 0 4 >>= function
    | Error e -> Lwt.return (Error e)
    | Ok () ->
        let len = Cstruct.BE.get_uint32 (Cstruct.of_bytes buf) 0 in
        if len > 0l then
          let b = Bytes.create (Int32.to_int len) in
          r b 0 (Int32.to_int len) >|= function
          | Error e -> Error e
          | Ok () -> Ok (Cstruct.of_bytes b)
        else Lwt.return (Error `Eof)
end

let query rpc ~socket x =
  let open Lwt.Syntax in
  let ( let** ) = Lwt_result.bind in
  let module Rpc = Current_deployer_api.Rpc in
  let tag = Rpc.Tag.v rpc in
  let inj, proj = Rpc.get_client rpc in
  let req = inj x |> Rpc.Tag.(add tag) in
  let* () = Wire.write ~socket req in
  let** response = Wire.read ~socket in
  let tag', response = Rpc.Tag.strip response in
  assert (tag = tag');
  Lwt.return
    (proj response
    |> Result.map_error (fun e ->
           (e :> [ `Eof | `Toomuch | `Exception | `Parse of string ])))

open Lwt.Syntax

module IpManager = struct
  module Spec = Current_deployer_api.Spec

  let list = query Spec.IpManager.list

  let request = query Spec.IpManager.request

  let remove = query Spec.IpManager.free
end

module Deployments = struct
  module Spec = Current_deployer_api.Spec

  let list = query Spec.Deployments.list

  let create = query Spec.Deployments.create

  let remove = query Spec.Deployments.delete
end

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
              bridges = [ ("service", Some network) ];
              argv = Some args;
            }))
    in
    assert (next = `End);
    let+ () = Vmm_lwt.safe_close fd in
    Ok ()
end

let job = Lwt.return_unit

let v = Wire.connect