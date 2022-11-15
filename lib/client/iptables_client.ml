open Lwt.Infix

let socket_path =
  "/var/run/current-iptables-daemon/current-iptables-daemon.sock"

module Wire = struct
  let safe_close fd =
    Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)

  let connect () =
    let sockaddr = Lwt_unix.ADDR_UNIX socket_path in
    let c = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
    Lwt_unix.set_close_on_exec c;
    Lwt.catch
      (fun () -> Lwt_unix.(connect c sockaddr) >|= fun () -> c)
      (fun e ->
        Logs.warn (fun m ->
            m "error %s connecting to socket %s" (Printexc.to_string e)
              socket_path);
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

type wire_error = [ `Eof | `Toomuch | `Exception | `Parse of string ]
type socket = Lwt_unix.file_descr

let query rpc ~socket x =
  let open Lwt.Syntax in
  let ( let** ) = Lwt_result.bind in
  let module Rpc = Iptables_daemon_api.Rpc in
  let tag = Rpc.Tag.v rpc in
  let inj, proj = Rpc.get_client rpc in
  let req = inj x |> Rpc.Tag.(add tag) in
  let* () = Wire.write ~socket req in
  let** response = Wire.read ~socket in
  let tag', response = Rpc.Tag.strip response in
  assert (tag = tag');
  Lwt.return (proj response |> Result.map_error (fun e -> (e :> wire_error)))

module IpManager = struct
  module Spec = Iptables_daemon_api.Spec

  let list = query Spec.IpManager.list
  let request = query Spec.IpManager.request
  let remove = query Spec.IpManager.free
end

module Deployments = struct
  module Spec = Iptables_daemon_api.Spec

  let list = query Spec.Deployments.list
  let create = query Spec.Deployments.create
  let remove = query Spec.Deployments.delete
end

let connect = Wire.connect
let close = Wire.safe_close
