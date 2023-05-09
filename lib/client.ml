open Lwt.Syntax
module IpManager = Iptables_client.IpManager
module Deployments = Iptables_client.Deployments

type next = [ `End | `Read ]

(** A way to send and receive commands to albatrossd *)
module type IO = sig
  type t

  val connect_and_write :
    Vmm_core.Name.t ->
    Vmm_commands.t ->
    (t * next, [> `Msg of string ]) result Lwt.t

  val read_wire :
    t -> (Vmm_commands.wire, [> `Eof | `Exception | `Toomuch ]) result Lwt.t

  val close : t -> (unit, [> `Msg of string ]) result Lwt.t
end

(** Talk to albatrossd via local socket *)
module Socket_io : IO = struct
  type t = Lwt_unix.file_descr

  let write_wire = Vmm_lwt.write_wire
  let read_wire = Vmm_lwt.read_wire
  let close t = Vmm_lwt.safe_close t |> Lwt_result.ok

  let connect_and_write name cmd =
    let header = Vmm_commands.header name in
    let wire = (header, `Command cmd) in
    let sock, next = Vmm_commands.endpoint cmd in
    let sockaddr = Lwt_unix.ADDR_UNIX (Vmm_core.socket_path sock) in
    let* result = Vmm_lwt.connect Lwt_unix.PF_UNIX sockaddr in
    match result with
    | None ->
        Logs.err (fun m ->
            m "couldn't connect to %a" Vmm_lwt.pp_sockaddr sockaddr);
        Lwt.return (Error (`Msg "Albatross connection failed"))
    | Some fd -> (
        let* result = write_wire fd wire in
        match result with
        | Error `Exception -> Lwt.return (Error (`Msg "Albatross write failed"))
        | Ok () -> Lwt.return (Ok (fd, next)))
end

type tls_config = {
  host : string;
  port : int;
  ca : string;
  ca_key : string;
  server_ca : string;
}
[@@deriving yojson]

module type TLS_CONFIG = sig
  val v : tls_config
end

(** Talk to albatrossd through remote TLS endpoint *)
module Tls_io (C : TLS_CONFIG) : IO = struct
  type t = Tls_lwt.Unix.t

  let ( let** ) = Lwt_result.bind

  let read_wire t =
    Lwt.catch
      (fun () -> Vmm_tls_lwt.read_tls t)
      (fun _ -> Lwt_result.fail `Exception)

  let close t =
    (* process last reply *)
    let* reply = read_wire t in
    let res =
      match reply with
      | Ok _ -> Ok ()
      | Error `Eof -> Ok ()
      | Error _ -> Error (`Msg "error when reading server reply")
    in
    let* () = Vmm_tls_lwt.close t in
    Lwt_result.lift res

  (* TODO This duplicates a lot of code from Albatross_bistro, would be nice to
     factorize *)

  let timestamps validity =
    let now = Ptime_clock.now () in
    match
      (* subtracting some seconds here to not require perfectly synchronised
         clocks on client and server *)
      ( Ptime.sub_span now (Ptime.Span.of_int_s 10),
        Ptime.add_span now (Ptime.Span.of_int_s validity) )
    with
    | None, _ | _, None -> invalid_arg "span too big - reached end of ptime"
    | Some now, Some exp -> (now, exp)

  let key_ids exts pub issuer =
    let open X509 in
    let auth = (Some (Public_key.id issuer), General_name.empty, None) in
    Extension.(
      add Subject_key_id
        (false, Public_key.id pub)
        (add Authority_key_id (false, auth) exts))

  let connect_and_write name cmd =
    let open X509 in
    let _, next = Vmm_commands.endpoint cmd in
    let* server_ca_cs = Vmm_lwt.read_from_file C.v.server_ca in
    let* ca_key_cs = Vmm_lwt.read_from_file C.v.ca_key in
    let** server_ca = Lwt_result.lift @@ Certificate.decode_pem server_ca_cs in
    let** ca_key = Lwt_result.lift @@ Private_key.decode_pem ca_key_cs in
    let tmpkey = X509.Private_key.generate `ED25519 in
    let extensions =
      let v = Vmm_asn.to_cert_extension cmd in
      Extension.(
        add Key_usage
          (true, [ `Digital_signature; `Key_encipherment ])
          (add Basic_constraints
             (true, (false, None))
             (add Ext_key_usage (true, [ `Client_auth ])
                (singleton (Unsupported Vmm_asn.oid) (false, v)))))
    in
    let** csr =
      let cn =
        Vmm_core.Name.name name
        |> Option.value ~default:(Vmm_core.Name.to_string name)
      in
      let name =
        [ Distinguished_name.(Relative_distinguished_name.singleton (CN cn)) ]
      in
      let extensions = Signing_request.Ext.(singleton Extensions extensions) in
      Lwt_result.lift (Signing_request.create name ~extensions tmpkey)
    in
    let valid_from, valid_until = timestamps 300 in
    let extensions =
      let capub = X509.Private_key.public ca_key in
      key_ids extensions Signing_request.((info csr).public_key) capub
    in
    let issuer = Certificate.subject server_ca in
    let** mycert =
      Signing_request.sign csr ~valid_from ~valid_until ~extensions ca_key
        issuer
      |> Result.map_error (fun e ->
             `Msg (Fmt.to_to_string X509.Validation.pp_signature_error e))
      |> Lwt_result.lift
    in
    let tls = C.v in
    let* authenticator = X509_lwt.authenticator (`Ca_file tls.ca) in
    let certificates = `Single ([ mycert; server_ca ], tmpkey) in
    let client = Tls.Config.client ~authenticator ~certificates () in
    let connect () = Tls_lwt.Unix.connect client (tls.host, tls.port) in
    let** t =
      Utils.catch_as_msg "exception on connection to TLS endpoint" (connect ())
    in
    Lwt_result.return (t, next)
end

module Make_albatross (E : IO) = struct
  let io_error_to_string = function
    | `Eof -> "end of file"
    | `Exception -> "unknown exception"
    | `Toomuch -> "too much"

  let show_unikernel name =
    let ( let** ) = Lwt_result.bind in
    let** fd, _ = E.connect_and_write name (`Unikernel_cmd `Unikernel_info) in
    Lwt.finalize
      (fun () ->
        let** _, response =
          E.read_wire fd
          |> Lwt_result.map_error (fun e ->
                 `Msg ("show_unikernel: read_wire: " ^ io_error_to_string e))
        in
        match response with
        | `Success (`Unikernel_info v) -> Lwt.return_ok v
        | _ -> Lwt.return_error (`Msg "unexpected response"))
      (fun () ->
        let* res = E.close fd in
        Lwt.return (Result.get_ok res))

  let list_unikernels () = show_unikernel Vmm_core.Name.root

  let destroy_unikernel name =
    let ( let** ) = Lwt_result.bind in
    let** fd, next =
      E.connect_and_write name (`Unikernel_cmd `Unikernel_destroy)
    in
    assert (next = `End);
    E.close fd

  let spawn_unikernel ~path ~name ~memory ~network ~cpu ~args () =
    let ( let** ) = Lwt_result.bind in
    let* buffer = Lwt_io.open_file ~mode:Input path in
    let* content =
      Lwt.finalize
        (fun () -> Lwt_io.read buffer)
        (fun () -> Lwt_io.close buffer)
    in
    let image = Cstruct.of_string content in
    let bridges =
      match network with Some v -> [ ("service", Some v, None) ] | None -> []
    in
    let** fd, next =
      E.connect_and_write name
        (`Unikernel_cmd
          (`Unikernel_create
            {
              Vmm_core.Unikernel.typ = `Solo5;
              compressed = false;
              image;
              fail_behaviour = `Quit;
              cpuid = cpu;
              memory;
              block_devices = [];
              bridges;
              argv = Some args;
            }))
    in
    assert (next = `End);
    E.close fd
end

module Albatross_socket = Make_albatross (Socket_io)
module Albatross_tls (C : TLS_CONFIG) = Make_albatross (Tls_io (C))

module type S = module type of Albatross_socket

let client_of_mode = function
  | `Socket -> (module Albatross_socket : S)
  | `Tls tls ->
      (module Albatross_tls (struct
        let v = tls
      end))

let job = Lwt.return_unit
let connect = Iptables_client.connect
let close = Iptables_client.close
