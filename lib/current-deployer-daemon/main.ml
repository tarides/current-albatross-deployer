module Config = struct
  type t = { network : Ipaddr.V4.Prefix.t }
end

module Types = Current_deployer_api.Types

module Ipmap = struct
  module Map = Map.Make (Ipaddr.V4)
  module Set = Set.Make (Ipaddr.V4)

  type 'a t = { content : 'a Map.t }

  let mem ip { content; _ } = Map.mem ip content

  let obtain ~blacklist ~prefix t name =
    (* assumption: name don't exist *)
    (* first IP is host: maybe don't hardcode that ? *)
    let blacklist = Set.of_list blacklist in
    let start = Ipaddr.V4.Prefix.first prefix in
    let stop = Ipaddr.V4.Prefix.last prefix in
    let rec test ip =
      match Map.mem ip t.content || Set.mem ip blacklist with
      | false -> Some ({ content = Map.add ip name t.content }, ip)
      | true when ip <> stop -> test (Ipaddr.V4.succ ip |> Result.get_ok)
      | true -> None
    in
    test start

  let remove t name =
    let content, removed_ip =
      Map.fold
        (fun ip name' (new_map, removed_ip) ->
          if name' <> name then (Map.add ip name' new_map, removed_ip)
          else (new_map, Some ip))
        t.content (Map.empty, None)
    in
    ({ content }, removed_ip)

  let bindings t =
    Map.bindings t.content |> List.map (fun (ip, tag) -> { Types.Ip.ip; tag })

  let make () = { content = Map.empty }
end

module State = struct
  type t = {
    mutable ips : string Ipmap.t;
    mutable deployments :
      (Iptables.Nat.Rule.handle list * Types.DeploymentInfo.t) list;
    mutable iptables : Iptables.Nat.t;
  }

  let v () =
    {
      ips = Ipmap.make ();
      deployments = [];
      iptables = Iptables.Nat.init ~name:"CURRENT-DEPLOYER" [];
    }

  let list_ips t = Ipmap.bindings t.ips

  let remove_ip t name =
    let ips, removed_element = Ipmap.remove t.ips name in
    t.ips <- ips;
    match removed_element with
    | None -> Error `Not_found
    | Some ip -> Ok { Types.Ip.ip; tag = name }

  let obtain_ip ~blacklist ~prefix t name =
    let existing_ip =
      Ipmap.bindings t.ips
      |> List.find_map (fun (ip : Types.Ip.t) ->
             if ip.tag = name then Some ip else None)
    in
    let obtain () =
      match Ipmap.obtain ~blacklist ~prefix t.ips name with
      | None -> Error `Full
      | Some (ips, new_ip) ->
          t.ips <- ips;
          Ok { Types.Ip.ip = new_ip; tag = name }
    in
    match existing_ip with
    | Some ip when not (List.exists (fun ip' -> ip.ip = ip') blacklist) -> Ok ip
    | Some _ ->
        let _ = remove_ip t name |> Result.get_ok in
        obtain ()
    | None -> obtain ()

  let list_deployments t = t.deployments |> List.map snd

  let remove_deployment t name =
    let remaining_deployments, removed_deployment =
      List.fold_left
        (fun (remaining_deployments, removed_deployment) ((_, info) as entry) ->
          if info.Types.DeploymentInfo.name = name then
            (remaining_deployments, Some entry)
          else (entry :: remaining_deployments, removed_deployment))
        ([], None) t.deployments
    in
    t.deployments <- remaining_deployments;
    match removed_deployment with
    | Some (removed_rules, removed_deployment) ->
        List.iter (Iptables.Nat.remove_rule t.iptables) removed_rules;
        Ok removed_deployment
    | None -> Error `Not_found

  let add_new_deployment t (deployment : Types.DeploymentInfo.t) =
    let existing_deployment =
      List.exists
        (fun (_, d) -> d.Types.DeploymentInfo.name = deployment.name)
        t.deployments
    in
    if existing_deployment then remove_deployment t deployment.name |> ignore;
    let module PortSet = Set.Make (Int) in
    let ports =
      List.map
        (fun ((_, deployment) : _ * Types.DeploymentInfo.t) ->
          List.map
            (function { Types.PortRedirection.source; _ } -> source)
            deployment.ports)
        t.deployments
      |> List.concat |> PortSet.of_list
    in
    let exception Port_conflict of int in
    try
      let new_rules =
        List.map
          (fun { Types.PortRedirection.source; target } ->
            if PortSet.mem source ports then raise (Port_conflict source);
            let module Nat = Iptables.Nat in
            let match_tcp =
              Nat.Match.(
                protocol
                  (`Tcp (Tcp.v ~dport:(Nat.Negatable.V [ Port source ]) ())))
            in
            let match_udp =
              Nat.Match.(
                protocol
                  (`Udp (Udp.v ~dport:(Nat.Negatable.V [ Port source ]) ())))
            in
            let action =
              Nat.Action.destination_nat ~port:(Nat.Port target)
                deployment.ip.ip
            in
            [ Nat.Rule.v [ match_tcp ] action; Nat.Rule.v [ match_udp ] action ])
          deployment.ports
        |> List.concat
      in
      let handles = Iptables.Nat.add_rules t.iptables new_rules in
      t.deployments <- (handles, deployment) :: t.deployments;
      Ok ()
    with Port_conflict v -> Error (`Port_already_allocated v)
end

let safe_close fd =
  Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)

module Wire = struct
  let respond ~socket data =
    let open Lwt.Infix in
    let write_raw buf =
      let rec w off l =
        Lwt.catch
          (fun () ->
            Lwt_unix.send socket buf off l [] >>= fun n ->
            if n = l then Lwt.return (Ok ()) else w (off + n) (l - n))
          (fun e ->
            Logs.err (fun m ->
                m "exception %s while writing" (Printexc.to_string e));
            safe_close socket >|= fun () -> Error `Exception)
      in
      w 0 (Bytes.length buf)
    in
    let dlen = Cstruct.create 4 in
    Cstruct.BE.set_uint32 dlen 0 (Int32.of_int (Cstruct.length data));
    let bytes = Cstruct.(to_bytes (append dlen data)) in
    write_raw bytes

  let read s =
    let open Lwt.Infix in
    let buf = Bytes.create 4 in
    let rec r b i l =
      Lwt.catch
        (fun () ->
          Lwt_unix.read s b i l >>= function
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
          safe_close s >|= fun () -> Error `Exception)
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

  let create_socket =
    let open Lwt.Infix in
    let name = "/var/run/current-deployer/current-deployerd.sock" in
    (Lwt_unix.file_exists name >>= function
     | true -> Lwt_unix.unlink name
     | false -> Lwt.return_unit)
    >>= fun () ->
    let s = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
    Lwt_unix.set_close_on_exec s;
    let old_umask = Unix.umask 0 in
    Lwt_unix.(bind s (ADDR_UNIX name)) >|= fun () ->
    Logs.app (fun m -> m "listening on %s" name);
    let _ = Unix.umask old_umask in
    Lwt_unix.listen s 1;
    s
end

module Handler = struct
  module Rpc = Current_deployer_api.Rpc

  type t = {
    tag : Rpc.Tag.t;
    handle :
      Rpc.untagged_buffer ->
      (Rpc.untagged_buffer, Current_deployer_api.Rpc.error) result;
  }

  let implement rpc f =
    let inj, proj = Rpc.get_server rpc in
    {
      tag = Rpc.Tag.v rpc;
      handle =
        (fun request ->
          let ( let* ) = Result.bind in
          let* request = inj request in
          let response = f request in
          Ok (proj response));
    }

  let handle handlers tagged_query =
    let tag, query = Rpc.Tag.strip tagged_query in
    let handler = Rpc.Tag.Map.find tag handlers in
    handler.handle query |> Result.map (Rpc.Tag.add tag)
end

let handle ~handlers socket =
  let open Lwt.Syntax in
  Logs.app (fun f -> f "New client!");
  let rec loop () =
    let* v = Wire.read socket in
    match v with
    | Ok command ->
        Logs.info (fun f -> f "Request !");
        let* _ =
          match Handler.handle handlers command with
          | Ok response -> Wire.respond ~socket response
          | Error _ ->
              Logs.app (fun f -> f "Error while executing command");
              Lwt.return_error (`Msg "TODO")
          | exception _ ->
              Logs.app (fun f -> f "Got exception while executing command. ");
              Lwt.return_error (`Msg "TODO")
        in
        loop ()
    | Error _ ->
        Logs.err (fun m -> m "error while reading");
        Lwt.return_unit
  in
  let+ () = loop () in
  Logs.app (fun f -> f "Client left.")

let handlers ~state =
  let module Spec = Current_deployer_api.Spec in
  Handler.
    [ 
      (* IPs *)
      implement Spec.IpManager.list (fun () -> State.list_ips state);
      implement Spec.IpManager.request (fun (tag, prefix, blacklist) ->
          State.obtain_ip ~blacklist ~prefix state tag);
      implement Spec.IpManager.free (fun tag -> State.remove_ip state tag);
      (* Port publishing *)
      implement Spec.Deployments.list (fun () -> State.list_deployments state);
      implement Spec.Deployments.create (fun dpl ->
          State.add_new_deployment state dpl);
      implement Spec.Deployments.delete (fun service ->
          State.remove_deployment state service);
    ]
  |> List.to_seq
  |> Seq.map (fun ({ Handler.tag; _ } as v) -> (tag, v))
  |> Handler.Rpc.Tag.Map.of_seq

let job =
  let state = State.v () in
  let handlers = handlers ~state in
  let open Lwt.Syntax in
  let* socket = Wire.create_socket in
  let rec loop () =
    let* client_socket, _ = Lwt_unix.accept socket in
    Lwt.async (fun () -> handle ~handlers client_socket);
    loop ()
  in
  loop ()

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Lwt_main.run job
