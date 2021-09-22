module Rpc = struct
  type ('a, 'b) t = { tag : string; query : 'a Asn.t; resp : 'b Asn.t }

  type untagged_buffer = Cstruct.t

  module Tag = struct
    type ('a, 'b) rpc = ('a, 'b) t

    type t = string

    module Map = Map.Make (String)

    let header tag =
      let tag_buffer = Cstruct.of_string tag in
      let tag_length_buffer = Cstruct.create 4 in
      let tag_length = Cstruct.length tag_buffer in
      Cstruct.BE.set_uint32 tag_length_buffer 0 (Int32.of_int tag_length);
      Cstruct.append tag_length_buffer tag_buffer

    let v rpc = rpc.tag

    let strip buffer =
      let tag_length = Cstruct.BE.get_uint32 buffer 0 |> Int32.to_int in
      let tag = Cstruct.to_string ~off:4 ~len:tag_length buffer in
      let header_length = 4 + tag_length in
      ( tag,
        Cstruct.sub buffer header_length (Cstruct.length buffer - header_length)
      )

    let add tag value =
      let header = header tag in
      Cstruct.append header value
  end

  type error = [ `Parse of string ]

  let get_client rpc =
    let query_rpc = Asn.(codec der) rpc.query in
    let response_rpc = Asn.(codec der) rpc.resp in
    ( (fun value -> Asn.encode query_rpc value),
      fun response ->
        let ( let* ) = Result.bind in
        (* what to do with the trailing bytes?*)
        let* value, _ =
          Asn.decode response_rpc response
          |> Result.map_error (fun e -> (e :> error))
        in
        Ok value )

  let get_server rpc =
    let query_rpc = Asn.(codec der) rpc.query in
    let response_rpc = Asn.(codec der) rpc.resp in
    ( (fun request ->
        let ( let* ) = Result.bind in
        (* what to do with the trailing bytes?*)
        let* value, _ =
          Asn.decode query_rpc request
          |> Result.map_error (fun e -> (e :> error))
        in
        Ok value),
      fun value -> Asn.encode response_rpc value )
end

module Types = struct
  module PortRedirection = struct
    type t = { source : int; target : int }

    let pp f { source; target } = Fmt.pf f "%d -> %d" source target
  end

  module Ip = struct
    type t = { ip : Ipaddr.V4.t; tag : string }

    let pp f { ip; tag } = Fmt.pf f "%s: %a" tag Ipaddr.V4.pp ip
  end

  module DeploymentInfo = struct
    type t = { ip : Ip.t; ports : PortRedirection.t list; name : string }

    let pp f { ip; ports; name } =
      Fmt.pf f "%s@%a@[<v 2>@ %a@]" name Ipaddr.V4.pp ip.ip
        (Fmt.list PortRedirection.pp)
        ports
  end
end

module Asn_values = struct
  let port_redirection =
    let f (source, target) = { Types.PortRedirection.source; target } in
    let g { Types.PortRedirection.source; target } = (source, target) in
    Asn.S.map f g
    @@ Asn.S.(sequence2 (required ~label:"from" int) (required ~label:"to" int))

  let ip =
    Asn.S.map Ipaddr.V4.of_string_exn Ipaddr.V4.to_string Asn.S.printable_string

  let ip_prefix =
    Asn.S.map Ipaddr.V4.Prefix.of_string_exn Ipaddr.V4.Prefix.to_string
      Asn.S.printable_string

  let ip_tag =
    let f (ip, tag) = { Types.Ip.ip; tag } in
    let g { Types.Ip.ip; tag } = (ip, tag) in
    Asn.S.map f g
    @@ Asn.S.(
         sequence2 (required ~label:"ip" ip)
           (required ~label:"tag" printable_string))

  let deployment_info =
    let f (ip, ports, name) = { Types.DeploymentInfo.ip; ports; name } in
    let g { Types.DeploymentInfo.ip; ports; name } = (ip, ports, name) in
    Asn.S.map f g
    @@ Asn.S.(
         sequence3
           (required ~label:"ip" ip_tag)
           (required ~label:"ports" (sequence_of port_redirection))
           (required ~label:"name" printable_string))

  module Error = struct
    let not_found =
      Asn.S.null
      |> Asn.S.map (function () -> `Not_found) (function `Not_found -> ())

    let full =
      Asn.S.null |> Asn.S.map (function () -> `Full) (function `Full -> ())

    let port_already_allocated =
      Asn.S.int
      |> Asn.S.map
           (function v -> `Port_already_allocated v)
           (function `Port_already_allocated v -> v)
  end
end

module Spec = struct
  let result a b =
    Asn.S.choice2 a b
    |> Asn.S.map
         (function `C1 a -> Ok a | `C2 a -> Error a)
         (function Ok a -> `C1 a | Error a -> `C2 a)

  module IpManager = struct
    let list =
      {
        Rpc.tag = "ipmanager.list";
        query = Asn.S.null;
        resp = Asn.S.sequence_of Asn_values.ip_tag;
      }

    let request =
      {
        Rpc.tag = "ipmanager.request";
        query =
          Asn.S.(
            sequence3
              (required printable_string)
              (required Asn_values.ip_prefix)
              (required (sequence_of Asn_values.ip)));
        resp = result Asn_values.ip_tag Asn_values.Error.full;
      }

    let free =
      {
        Rpc.tag = "ipmanager.free";
        query = Asn.S.printable_string;
        resp = result Asn_values.ip_tag Asn_values.Error.not_found;
      }
  end

  module Deployments = struct
    let list =
      {
        Rpc.tag = "deployments.list";
        query = Asn.S.null;
        resp = Asn.S.sequence_of Asn_values.deployment_info;
      }

    let create =
      {
        Rpc.tag = "deployments.create";
        query = Asn_values.deployment_info;
        resp = result Asn.S.null Asn_values.Error.port_already_allocated;
      }

    let delete =
      {
        Rpc.tag = "deployments.delete";
        query = Asn.S.printable_string;
        resp = result Asn_values.deployment_info Asn_values.Error.not_found;
      }
  end
end
