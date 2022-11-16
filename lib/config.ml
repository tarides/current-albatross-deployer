module Docker = Current_docker.Default

(* overrides *)
module Ipaddr = struct
  module V4 = struct
    include Ipaddr.V4

    let of_yojson = function
      | `String v -> of_string v |> Result.map_error (fun (`Msg m) -> m)
      | _ -> Error "type error"

    let to_yojson v = `String (to_string v)
  end
end

module Pre = struct
  type t = {
    service : string;
    unikernel : Unikernel.t;
    args : Ipaddr.V4.t -> string list;
    memory : int;
    network : string;
    cpu : int;
  }

  let value_digest { unikernel; args; memory; network; _ } =
    let args = args (Ipaddr.V4.of_string_exn "0.0.0.0") in
    Fmt.str "%s|%a|%d|%s"
      (Unikernel.digest unikernel)
      Fmt.(list ~sep:sp string)
      args memory network
    |> Digest.string |> Digest.to_hex

  let id t =
    let v = value_digest t in
    let v = String.sub v 0 (min 10 (String.length v)) in
    t.service ^ "." ^ v
end

type t = {
  service : string;
  unikernel : Unikernel.t;
  args : string list;
  id : string;
  ip : Ipaddr.V4.t;
  memory : int;
  network : string;
  cpu : int;
}
[@@deriving yojson]

let v (pre : Pre.t) ip =
  {
    service = pre.service;
    unikernel = pre.unikernel;
    args = pre.args ip;
    id = Pre.id pre;
    ip;
    memory = pre.memory;
    network = pre.network;
    cpu = pre.cpu;
  }
