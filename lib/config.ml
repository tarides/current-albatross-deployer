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
  }
end

type t = {
  service : string;
  unikernel : Unikernel.t;
  args : string list;
  ip : Ipaddr.V4.t;
  memory : int;
  network : string;
}
[@@deriving yojson]

let value_digest { unikernel; args; memory; network; ip; _ } =
  Fmt.str "%s|%a|%a|%d|%s|%a"
    (Docker.Image.digest unikernel.image)
    Fmt.(list ~sep:sp string)
    args Fpath.pp unikernel.location memory network Ipaddr.V4.pp ip
  |> Digest.string |> Digest.to_hex

let name t =
  let v = value_digest t in
  let v = String.sub v 0 (min 10 (String.length v)) in
  t.service ^ "." ^ v

let v (pre : Pre.t) ip =
  {
    service = pre.service;
    unikernel = pre.unikernel;
    args = pre.args ip;
    ip;
    memory = pre.memory;
    network = pre.network;
  }
