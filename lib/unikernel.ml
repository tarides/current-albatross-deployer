module Fpath = struct
  include Fpath

  let of_yojson = function
    | `String v -> Fpath.of_string v |> Result.map_error (fun (`Msg s) -> s)
    | _ -> Error "type error"

  let to_yojson v = `String (Fpath.to_string v)
end

module Docker = struct
  module Image = struct
    include Current_docker.Default.Image

    let to_yojson v = `String (hash v)

    let of_yojson = function
      | `String v -> Ok (of_hash v)
      | _ -> Error "type error"
  end
end

type t = { image : Docker.Image.t; location : Fpath.t } [@@deriving yojson]

let of_docker ~image ~location = { image; location }
