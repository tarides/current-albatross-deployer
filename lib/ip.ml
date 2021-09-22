open Lwt.Syntax

let (let**) = Lwt_result.bind

module IpOp = struct
  type t = No_context

  let id = "get-ip"

  let auto_cancel = true

  module Key = struct
    type t = string * Ipaddr.V4.Prefix.t * Ipaddr.V4.t list

    let digest (key, prefix, blacklist) =
      (List.sort Ipaddr.V4.compare blacklist
      |> List.map Ipaddr.V4.to_string
      |> String.concat "|")
      ^ "|" ^ key ^ "|"
      ^ Ipaddr.V4.Prefix.to_string prefix
  end

  module Value = struct
    type t = Ipaddr.V4.t

    let marshal = Ipaddr.V4.to_string

    let unmarshal = Ipaddr.V4.of_string_exn
  end

  let build No_context job (key, prefix, blacklist) =
    let* () = Current.Job.start ~level:Mostly_harmless job in

    let* socket = Client.connect () in
    let** ip =
      Lwt.finalize
        (fun () ->
          Client.IpManager.request ~socket (key, prefix, blacklist)
          |> Lwt.map Utils.remap_errors)
        (fun () -> Client.close socket)
    in
    let** ip = Lwt.return (Utils.remap_errors ip) in
    Current.Job.log job "Got IP: for %s: %a" key Ipaddr.V4.pp ip.ip;
    Lwt.return_ok ip.ip

  let pp f (key, prefix, _) =
    Fmt.pf f "Get IP for %S (prefix: %a)" key Ipaddr.V4.Prefix.pp prefix
end

module IpCache = Current_cache.Make (IpOp)

let get_ip ?(blacklist = []) ~prefix config =
  let open Current.Syntax in
  Current.component "get IP"
  |> let> config = config in
     let key = Config.Pre.id config in
     IpCache.get No_context (key, prefix, blacklist)
