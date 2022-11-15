let ( let** ) = Lwt_result.bind

module Port = struct
  type t = { source : int; target : int }

  let pp f { source; target } = Fmt.pf f "%d->%d" source target
end

module Published = struct
  type t = { service : string }

  let marshal t = t.service
  let unmarshal service = { service }
end

module OpPublish = struct
  type t = No_context

  let id = "mirage-publish"

  module Key = struct
    type t = { service : string }

    let digest t = t.service
  end

  module Value = struct
    type t = { ports : Port.t list; ip : Ipaddr.V4.t }

    let digest { ports; ip } =
      Fmt.str "%a|%a" Ipaddr.V4.pp ip Fmt.(list ~sep:sp Port.pp) ports
      |> Digest.string |> Digest.to_hex
  end

  module Outcome = Published

  let publish No_context job { Key.service } { Value.ports; ip } =
    let open Lwt.Syntax in
    let* () = Current.Job.start job ~level:Mostly_harmless in
    Current.Job.log job
      "Register the service %s to ip %a and enable port forwarding" service
      Ipaddr.V4.pp ip;
    (* Set up port forwarning *)
    let ports =
      List.map
        (function
          | { Port.source; target } ->
              { Iptables_daemon_api.Types.PortRedirection.source; target })
        ports
    in
    let* socket = Client.connect () in
    let** result =
      Lwt.finalize
        (fun () ->
          Client.Deployments.create ~socket
            {
              (* todo: a bit flaky here *)
              Iptables_daemon_api.Types.DeploymentInfo.ip =
                { tag = service; ip };
              ports;
              name = service;
            }
          |> Lwt.map Utils.remap_errors)
        (fun () -> Client.close socket)
    in
    let** () = Lwt.return (result |> Utils.remap_errors) in

    Lwt_result.return { Published.service }

  let pp f (key, _v) = Fmt.pf f "@[<v2>deploy %s@]" key.Key.service
  let auto_cancel = true
end

module Publish = Current_cache.Output (OpPublish)

let publish ~service ?(ports = []) info =
  let open Current.Syntax in
  Current.component "Publish %s\n%a" service Fmt.(list Port.pp) ports
  |> let> info = info in
     Publish.set No_context { service }
       { ports; ip = info.Albatross_deploy.Deployed.config.ip }
