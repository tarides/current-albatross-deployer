open Lwt.Syntax
module Published = Publish.Published

let ( let** ) = Lwt_result.bind

module OpCollect (C : Client.S) = struct
  type t = No_context

  let id = "collect"
  let pp f _ = Fmt.string f "collect"
  let auto_cancel = false

  module Key = struct
    type t = Published.t list

    let digest t =
      t
      |> List.sort (fun a b -> String.compare a.Published.service b.service)
      |> List.map (fun d ->
             d.Published.service ^ ":"
             ^ (d.info |> Albatross_deploy.Deployed.digest))
      |> String.concat "|"
  end

  module Value = Current.Unit

  let build No_context job roots =
    let* () = Current.Job.start ~level:Average job in

    let module StringSet = Set.Make (String) in
    let deployed_services =
      StringSet.of_list (List.map (fun t -> t.Published.service) roots)
    in
    let perform ~socket =
      (* Step 1: daemon: remove unused deployments *)
      let** deployments =
        Client.Deployments.list ~socket () |> Lwt.map Utils.remap_errors
      in
      let deployments_to_keep, deployments_to_remove =
        List.partition
          (fun (deployment : Iptables_daemon_api.Types.DeploymentInfo.t) ->
            StringSet.mem deployment.name deployed_services)
          deployments
      in
      Current.Job.log job "Roots:";
      List.iter
        (fun (deployment : Iptables_daemon_api.Types.DeploymentInfo.t) ->
          Current.Job.log job " - %s @%a (%s)" deployment.name Ipaddr.V4.pp
            deployment.ip.ip deployment.ip.tag)
        deployments_to_keep;

      Current.Job.log job "Stage 1: remove unused deployments";
      let* () =
        Lwt_list.iter_s
          (fun (deployment : Iptables_daemon_api.Types.DeploymentInfo.t) ->
            Current.Job.log job "- %s" deployment.name;
            Client.Deployments.remove ~socket deployment.name |> Lwt.map ignore)
          deployments_to_remove
      in
      let ips_to_keep =
        List.map
          (fun (d : Iptables_daemon_api.Types.DeploymentInfo.t) ->
            Ipaddr.V4.to_string d.ip.ip)
          deployments_to_keep
        |> StringSet.of_list
      in

      (* Step 1.5: collect IPs and unikernel tags *)
      let** ips =
        Client.IpManager.list ~socket () |> Lwt.map Utils.remap_errors
      in
      let ips_to_remove =
        List.filter
          (fun (ip : Iptables_daemon_api.Types.Ip.t) ->
            not (StringSet.mem (Ipaddr.V4.to_string ip.ip) ips_to_keep))
          ips
      in
      let removed_ips_tags =
        List.map (fun ip -> ip.Iptables_daemon_api.Types.Ip.tag) ips_to_remove
        |> StringSet.of_list
      in
      (* Step 2: albatross: remove unikernels starting by a deployment's service
         name *)
      Current.Job.log job "Stage 2: remove unused unikernels";
      Current.Job.log job "Live unikernels:";
      let ( let** ) = Lwt_result.bind in
      let** unikernels = C.list_unikernels () in
      let unikernels_to_remove =
        List.filter
          (fun (name, _) ->
            let tag = Vmm_core.Name.to_string name in
            let tag = String.sub tag 1 (String.length tag - 1) in
            Current.Job.log job "- %s" tag;
            StringSet.mem tag removed_ips_tags)
          unikernels
      in
      Current.Job.log job "Remove them:";
      let* () =
        Lwt_list.iter_s
          (fun (name, _) ->
            Current.Job.log job "- %a" Vmm_core.Name.pp name;
            C.destroy_unikernel name |> Lwt.map ignore)
          unikernels_to_remove
      in
      (* Step 3: daemon: remove unused IPs *)
      Current.Job.log job "Stage 3: remove unused IPs";
      let+ () =
        Lwt_list.iter_s
          (fun (ip : Iptables_daemon_api.Types.Ip.t) ->
            Current.Job.log job "- %s @%a" ip.tag Ipaddr.V4.pp ip.ip;
            Client.IpManager.remove ~socket ip.tag |> Lwt.map ignore)
          ips_to_remove
      in
      Ok ()
    in

    let* socket = Client.connect () in
    Lwt.finalize (fun () -> perform ~socket) (fun () -> Client.close socket)
end

let collect ?(mode = `Socket) deployments =
  let module C = (val Client.client_of_mode mode) in
  let module Collect = Current_cache.Make (OpCollect (C)) in
  let open Current.Syntax in
  Current.component "collect"
  |> let> deployments = deployments in
     Collect.get No_context deployments
