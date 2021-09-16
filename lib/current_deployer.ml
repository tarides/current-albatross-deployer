module Docker = Current_docker.Default
module Raw = Current_docker.Raw
open Lwt.Syntax

let ( let** ) = Lwt_result.bind

let () = Vmm_core.set_tmpdir (Fpath.v "/run/albatross")

let remap_errors result =
  Result.map_error
    (function
      | `Exception -> `Msg "exception occured during request"
      | `Eof -> `Msg "encountered EOF during request"
      | `Toomuch -> `Msg "got too much during request"
      | `Port_already_allocated p ->
          `Msg ("Port already allocated: " ^ string_of_int p)
      | `Full -> `Msg "Couldn't allocate an IP: is full."
      | `Parse m -> `Msg ("ASN parse error: " ^ m))
    result

let process fd =
  let+ result = Vmm_lwt.read_wire fd in
  match result with
  | Error `Eof -> Error (`Msg "Success")
  | Error _ -> Error (`Msg "Success")
  | Ok wire -> Ok wire

let _read (fd, next) =
  (* now we busy read and process output *)
  let rec loop () =
    let* _ = process fd in
    loop ()
  in
  match next with `Read -> loop () | `End -> process fd

let with_tmp ~prefix ~suffix fn =
  let tmp_path = Filename.temp_file prefix suffix in
  Lwt.finalize
    (fun () -> fn tmp_path)
    (fun () ->
      Unix.unlink tmp_path;
      Lwt.return_unit)

module Unikernel = struct
  module Fpath = struct
    include Fpath

    let of_yojson = function
      | `String v -> Fpath.of_string v |> Result.map_error (fun (`Msg s) -> s)
      | _ -> Error "type error"

    let to_yojson v = `String (Fpath.to_string v)
  end

  module Docker = struct
    module Image = struct
      include Docker.Image

      let to_yojson v = `String (hash v)

      let of_yojson = function
        | `String v -> Ok (of_hash v)
        | _ -> Error "type error"
    end
  end

  type t = { image : Docker.Image.t; location : Fpath.t } [@@deriving yojson]

  let of_docker ~image ~location = { image; location }
end


module Port = struct
  type t = { source : int; target : int }

  let pp f { source; target } = Fmt.pf f "%d->%d" source target
end

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

module Config = struct
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
end

module Info = struct
  type t = {
    status : [ `Running | `Exited ];
    config : Config.t;
    ip : Ipaddr.V4.t;
  }
  [@@deriving yojson]

  let status t = t.status

  let marshal t = to_yojson t |> Yojson.Safe.to_string

  let unmarshal t = Yojson.Safe.from_string t |> of_yojson |> Result.get_ok

  let digest = marshal

  let pp f { config; ip; _ } = Fmt.pf f "%s @%a" config.service Ipaddr.V4.pp ip
end

module IpOp = struct
  type t = No_context

  let id = "get-ip"

  let auto_cancel = true

  module Key = Current.String

  module Value = struct
    type t = Ipaddr.V4.t

    let marshal = Ipaddr.V4.to_string

    let unmarshal = Ipaddr.V4.of_string_exn
  end

  let build No_context job key =
    let* () = Current.Job.start ~level:Mostly_harmless job in

    let* socket = Client.Wire.connect () in
    let** ip =
      Lwt.finalize
        (fun () -> Client.IpManager.request ~socket key |> Lwt.map remap_errors)
        (fun () -> Client.Wire.safe_close socket)
    in
    let** ip = Lwt.return (remap_errors ip) in
    Current.Job.log job "Got IP: for %s: %a" key Ipaddr.V4.pp ip.ip;
    Lwt.return_ok ip.ip

  let pp f key = Fmt.pf f "Get IP for %S " key
end

module IpCache = Current_cache.Make (IpOp)

let get_ip config =
  let open Current.Syntax in
  Current.component "get IP"
  |> let> config = config in
     let fake_config = Config.v config (Ipaddr.V4.of_string_exn "0.0.0.0") in
     let key = Config.name fake_config in
     IpCache.get No_context key

module OpDeploy = struct
  type t = No_context

  let id = "mirage-deploy"

  module Key = struct
    type t = { name : string }

    let digest t = t.name
  end

  module Value = struct
    type t = {
      unikernel : Unikernel.t;
      args : string list;
      memory : int;
      network : string;
    }

    let digest { unikernel; args; memory; network } =
      Fmt.str "%s|%a|%a|%d|%s"
        (Docker.Image.digest unikernel.image)
        Fmt.(list ~sep:sp string)
        args Fpath.pp unikernel.location memory network
      |> Digest.string |> Digest.to_hex
  end

  module Outcome = Current.Unit

  let run image =
    Raw.Cmd.docker [ "container"; "run"; "-d"; Raw.Image.hash image ]

  let docker_cp src dst = Raw.Cmd.docker [ "cp"; src; dst ]

  let publish No_context job { Key.name }
      { Value.unikernel; args; memory; network } =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let image = Raw.Image.of_hash (Docker.Image.hash unikernel.image) in
    let* () = Current.Job.start job ~level:Mostly_harmless in
    Current.Job.log job "Deploy %a -> %s" Raw.Image.pp image name;
    (* Extract unikernel image from Docker image: *)
    Current.Job.log job "Extracting unikernel";
    with_tmp ~prefix:"ocurrent-deployer-" ~suffix:".hvt" @@ fun tmp_path ->
    let** () =
      Raw.Cmd.with_container ~docker_context:None ~job ~kill_on_cancel:true
        (run image ~docker_context:None) (fun id ->
          let src = Fmt.str "%s:%a" id Fpath.pp unikernel.location in
          Current.Process.exec ~cancellable:true ~job
            (docker_cp ~docker_context:None src tmp_path))
    in
    Current.Job.log job "Check if unikernel exists in albatross";
    Current.Job.log job "Unikernel name: %s" name;
    (* Check if unikernel exists and destroy it (actually, that shouldn't
       happen)*)
    let** vmm_unikernel_name = Lwt.return (Vmm_core.Name.of_string name) in
    let** info = Client.Albatross.show_unikernel vmm_unikernel_name in
    let** () =
      match info with
      | [] -> Lwt.return_ok ()
      | i ->
          Current.Job.log job "Defeat the unikernel: %a"
            (Fmt.list (fun f (name, _) -> Vmm_core.Name.pp f name))
            i;
          Client.Albatross.destroy_unikernel vmm_unikernel_name
    in
    Current.Job.log job "Create the unikernel";
    (* Create the unikernel *)
    let** () =
      Client.Albatross.spawn_unikernel ~path:tmp_path ~name:vmm_unikernel_name
        ~memory ~network ~args ()
    in
    Lwt_result.return ()

  let pp f (key, _v) = Fmt.pf f "@[<v2>deploy %s@]" key.Key.name

  let auto_cancel = true
end

module Deploy = Current_cache.Output (OpDeploy)

let deploy_albatross config =
  let open Current.Syntax in
  let deploy =
    Current.component "Deploy to albatross"
    |> let> config = config in
       Deploy.set No_context
         { name = Config.name config }
         {
           unikernel = config.unikernel;
           args = config.args;
           memory = config.memory;
           network = config.network;
         }
  in
  let monitor =
    (* TODO: actually monitor *)
    Current.component "Monitor status"
    |> let> config = config and> () = deploy in
       let name = Config.name config in
       let ip = config.ip in
       let read () = Lwt_result.return { Info.status = `Running; ip; config } in
       let watch _refresh = Lwt.return (fun () -> Lwt.return ()) in
       let pp f = Fmt.pf f "Monitor deployment %s on %a" name Ipaddr.V4.pp ip in

       Current.Monitor.create ~read ~watch ~pp |> Current.Monitor.get
  in
  monitor

module Deployment = struct
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

  module Outcome = Deployment

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
              { Current_deployer_api.Types.PortRedirection.source; target })
        ports
    in
    let* socket = Client.Wire.connect () in
    let** result =
      Lwt.finalize
        (fun () ->
          Client.Deployments.create ~socket
            {
              (* todo: a bit flaky here *)
              Current_deployer_api.Types.DeploymentInfo.ip =
                { tag = service; ip };
              ports;
              name = service;
            }
          |> Lwt.map remap_errors)
        (fun () -> Client.Wire.safe_close socket)
    in
    let** () = Lwt.return (result |> remap_errors) in

    Lwt_result.return { Deployment.service }

  let pp f (key, _v) = Fmt.pf f "@[<v2>deploy %s@]" key.Key.service

  let auto_cancel = true
end

module Publish = Current_cache.Output (OpPublish)

let publish ~service ?(ports = []) info =
  let open Current.Syntax in
  Current.component "Publish port"
  |> let> info = info in
     Publish.set No_context { service } { ports; ip = info.Info.ip }

module OpCollect = struct
  type t = No_context

  let id = "collect"

  let pp f _ = Fmt.string f "collect"

  let auto_cancel = false

  module Key = struct
    type t = Deployment.t list

    let digest t =
      t
      |> List.sort (fun a b -> String.compare a.Deployment.service b.service)
      |> List.map (fun d -> d.Deployment.service)
      |> String.concat "|"
  end

  module Value = Current.Unit

  let build No_context job roots =
    let* () = Current.Job.start ~level:Dangerous job in

    let module StringSet = Set.Make (String) in
    let deployed_services =
      StringSet.of_list (List.map (fun t -> t.Deployment.service) roots)
    in
    let perform ~socket =
      (* Step 1: daemon: remove unused deployments *)
      let** deployments =
        Client.Deployments.list ~socket () |> Lwt.map remap_errors
      in
      let deployments_to_keep, deployments_to_remove =
        List.partition
          (fun (deployment : Current_deployer_api.Types.DeploymentInfo.t) ->
            StringSet.mem deployment.name deployed_services)
          deployments
      in
      Current.Job.log job "Roots:";
      List.iter
        (fun (deployment : Current_deployer_api.Types.DeploymentInfo.t) ->
          Current.Job.log job " - %s @%a (%s)" deployment.name Ipaddr.V4.pp
            deployment.ip.ip deployment.ip.tag)
        deployments_to_keep;

      Current.Job.log job "Stage 1: remove unused deployments";
      let* () =
        Lwt_list.iter_s
          (fun (deployment : Current_deployer_api.Types.DeploymentInfo.t) ->
            Current.Job.log job "- %s" deployment.name;
            Client.Deployments.remove ~socket deployment.name |> Lwt.map ignore)
          deployments_to_remove
      in
      let ips_to_keep =
        List.map
          (fun (d : Current_deployer_api.Types.DeploymentInfo.t) ->
            Ipaddr.V4.to_string d.ip.ip)
          deployments_to_keep
        |> StringSet.of_list
      in

      (* Step 1.5: collect IPs and unikernel tags *)
      let** ips = Client.IpManager.list ~socket () |> Lwt.map remap_errors in
      let ips_to_remove =
        List.filter
          (fun (ip : Current_deployer_api.Types.Ip.t) ->
            not (StringSet.mem (Ipaddr.V4.to_string ip.ip) ips_to_keep))
          ips
      in
      let removed_ips_tags =
        List.map (fun ip -> ip.Current_deployer_api.Types.Ip.tag) ips_to_remove
        |> StringSet.of_list
      in
      (* Step 2: albatross: remove unikernels starting by a deployment's service
         name *)
      Current.Job.log job "Stage 2: remove unused unikernels";
      let ( let** ) = Lwt_result.bind in
      let** unikernels = Client.Albatross.list_unikernels () in
      let unikernels_to_remove =
        List.filter
          (fun (name, _) ->
            let tag = Vmm_core.Name.to_string name in
            StringSet.mem tag removed_ips_tags)
          unikernels
      in
      let* () =
        Lwt_list.iter_s
          (fun (name, _) ->
            Current.Job.log job "- %a" Vmm_core.Name.pp name;
            Client.Albatross.destroy_unikernel name |> Lwt.map ignore)
          unikernels_to_remove
      in
      (* Step 3: daemon: remove unused IPs *)
      Current.Job.log job "Stage 3: remove unused IPs";
      let+ () =
        Lwt_list.iter_s
          (fun (ip : Current_deployer_api.Types.Ip.t) ->
            Current.Job.log job "- %s @%a" ip.tag Ipaddr.V4.pp ip.ip;
            Client.IpManager.remove ~socket ip.tag |> Lwt.map ignore)
          ips_to_remove
      in
      Ok ()
    in

    let* socket = Client.Wire.connect () in
    Lwt.finalize
      (fun () -> perform ~socket)
      (fun () -> Client.Wire.safe_close socket)
end

module Collect = Current_cache.Make (OpCollect)

let collect deployments =
  let open Current.Syntax in
  Current.component "collect"
  |> let> deployments = deployments in
     Collect.get No_context deployments
