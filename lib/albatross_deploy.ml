module Docker = Current_docker.Default
module Raw = Current_docker.Raw

let with_tmp ~prefix ~suffix fn =
  let tmp_path = Filename.temp_file prefix suffix in
  Lwt.finalize
    (fun () -> fn tmp_path)
    (fun () ->
      Unix.unlink tmp_path;
      Lwt.return_unit)

module Deployed = struct
  type t = { config : Config.t; mode : [ `Socket | `Tls of Client.tls_config ] }
  [@@deriving yojson]

  let marshal t = to_yojson t |> Yojson.Safe.to_string
  let unmarshal t = Yojson.Safe.from_string t |> of_yojson |> Result.get_ok
  let digest = marshal

  let pp f { config; _ } =
    Fmt.pf f "%s @%a" config.service Ipaddr.V4.pp config.ip
end

module Key = struct
  type t = { name : string }

  let digest t = t.name
end

module Value = struct
  type t = {
    unikernel : Unikernel.t;
    args : string list;
    memory : int;
    cpu : int;
    network : string option;
  }

  let digest { unikernel; args; memory; network; cpu } =
    Fmt.str "%s|%a|%d|%d|%a"
      (Unikernel.digest unikernel)
      Fmt.(list ~sep:sp string)
      args memory cpu
      Fmt.(option string)
      network
    |> Digest.string |> Digest.to_hex
end

module OpDeploy (C : Client.S) = struct
  type t = Current.Level.t

  let id = "mirage-deploy"

  module Key = Key
  module Value = Value
  module Outcome = Current.Unit

  let run image =
    Raw.Cmd.docker [ "container"; "run"; "-d"; Raw.Image.hash image ]

  let docker_cp src dst = Raw.Cmd.docker [ "cp"; src; dst ]

  let publish level job { Key.name }
      { Value.unikernel; args; memory; network; cpu } =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let* () = Current.Job.start job ~level in
    Current.Job.log job "Deploy %s -> %s" (Unikernel.digest unikernel) name;
    (* Extract unikernel image from Docker image: *)
    Current.Job.log job "Extracting unikernel";
    with_tmp ~prefix:"ocurrent-deployer-" ~suffix:".hvt" @@ fun tmp_path ->
    let** () = Unikernel.extract_to ~path:(Fpath.v tmp_path) ~job unikernel in
    Current.Job.log job "Check if unikernel exists in albatross";
    Current.Job.log job "Unikernel name: %s" name;
    (* Check if unikernel exists and destroy it (actually, that shouldn't
       happen)*)
    let** vmm_unikernel_name = Lwt.return (Vmm_core.Name.of_string name) in
    let** info = C.show_unikernel vmm_unikernel_name in
    let** () =
      match info with
      | [] -> Lwt.return_ok ()
      | i ->
          Current.Job.log job "Defeat the unikernel: %a"
            (Fmt.list (fun f (name, _) -> Vmm_core.Name.pp f name))
            i;
          C.destroy_unikernel vmm_unikernel_name
    in
    Current.Job.log job "Create the unikernel";
    (* Create the unikernel *)
    let** () =
      C.spawn_unikernel ~path:tmp_path ~name:vmm_unikernel_name ~memory ~network
        ~cpu ~args ()
    in
    Lwt_result.return ()

  let pp f (key, _v) = Fmt.pf f "@[<v2>deploy %s@]" key.Key.name
  let auto_cancel = true
end

let deploy_albatross ?(level = Current.Level.Average) ?label ?(mode = `Socket)
    config =
  let module C = (val Client.client_of_mode mode) in
  let module Deploy = Current_cache.Output (OpDeploy (C)) in
  let open Current.Syntax in
  let suffix = match label with None -> "" | Some v -> ": " ^ v in
  Current.component "Deploy to albatross%s" suffix
  |> let> config = config in
     Deploy.set level
       Key.{ name = config.Config.id }
       Value.
         {
           unikernel = config.unikernel;
           args = config.args;
           memory = config.memory;
           cpu = config.cpu;
           network = config.network;
         }
     |> Current.Primitive.map_result (function
          | Error _ as e -> e
          | Ok () -> Ok { Deployed.config; mode })
