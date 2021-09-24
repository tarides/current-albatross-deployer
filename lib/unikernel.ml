module Docker = struct
  module Image = struct
    include Current_docker.Default.Image

    let to_yojson v = `String (hash v)

    let of_yojson = function
      | `String v -> Ok (of_hash v)
      | _ -> Error "type error"
  end
end

module Fpath = struct
  include Fpath

  let of_yojson = function
    | `String v -> Fpath.of_string v |> Result.map_error (fun (`Msg s) -> s)
    | _ -> Error "type error"

  let to_yojson v = `String (Fpath.to_string v)
end

type t = { location : Fpath.t; image : Docker.Image.t } [@@deriving yojson]

let extract_to ~path ~job unikernel =
  let module Raw = Current_docker.Raw in
  let create image =
    Raw.Cmd.docker
      [ "create"; "--entrypoint"; "/nonexistent"; Raw.Image.hash image ]
  in
  let docker_cp src dst = Raw.Cmd.docker [ "cp"; src; dst ] in
  let image = Raw.Image.of_hash (Docker.Image.hash unikernel.image) in
  Raw.Cmd.with_container ~docker_context:None ~job ~kill_on_cancel:true
    (create image ~docker_context:None) (fun id ->
      let src = Fmt.str "%s:%a" id Fpath.pp unikernel.location in
      Current.Process.exec ~cancellable:true ~job
        (docker_cp ~docker_context:None src (Fpath.to_string path)))

let digest { image; location } =
  Fmt.str "%s|%a" (Docker.Image.digest image) Fpath.pp location
  |> Digest.string |> Digest.to_hex

open Current.Syntax

let of_docker ~location image =
  let+ image = image in
  { location; image }

module Git = struct
  let network = [ "host" ]

  let download_cache =
    Obuilder_spec.Cache.v "opam-archives"
      ~target:"/home/opam/.opam/download-cache"

  let dune_cache =
    Obuilder_spec.Cache.v "opam-dune-cache" ~target:"/home/opam/.cache/dune"

  let cache = [ download_cache; dune_cache ]

  let spec_mirage_4 ?(target = "hvt") ?(extra_flags = "") config_file =
    let open Obuilder_spec in
    let base_path = Fpath.v "/home/opam/repo" in
    let config_file_path = Fpath.(base_path // config_file) in
    let config_file_dir, config_file_name = Fpath.split_base config_file_path in
    let config_file_dir, config_file_name =
      (Fpath.to_string config_file_dir, Fpath.to_string config_file_name)
    in

    let build =
      stage ~from:"ocaml/opam:ubuntu-ocaml-4.11"
        [
          user ~uid:1000 ~gid:1000;
          run ~network "sudo apt install -y m4 pkg-config";
          run ~network
            "cd ~/opam-repository && git pull origin master && git reset \
             --hard ef82e5bc09e89868e9393bc8ded218b02517876e && opam update";
          run ~network
            "opam repo add mirage-dev \
             https://github.com/mirage/mirage-dev.git#0c7c0a14240236bf00c5ccdceab0612f09cbe339";
          env "DUNE_CACHE" "enabled";
          env "DUNE_CACHE_DUPLICATION" "copy";
          env "DUNE_CACHE_TRANSPORT" "direct";
          run ~network ~cache
            "opam depext -ui ocaml-freestanding mirage opam-monorepo";
          run "mkdir -p %s" config_file_dir;
          workdir config_file_dir;
          copy ~from:`Context
            [ Fpath.to_string config_file ]
            ~dst:config_file_name;
          run ~cache
            "opam config exec -- mirage configure -f %s -o unikernel -t %s %s \
             --extra-repo \
             https://github.com/mirage/opam-overlays.git#f033f8b770097e768cc974cc407e0cd6d7889d63"
            config_file_name target extra_flags;
          run ~network ~cache "opam config exec -- make depend";
          copy ~from:`Context [ "./" ] ~dst:(Fpath.to_string base_path);
          run ~cache
            "opam config exec -- mirage configure -f %s -o unikernel -t %s %s \
             --extra-repo \
             https://github.com/mirage/opam-overlays.git#f033f8b770097e768cc974cc407e0cd6d7889d63"
            config_file_name target extra_flags;
          run ~cache "opam config exec -- mirage build";
        ]
    in
    stage ~child_builds:[ ("build", build) ] ~from:"scratch"
      [
        copy ~from:(`Build "build")
          [
            Fpath.(
              v config_file_dir / "dist" / ("unikernel." ^ target) |> to_string);
          ]
          ~dst:("/unikernel." ^ target);
      ]

  let spec_mirage_3 ?(target = "hvt") ?(extra_flags = "") config_file =
    let open Obuilder_spec in
    let base_path = Fpath.v "/home/opam/repo" in
    let config_file_path = Fpath.(base_path // config_file) in
    let config_file_dir, config_file_name = Fpath.split_base config_file_path in
    let config_file_dir, config_file_name =
      (Fpath.to_string config_file_dir, Fpath.to_string config_file_name)
    in

    let build =
      stage ~from:"ocaml/opam:ubuntu-ocaml-4.11"
        [
          user ~uid:1000 ~gid:1000;
          run ~network "sudo apt install -y m4 pkg-config";
          run ~network
            "cd ~/opam-repository && git pull origin master && git reset \
             --hard ef82e5bc09e89868e9393bc8ded218b02517876e && opam update";
          env "DUNE_CACHE" "enabled";
          env "DUNE_CACHE_DUPLICATION" "copy";
          env "DUNE_CACHE_TRANSPORT" "direct";
          run ~network ~cache "opam depext -ui ocaml-freestanding mirage.3.10.4";
          run "mkdir -p %s" config_file_dir;
          workdir config_file_dir;
          copy ~from:`Context
            [ Fpath.to_string config_file ]
            ~dst:config_file_name;
          run ~cache
            "opam config exec -- mirage configure -f %s -o unikernel -t %s %s"
            config_file_name target extra_flags;
          run ~network ~cache "opam config exec -- make depend";
          copy ~from:`Context [ "./" ] ~dst:(Fpath.to_string base_path);
          run ~cache
            "opam config exec -- mirage configure -f %s -o unikernel -t %s %s"
            config_file_name target extra_flags;
          run ~cache "opam config exec -- make";
          run "ls";
        ]
    in
    stage ~child_builds:[ ("build", build) ] ~from:"scratch"
      [
        copy ~from:(`Build "build")
          [
            Fpath.(
              v config_file_dir / "dist" / ("unikernel." ^ target) |> to_string);
          ]
          ~dst:("/unikernel." ^ target);
      ]

  let digest ~config_file repo =
    Fmt.str "%a|%a" Fpath.pp config_file Current_git.Commit.pp repo
    |> Digest.string |> Digest.to_hex

  let build_image ~mirage_version ~config_file ?(args = Current.return []) repo
      =
    let spec =
      match mirage_version with
      | `Mirage_3 -> spec_mirage_3
      | `Mirage_4 -> spec_mirage_4
    in
    let dockerfile_content =
      let+ config_file = config_file and+ args = args in
      let extra_flags = String.concat " " args in
      spec ~extra_flags config_file
      |> Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:true
    in
    let id =
      let+ config_file = config_file and+ repo = repo in
      digest ~config_file repo
    in
    let dockerfile =
      let open Current.Syntax in
      let path =
        let+ id = id in
        let folder = Fpath.(v "/tmp" / "current-deployer" / id) in
        let _ = Bos.OS.Dir.create folder |> Result.get_ok in
        Fpath.(folder / "Dockerfile")
      in
      let+ () = Current_fs.save path dockerfile_content and+ path = path in
      `File path
    in
    let+ image =
      Current_docker.Default.build ~dockerfile ~pull:false (`Git repo)
    in
    { location = Fpath.v "/unikernel.hvt"; image }
end

let of_git ~mirage_version ~config_file ?args repo =
  Git.build_image ~mirage_version ~config_file ?args repo
