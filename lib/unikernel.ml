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
  let run image =
    Raw.Cmd.docker [ "container"; "run"; "-d"; Raw.Image.hash image ]
  in
  let docker_cp src dst = Raw.Cmd.docker [ "cp"; src; dst ] in
  let image = Raw.Image.of_hash (Docker.Image.hash unikernel.image) in
  Raw.Cmd.with_container ~docker_context:None ~job ~kill_on_cancel:true
    (run image ~docker_context:None) (fun id ->
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
  let spec ?(target = "hvt") ?(extra_flags = "") config_file =
    let open Obuilder_spec in
    let network = [ "host" ] in
    let download_cache =
      Obuilder_spec.Cache.v "opam-archives"
        ~target:"/home/opam/.opam/download-cache"
    in
    let dune_cache =
      Obuilder_spec.Cache.v "opam-dune-cache" ~target:"/home/opam/.cache/dune"
    in
    let cache = [ download_cache; dune_cache ] in

    let build =
      stage ~from:"ocaml/opam:ubuntu-ocaml-4.12"
        [
          run ~network "apt install -y m4 pkg-config";
          run ~network
            "cd ~/opam-repository && git pull origin master && git reset \
             --hard 7af4b295dc2de0b2b594c05ab4af40ef963bbc5c && opam update";
          run ~network
            "opam repo add mirage-dev \
             https://github.com/mirage/mirage-dev.git#713b884b36a58579515b2ea55ec057f6fe310a52";
          run ~network ~cache "opam depext -ui mirage";
          run "mkdir -p /home/opam/repo";
          workdir "/home/opam/repo";
          copy ~from:`Context [ config_file ] ~dst:config_file;
          env "TARGET" target;
          env "EXTRA_FLAGS" extra_flags;
          run
            "opam config exec -- mirage configure -t $TARGET $EXTRA_FLAGS \
             --extra-repo \
             https://github.com/mirage/opam-overlays.git#aa30403f107034500e5f697ddfc6e954117fc059";
          run ~network ~cache "opam config exec -- make depend";
          copy ~from:`Context [ "./" ] ~dst:"/home/opam/repo/";
          run
            "opam config exec -- mirage configure -t $TARGET $EXTRA_FLAGS \
             --extra-repo \
             https://github.com/mirage/opam-overlays.git#aa30403f107034500e5f697ddfc6e954117fc059";
          run "opam config exec -- mirage build";
        ]
    in
    stage ~child_builds:[ ("build", build) ] ~from:"scratch"
      [
        copy ~from:(`Build "build")
          [ "/home/opam/repo/dist/<UNIKERNEL_NAME>." ^ target ]
          ~dst:("/unikernel." ^ target);
      ]

  let digest ~config_file repo =
    Fmt.str "%a|%a" Fpath.pp config_file Current_git.Commit.pp repo
    |> Digest.string |> Digest.to_hex

  let build_image ~config_file repo =
    let dockerfile_content =
      let+ config_file = config_file in
      spec (Fpath.to_string config_file)
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
        Fpath.(v "tmp" / "current-deployer-unikernel" / id / "Dockerfile")
      in
      let+ () = Current_fs.save path dockerfile_content and+ path = path in
      `File path
    in
    let+ image =
      Current_docker.Default.build ~dockerfile ~pull:false (`Git repo)
    in
    { location = Fpath.v "/unikernel.hvt"; image }
end

let of_git ~config_file repo = Git.build_image ~config_file repo
