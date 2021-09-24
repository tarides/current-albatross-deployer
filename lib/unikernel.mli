type t [@@deriving yojson]

val digest : t -> string

val extract_to :
  path:Fpath.t -> job:Current.Job.t -> t -> unit Current.or_error Lwt.t

val of_docker :
  location:Fpath.t -> Current_docker.Default.Image.t Current.t -> t Current.t

val of_git :
  ?extra_instructions:Obuilder_spec.op list Current.t ->
  mirage_version:[ `Mirage_3 | `Mirage_4 ] ->
  config_file:Fpath.t Current.t ->
  ?args:string list Current.t ->
  Current_git.Commit.t Current.t ->
  t Current.t
