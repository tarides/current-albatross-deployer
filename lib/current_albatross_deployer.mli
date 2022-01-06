module Unikernel : sig
  type t
  (** The type for an unikernel image *)

  val of_docker :
    location:Fpath.t -> Current_docker.Default.Image.t Current.t -> t Current.t
  (** Extract an unikernel image from docker. It's assumed to be an hvt target
      unikernel *)

  type instruction_fn =
    network:string list ->
    cache:Obuilder_spec.Cache.t list ->
    Obuilder_spec.op list

  val of_git :
    ?extra_instructions:instruction_fn Current.t ->
    ?opam_repository_commit:string Current.t ->
    mirage_version:[ `Mirage_3 | `Mirage_4 ] ->
    config_file:Fpath.t Current.t ->
    ?args:string list Current.t ->
    Current_git.Commit.t Current.t ->
    t Current.t
end

module Port : sig
  type t = { source : int; target : int }
  (** Defines a port redirection from [source] to [target] *)
end

module Info : sig
  type t
  (** A deployed unikernel information *)

  val status : t -> [ `Running | `Exited ]
end

module Config : sig
  module Pre : sig
    type t = {
      service : string;
      unikernel : Unikernel.t;
      args : Ipaddr.V4.t -> string list;
      memory : int;
      network : string;
    }
    (** Configuration preparation is configuration with an additional IP
        parameter that is provided later. *)
  end

  type t
  (** Deployment configuration *)

  val v : Pre.t -> Ipaddr.V4.t -> t
  (** Feed the prepared configuration with an IP *)
end

val get_ip :
  ?blacklist:Ipaddr.V4.t list ->
  prefix:Ipaddr.V4.Prefix.t ->
  Config.Pre.t Current.t ->
  Ipaddr.V4.t Current.t
(** Use the current-deployerd service to obtain an IP for the configuration. *)

module Deployed : sig
  type t

  val pp : t Fmt.t

  val marshal : t -> string

  val unmarshal : string -> t

  val digest : t -> string
end

val deploy_albatross :
  ?label:string -> Config.t Current.t -> Deployed.t Current.t
(** Deploy the configuration to albatross *)

val monitor : ?poll_rate:float -> Deployed.t Current.t -> Info.t Current.t

val is_running : Info.t Current.t -> unit Current.t

module Published : sig
  type t
end

val publish :
  service:string ->
  ?ports:Port.t list ->
  Deployed.t Current.t ->
  Published.t Current.t
(** Publish a service, optionally exposing ports to the deployed unikernel *)

val collect : Published.t list Current.t -> unit Current.t
(** Garbage collect IPs and deployments managed by current-albatross-deployer
    and kill corresponding unikernels. Only specified deployments are kept. *)
