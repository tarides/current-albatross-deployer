module Unikernel : sig
  type t
  (** The type for an unikernel image *)

  val of_docker : image:Current_docker.Default.Image.t -> location:Fpath.t -> t
  (** Extract an unikernel image from docker. It's assumed to be an hvt target
      unikernel *)
end

module Port : sig
  type t = { source : int; target : int }
  (** Defines a port redirection from [source] to [target] *)
end

module Info : sig
  type t
  (** A deployed unikernel information *)

  val status : t -> [ `Running | `Exited ]

  val pp : t Fmt.t

  val marshal : t -> string

  val unmarshal : string -> t

  val digest : t -> string
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

val get_ip : Config.Pre.t Current.t -> Ipaddr.V4.t Current.t
(** Use the current-deployerd service to obtain an IP for the configuration. *)

val deploy_albatross : Config.t Current.t -> Info.t Current.t
(** Deploy the configuration to albatross *)

module Deployment : sig
  type t
end

val publish :
  service:string ->
  ?ports:Port.t list ->
  Info.t Current.t ->
  Deployment.t Current.t
(** Publish a service, optionally exposing ports to the deployed unikernel *)

val collect : Deployment.t list Current.t -> unit Current.t
(** Garbage collect IPs and deployments managed by current-deployer and kill
    corresponding unikernels. Only specified deployments are kept. *)
