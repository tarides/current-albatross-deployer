module Docker = Current_docker.Default

module Unikernel : sig
  type t

  val of_docker : image:Docker.Image.t -> location:Fpath.t -> t
end

module Info : sig
  type t

  val ip : t -> Ipaddr.V4.t
end

module Args : sig
  type t

  (* get the arguments, first IP is unikernel's allocated IP. *)
  val v : (Ipaddr.V4.t -> string list) Current.t -> t
end

module Port : sig
  type t = { source : int; target : int }
end

(* Experimental *)

module E : sig
  module Info : sig
    type t

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
    end

    type t

    val v : Pre.t -> Ipaddr.V4.t -> t
  end

  val get_ip : Config.Pre.t Current.t -> Ipaddr.V4.t Current.t

  val deploy_albatross : Config.t Current.t -> Info.t Current.t

  module Deployment : sig
    type t
  end

  val publish :
    service:string ->
    ?ports:Port.t list ->
    Info.t Current.t ->
    Deployment.t Current.t

  val collect : Deployment.t list Current.t -> unit Current.t
end