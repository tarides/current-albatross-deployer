module Rpc : sig
  type ('a, 'b) t

  type error = [ `Parse of string ]

  type untagged_buffer

  val get_client :
    ('a, 'b) t ->
    ('a -> untagged_buffer) * (untagged_buffer -> ('b, error) result)

  val get_server :
    ('a, 'b) t ->
    (untagged_buffer -> ('a, error) result) * ('b -> untagged_buffer)

  module Tag : sig
    type ('a, 'b) rpc = ('a, 'b) t

    type t

    module Map : Map.S with type key = t

    val v : _ rpc -> t

    val strip : Cstruct.t -> t * untagged_buffer

    val add : t -> untagged_buffer -> Cstruct.t
  end
end

module Types : sig
  module PortRedirection : sig
    type t = { source : int; target : int }
  end

  module Ip : sig
    type t = { ip : Ipaddr.V4.t; tag : string }
  end

  module DeploymentInfo : sig
    type t = { ip : Ip.t; ports : PortRedirection.t list; name : string }
  end
end

module Spec : sig
  module IpManager : sig
    val list : (unit, Types.Ip.t list) Rpc.t

    val request : (string, (Types.Ip.t, [ `Full ]) result) Rpc.t

    val free : (string, (Types.Ip.t, [ `Not_found ]) result) Rpc.t
  end

  module Deployments : sig
    val list : (unit, Types.DeploymentInfo.t list) Rpc.t

    val create :
      ( Types.DeploymentInfo.t,
        (unit, [ `Port_already_allocated of int ]) result )
      Rpc.t

    val delete : (string, (Types.DeploymentInfo.t, [ `Not_found ]) result) Rpc.t
  end
end
