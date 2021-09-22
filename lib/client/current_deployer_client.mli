type wire_error = [ `Eof | `Exception | `Parse of string | `Toomuch ]

type socket

module IpManager : sig
  val list :
    socket:socket ->
    unit ->
    (Current_deployer_api.Types.Ip.t list, wire_error) result Lwt.t

  val request :
    socket:socket ->
    string * Ipaddr.V4.Prefix.t * Ipaddr.V4.t list ->
    ((Current_deployer_api.Types.Ip.t, [ `Full ]) result, wire_error) result
    Lwt.t

  val remove :
    socket:socket ->
    string ->
    ( (Current_deployer_api.Types.Ip.t, [ `Not_found ]) result,
      wire_error )
    result
    Lwt.t
end

module Deployments : sig

  val list :
    socket:socket ->
    unit ->
    (Current_deployer_api.Types.DeploymentInfo.t list, wire_error) result Lwt.t

  val create :
    socket:socket ->
    Current_deployer_api.Types.DeploymentInfo.t ->
    ((unit, [ `Port_already_allocated of int ]) result, wire_error) result Lwt.t

  val remove :
    socket:socket ->
    string ->
    ( (Current_deployer_api.Types.DeploymentInfo.t, [ `Not_found ]) result,
      wire_error )
    result
    Lwt.t
end

val connect : unit -> socket Lwt.t

val close : socket -> unit Lwt.t
