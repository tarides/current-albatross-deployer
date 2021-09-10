type port = Port of int | Port_range of int * int

type port_or_multiport = port list

type ip = Ip of Ipaddr.V4.t | Prefix of Ipaddr.V4.Prefix.t

module Negatable : sig
  type 'a t = V of 'a | Neg of 'a

  val not : 'a t -> 'a t
end

type 'a negatable = 'a Negatable.t

module Match : sig
  module Tcp : sig
    type t

    type flag = SYN | ACK | FIN | RST | URG | PSH | ALL | NONE

    type state = ESTABLISHED | NEW | INVALID | RELATED

    val v :
      ?sport:port_or_multiport negatable ->
      ?dport:port_or_multiport negatable ->
      ?state:state list negatable ->
      ?flags:(flag * bool) list negatable ->
      unit ->
      t
  end

  module Udp : sig
    type t

    val v :
      ?sport:port_or_multiport negatable ->
      ?dport:port_or_multiport negatable ->
      unit ->
      t
  end

  module Icmp : sig
    type t

    val v : ?typ:int negatable -> unit -> t
  end

  module All : sig
    type t

    val v :
      ?sport:port_or_multiport negatable ->
      ?dport:port_or_multiport negatable ->
      ?tcp_state:Tcp.state list negatable ->
      ?tcp_flags:(Tcp.flag * bool) list negatable ->
      ?icmp_type:int negatable ->
      unit ->
      t
  end

  type t

  val not : t -> t

  val in_interface : string -> t

  val out_interface : string -> t

  val ip_destination : ip -> t

  val ip_source : ip -> t

  val mac_source : Macaddr.t -> t

  val protocol :
    [ `Tcp of Tcp.t | `Udp of Udp.t | `Icmp of Icmp.t | `All of All.t ] -> t
end

module Action : sig
  type t

  val source_nat : ?port:port -> Ipaddr.V4.t -> t

  val masquerade : t

  val destination_nat : ?port:port -> Ipaddr.V4.t -> t

  val redirect : port -> t
end

module Rule : sig
  type t

  type handle

  val v : Match.t list -> Action.t -> t

  val to_cmd : t -> Bos.Cmd.t
end

module Raw : sig
  val add_rule : chain:string -> Rule.t -> unit

  val remove_rule : chain:string -> Rule.t -> unit
end

type t

(* Create the iptable chain with the given initial set of rules *)
val init : name:string -> Rule.t list -> t

(* Add a new rule to the chain *)
val add_rule : t -> Rule.t -> Rule.handle

val add_rules : t -> Rule.t list -> Rule.handle list

(* Remove a rule from the chain *)
val remove_rule : t -> Rule.handle -> unit

(* Flush rules *)
val flush_rules : t -> unit
