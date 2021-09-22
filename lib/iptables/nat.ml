type port = Port of int | Port_range of int * int

module Port = struct
  type t = port

  let to_string = function
    | Port v -> Fmt.str "%d" v
    | Port_range (b, e) -> Fmt.str "%d-%d" b e
end

module Multiport = struct
  type t = Port.t list

  let to_string v = List.map Port.to_string v |> String.concat ","
end

type port_or_multiport = Multiport.t

type ip = Ip of Ipaddr.V4.t | Prefix of Ipaddr.V4.Prefix.t

module Ip = struct
  let to_string = function
    | Ip v -> Ipaddr.V4.to_string v
    | Prefix v -> Ipaddr.V4.Prefix.to_string v
end

module Cmd = Bos.Cmd

module Negatable = struct
  type 'a t = V of 'a | Neg of 'a

  let not = function V v -> Neg v | Neg v -> V v

  let to_command v_cmd = function
    | V value -> v_cmd value
    | Neg value -> Cmd.(v "!" %% v_cmd value)
end

type 'a negatable = 'a Negatable.t

let option_command ?(extension = Cmd.empty) name f = function
  | None -> Cmd.empty
  | Some value ->
      Cmd.(
        extension %% Negatable.to_command (fun value -> v name %% f value) value)

module Match = struct
  let get_multiport ports =
    let open Negatable in
    let ok =
      List.exists
        (function
          | Some (V [ _; _; _ ] | Neg [ _; _; _ ]) -> true
          | Some (V _ | Neg _) | None -> false)
        ports
    in
    if ok then (Cmd.(v "-m" % "multiport"), "s") else (Cmd.empty, "")

  module Tcp = struct
    type flag = SYN | ACK | FIN | RST | URG | PSH | ALL | NONE

    let flag_to_string = function
      | SYN -> "SYN"
      | ACK -> "ACK"
      | FIN -> "FIN"
      | RST -> "RST"
      | URG -> "URG"
      | PSH -> "PSH"
      | ALL -> "ALL"
      | NONE -> "NONE"

    type state = ESTABLISHED | NEW | INVALID | RELATED

    let state_to_string = function
      | ESTABLISHED -> "ESTABLISHED"
      | NEW -> "NEW"
      | INVALID -> "INVALID"
      | RELATED -> "RELATED"

    type t = {
      sport : Multiport.t negatable option;
      dport : Multiport.t negatable option;
      state : state list negatable option;
      flags : (flag * bool) list negatable option;
    }

    let v ?sport ?dport ?state ?flags () = { sport; dport; state; flags }

    let flags_selection_command flags =
      let flags_str = List.map (fun (a, b) -> (flag_to_string a, b)) flags in
      let all_flags = List.map fst flags_str |> String.concat "," in
      let set_flags =
        List.filter snd flags_str |> List.map fst |> String.concat ","
      in
      Cmd.(v all_flags % set_flags)

    let to_command { sport; dport; state; flags } =
      let multiport, s_multiport = get_multiport [ sport; dport ] in
      Cmd.(
        multiport
        %% option_command
             ("--source-port" ^ s_multiport)
             (fun x -> Cmd.v (Multiport.to_string x))
             sport
        %% option_command
             ("--destination-port" ^ s_multiport)
             (fun x -> Cmd.v (Multiport.to_string x))
             dport
        %% option_command
             ~extension:Cmd.(v "-m" % "state")
             "--state"
             (fun state ->
               Cmd.v (List.map state_to_string state |> String.concat ","))
             state
        %% option_command "--tcp-flags" flags_selection_command flags)
  end

  module Udp = struct
    type t = {
      sport : Multiport.t negatable option;
      dport : Multiport.t negatable option;
    }

    let v ?sport ?dport () = { sport; dport }

    let to_command { sport; dport } =
      let multiport, s_multiport = get_multiport [ sport; dport ] in
      Cmd.(
        multiport
        %% option_command
             ("--source-port" ^ s_multiport)
             (fun x -> Cmd.v (Multiport.to_string x))
             sport
        %% option_command
             ("--destination-port" ^ s_multiport)
             (fun x -> Cmd.v (Multiport.to_string x))
             dport)
  end

  module Icmp = struct
    type t = { typ : int negatable option }

    let v ?typ () = { typ }

    let to_command { typ } =
      option_command "--icmp-type"
        (fun value -> Cmd.v (string_of_int value))
        typ
  end

  module All = struct
    type t = {
      sport : Multiport.t negatable option;
      dport : Multiport.t negatable option;
      tcp_state : Tcp.state list negatable option;
      tcp_flags : (Tcp.flag * bool) list negatable option;
      icmp_type : int negatable option;
    }

    let v ?sport ?dport ?tcp_state ?tcp_flags ?icmp_type () =
      { sport; dport; tcp_state; tcp_flags; icmp_type }

    let to_command { sport; dport; tcp_state; tcp_flags; icmp_type } =
      let multiport, s_multiport = get_multiport [ sport; dport ] in
      Cmd.(
        multiport
        %% option_command
             ("--source-port" ^ s_multiport)
             (fun x -> Cmd.v (Multiport.to_string x))
             sport
        %% option_command
             ("--destination-port" ^ s_multiport)
             (fun x -> Cmd.v (Multiport.to_string x))
             dport
        %% option_command
             ~extension:Cmd.(v "-m" % "state")
             "--state"
             (fun state ->
               Cmd.v (List.map Tcp.state_to_string state |> String.concat ","))
             tcp_state
        %% option_command "--tcp-flags" Tcp.flags_selection_command tcp_flags
        %% option_command "--icmp-type"
             (fun value -> Cmd.v (string_of_int value))
             icmp_type)
  end

  module Addrtype = struct
    type addrtype =
      [ `Unspec
      | `Unicast
      | `Local
      | `Broadcast
      | `Anycast
      | `Multicast
      | `Blackhole
      | `Unreachable
      | `Prohibit ]

    let addrtype_to_string = function
      | `Unspec -> "UNSPEC"
      | `Unicast -> "UNICAST"
      | `Local -> "LOCAL"
      | `Broadcast -> "BROADCAST"
      | `Anycast -> "ANYCAST"
      | `Multicast -> "MULTICAST"
      | `Blackhole -> "BLACKHOLE"
      | `Unreachable -> "UNREACHABLE"
      | `Prohibit -> "PROHIBIT"

    let addrtype_to_command v = Cmd.v (addrtype_to_string v)

    type t = {
      src_type : addrtype negatable option;
      dst_type : addrtype negatable option;
    }

    let v ?src_type ?dst_type () = { src_type; dst_type }

    let to_command { src_type; dst_type } =
      Cmd.(
        v "-m" % "addrtype"
        %% option_command "--dst-type" addrtype_to_command dst_type
        %% option_command "--src-type" addrtype_to_command src_type)
  end

  type kind =
    | In_interface of string
    | Out_interface of string
    | Ip_destination of ip
    | Ip_source of ip
    | Mac_source of Macaddr.t
    | Protocol of
        [ `Tcp of Tcp.t | `Udp of Udp.t | `Icmp of Icmp.t | `All of All.t ]

  type t = NG of kind negatable | V of Addrtype.t

  let kind_to_cmd = function
    | In_interface value -> Cmd.(v "--in-interface" % value)
    | Out_interface value -> Cmd.(v "--out-interface" % value)
    | Ip_destination value -> Cmd.(v "--destination" % Ip.to_string value)
    | Ip_source value -> Cmd.(v "--source" % Ip.to_string value)
    | Mac_source value -> Cmd.(v "--mac-source" % Macaddr.to_string value)
    | Protocol (`Tcp tcp) -> Cmd.(v "--protocol" % "tcp" %% Tcp.to_command tcp)
    | Protocol (`Udp udp) -> Cmd.(v "--protocol" % "udp" %% Udp.to_command udp)
    | Protocol (`Icmp icmp) ->
        Cmd.(v "--protocol" % "icmp" %% Icmp.to_command icmp)
    | Protocol (`All all) -> Cmd.(v "--protocol" % "all" %% All.to_command all)

  let to_cmd = function
    | NG ng -> Negatable.to_command kind_to_cmd ng
    | V v -> Addrtype.to_command v

  let not = function
    | NG t -> NG (Negatable.not t)
    | V _ -> failwith "not supported"

  let in_interface x = NG (Negatable.V (In_interface x))

  let out_interface x = NG (Negatable.V (Out_interface x))

  let ip_destination x = NG (Negatable.V (Ip_destination x))

  let ip_source x = NG (Negatable.V (Ip_source x))

  let mac_source x = NG (Negatable.V (Mac_source x))

  let protocol x = NG (Negatable.V (Protocol x))

  let addrtype x = V x
end

module Action = struct
  type t =
    | Source_nat of { port : port option; ip : Ipaddr.V4.t }
    | Masquerade
    | Destination_nat of { port : port option; ip : Ipaddr.V4.t }
    | Redirect of port

  let source_nat ?port ip = Source_nat { port; ip }

  let masquerade = Masquerade

  let destination_nat ?port ip = Destination_nat { port; ip }

  let redirect port = Redirect port

  let port_to_string_after_ip port =
    port
    |> Option.map (fun v -> ":" ^ Port.to_string v)
    |> Option.value ~default:""

  let to_cmd = function
    | Source_nat { port; ip } ->
        Cmd.(
          v "-j" % "SNAT" % "--to-source"
          % (Ipaddr.V4.to_string ip ^ port_to_string_after_ip port))
    | Destination_nat { port; ip } ->
        Cmd.(
          v "-j" % "DNAT" % "--to-destination"
          % (Ipaddr.V4.to_string ip ^ port_to_string_after_ip port))
    | Masquerade -> Cmd.(v "-j" % "MASQUERADE")
    | Redirect port ->
        Cmd.(v "-j" % "REDIRECT" % "--to-ports" % Port.to_string port)
end

module Rule = struct
  type t = { matches : Match.t list; action : Action.t }

  type handle = t

  let v matches action = { matches; action }

  let to_cmd { matches; action } =
    let matches = List.map Match.to_cmd matches in
    let matches_cmd = List.fold_left Cmd.add_args Cmd.empty matches in
    Cmd.(matches_cmd %% Action.to_cmd action)
end

type t = { name : string; mutable rules : Rule.t list }

module Exec = struct
  let ipt_nat cmd =
    let cmd = Cmd.(v "iptables" % "-t" % "nat" %% cmd) in
    Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.out_stdout |> Result.get_ok |> ignore
end

module Raw = struct
  let add_rule ~chain rule =
    Exec.ipt_nat Cmd.(v "--append" % chain %% Rule.to_cmd rule)

  let remove_rule ~chain rule =
    Exec.ipt_nat Cmd.(v "--delete" % chain %% Rule.to_cmd rule)
end

(* Create the iptable chain with the given initial set of rules *)
let init ~name rules =
  Exec.ipt_nat
    Cmd.(
      v "--delete" % "PREROUTING" % "-p" % "all" % "-m" % "addrtype"
      % "--dst-type" % "LOCAL" % "--jump" % name);
  Exec.ipt_nat
    Cmd.(
      v "--delete" % "OUTPUT" % "-p" % "all" % "-m" % "addrtype" % "--dst-type"
      % "LOCAL" % "--jump" % name);
  Exec.ipt_nat Cmd.(v "--flush" % name);
  Exec.ipt_nat Cmd.(v "--delete-chain" % name);
  Exec.ipt_nat Cmd.(v "--new-chain" % name);
  List.iter (Raw.add_rule ~chain:name) rules;
  Exec.ipt_nat
    Cmd.(
      v "--append" % "PREROUTING" % "-p" % "all" % "-m" % "addrtype"
      % "--dst-type" % "LOCAL" % "--jump" % name);
  Exec.ipt_nat
    Cmd.(
      v "--append" % "OUTPUT" % "-p" % "all" % "-m" % "addrtype" % "--dst-type"
      % "LOCAL" % "--jump" % name);
  { name; rules }

(* Add a new rule to the chain *)
let add_rule t new_rule =
  t.rules <- new_rule :: t.rules;
  Raw.add_rule ~chain:t.name new_rule;
  new_rule

let add_rules t rules = List.rev_map (add_rule t) rules |> List.rev

(* Remove a rule from the chain *)
let remove_rule t rule =
  let without_rule, has_rule =
    List.fold_left
      (fun (acc, ok) v -> if v = rule then (acc, true) else (v :: acc, ok))
      ([], false) t.rules
  in
  if has_rule then (
    t.rules <- without_rule;
    Raw.remove_rule ~chain:t.name rule)
  else raise Not_found

(* Flush rules *)
let flush_rules t =
  Exec.ipt_nat Cmd.(v "--flush" % t.name);
  t.rules <- []
