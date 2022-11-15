module Nat = Iptables.Nat

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug)

let action =
  Nat.Action.destination_nat ~port:(Nat.Port 8080)
    (Ipaddr.V4.of_string "172.17.0.2" |> Result.get_ok)

let match' =
  Nat.Match.(
    protocol (`Tcp (Tcp.v ~dport:(Nat.Negatable.V [ Nat.Port 8080 ]) ())))

let rule = Nat.Rule.v [ match' ] action
let ipt = Nat.init ~name:"ALBATROSS" [ rule ]
