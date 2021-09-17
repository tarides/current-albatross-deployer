module Nat = Iptables.Nat

let cmd = Alcotest.testable Bos.Cmd.pp Bos.Cmd.equal

let test_nat ~matches ~action command () =
  let rule = Nat.Rule.v matches action in
  Alcotest.(check cmd)
    "generated command is correct" command (Nat.Rule.to_cmd rule)

let command_generation_tests =
  Alcotest.
    [
      test_case "masquerade" `Quick
      @@ test_nat ~matches:[] ~action:Nat.Action.masquerade
           Bos.Cmd.(v "-j" % "MASQUERADE");
      test_case "dnat" `Quick
      @@ test_nat ~matches:[]
           ~action:
             (Nat.Action.destination_nat ~port:(Nat.Port 53)
                (Ipaddr.V4.of_string "192.168.0.9" |> Result.get_ok))
           Bos.Cmd.(v "-j" % "DNAT" % "--to-destination" % "192.168.0.9:53");
      test_case "snat with tcp match" `Quick
      @@ test_nat
           ~matches:
             [
               Nat.Match.(
                 protocol
                   (`Tcp
                     (Tcp.v
                        ~dport:(Nat.Negatable.V [ Nat.Port_range (0, 1024) ])
                        ~flags:
                          (Nat.Negatable.Neg
                             [ (Tcp.SYN, false); (Tcp.ACK, true) ])
                        ())));
             ]
           ~action:
             (Nat.Action.destination_nat
                ~port:(Nat.Port_range (1024, 2048))
                (Ipaddr.V4.of_string "192.168.0.9" |> Result.get_ok))
           Bos.Cmd.(
             of_string
               "--protocol tcp --destination-port 0-1024 !\n\
               \           --tcp-flags SYN,ACK ACK -j DNAT --to-destination\n\
               \           192.168.0.9:1024-2048"
             |> Result.get_ok);
      test_case "udp match with multiport" `Quick
      @@ test_nat
           ~matches:
             [
               Nat.Match.(
                 protocol
                   (`Udp
                     (Udp.v
                        ~sport:
                          (Nat.Negatable.V
                             [ Nat.Port_range (40000, 50000); Nat.Port 42 ])
                        ~dport:
                          (Nat.Negatable.V
                             [
                               Nat.Port_range (0, 1024);
                               Nat.Port 8080;
                               Nat.Port 8081;
                             ])
                        ())));
             ]
           ~action:
             (Nat.Action.destination_nat ~port:(Nat.Port 53)
                (Ipaddr.V4.of_string "192.168.0.9" |> Result.get_ok))
           Bos.Cmd.(
             of_string
               "--protocol udp -m multiport --source-ports 40000-50000,42\n\
               \           --destination-ports 0-1024,8080,8081 -j DNAT \
                --to-destination\n\
               \           192.168.0.9:53"
             |> Result.get_ok);
    ]

let () =
  let open Alcotest in
  run "Nat" [ ("command_generation", command_generation_tests) ]
