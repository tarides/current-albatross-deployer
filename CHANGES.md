# dev

- Support specifying the pinned CPU in config

# 1.0.0

- The initial release comprises of:
  - library `iptables`: an OCaml API to manage iptables nat redirections
  - library `iptables_daemon_api`: an API for communicating between the IP manager
    and the ocurrent plugin.
  - library `iptables_client`: a socket client for the IP manager API.
  - executable `current-iptables-daemon`: a daemon in charge of managing IPs and
    nat redirections, providing an endpoint for the `iptables_daemon_api`
  - executable `current-albatross-deployer`: an ocurrent plugin to deploy unikernels
    using _albatross_ and _current-iptables-daemon_
  - executable `iptables-cli`: a command line interface to inspect the IP manager state.
