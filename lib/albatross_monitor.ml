module Info = struct
  type t = { status : [ `Running | `Exited ] }

  let status t = t.status
end

let monitor ?(poll_rate = 10.)
    (deployment : Albatross_deploy.Deployed.t Current.t) =
  let open Current.Syntax in
  Current.component "Monitor status"
  |> let> deployment = deployment in
     let config = deployment.config in
     let mode = deployment.mode in
     let module C = (val Client.client_of_mode mode) in
     let name = config.id in
     let vmm_name = Vmm_core.Name.of_string name |> Result.get_ok in
     let ip = config.ip in
     let status = ref `Running in
     let set_and_refresh value refresh =
       if !status <> value then (
         status := value;
         refresh ())
     in
     let read () = Lwt_result.return { Info.status = !status } in
     let watch refresh =
       let open Lwt.Syntax in
       let stop = Lwt_condition.create () in
       let rec loop () =
         let* unikernel_info = C.show_unikernel vmm_name in
         match unikernel_info with
         | Ok [ (_, _info) ] ->
             set_and_refresh `Running refresh;
             let* () = Lwt_unix.sleep poll_rate in
             loop ()
         | Ok [] ->
             set_and_refresh `Exited refresh;
             let* () = Lwt_unix.sleep poll_rate in
             loop ()
         | _ ->
             (* on connection error, retry *)
             let* () = Lwt_unix.sleep poll_rate in
             loop ()
       in

       Lwt.async (fun () -> Lwt.pick [ Lwt_condition.wait stop; loop () ]);
       Lwt.return (fun () ->
           Lwt_condition.signal stop ();
           Lwt.return_unit)
     in
     let pp f = Fmt.pf f "Monitor deployment %s on %a" name Ipaddr.V4.pp ip in

     Current.Monitor.create ~read ~watch ~pp |> Current.Monitor.get

let is_running (info : Info.t Current.t) =
  let open Current.Syntax in
  Current.component "is running"
  |> let> info = info in
     let result =
       if info.status = `Running then Ok () else Error (`Msg "not running")
     in
     Current_incr.const (result, None)
