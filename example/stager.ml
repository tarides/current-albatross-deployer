open Lwt.Syntax

type 'a staged = {
  live : 'a;
  (* value kept alive by the stager *)
  current : 'a; (* current value like if the stager didn't exist *)
}

module type S = sig
  type t

  val digest : t -> string

  val marshal : t -> string

  val unmarshal : string -> t

  val pp : t Fmt.t
end

module Op (M : S) = struct
  type nonrec t = Value of M.t

  module Key = struct
    type t = string

    let digest = Fun.id
  end

  module Value = Current.String
  module Outcome = M

  let pp f (k, _) = Fmt.pf f "Stager %s" k

  let id = "stager"

  let auto_cancel = true

  let latched = true

  let run (Value v) job _ _key =
    let* () = Current.Job.start ~level:Harmless job in
    Current.Job.log job "Stager update validated";
    Lwt.return_ok v
end

let stage_auto (type a) ~id (module M : S with type t = a) t v =
  let module Stager = Current_cache.Generic (Op (M)) in
  let live =
    let open Current.Syntax in
    Current.component "stager"
    |> let> v = v and> t = t in
       Stager.run (Value v) id t
  in
  { current = v; live }

module OpActivator = struct
  type t = No_context

  let id = "activator"

  let pp f (k, _) = Fmt.pf f "activator %s" k

  module Key = Current.String
  module Value = Current.String
  module Outcome = Current.String

  let auto_cancel = false

  let latched = true

  let run No_context job _k v =
    let* () = Current.Job.start ~level:Dangerous job in
    Current.Job.log job "Stager update !";
    Lwt.return_ok v
end

module Activator = Current_cache.Generic (OpActivator)

type activation = string

let activate ~id v =
  let open Current.Syntax in
  Current.component "activator for %s" id
  |> let> () = v in
     let k = Random.int 1_000_000_000 |> string_of_int in
     Activator.run No_context id k