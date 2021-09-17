  (* Required to be able to stage a Current.t *)
  module type S = sig
    type t

    val digest : t -> string

    val marshal : t -> string

    val unmarshal : string -> t

    val pp : t Fmt.t
  end

  type 'a staged = {
    live : 'a;
    (* value kept alive by the stager *)
    current : 'a; (* current value like if the stager didn't exist *)
  }

  type activation

  (* promote the stages when the current has a value *)
  val activate : id:string -> unit Current.t -> activation Current.t

  val stage_auto :
    id:string ->
    (module S with type t = 'a) ->
    activation Current.t ->
    'a Current.t ->
    'a Current.t staged