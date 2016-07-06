
type t =
  | Ack
  | Err

val talk : target:string -> t -> unit Lwt.t
val select : t -> string
