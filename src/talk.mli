
type t =
  | Ack
  | Error

val talk : target:string -> t -> unit Lwt.t
val select : t -> string
