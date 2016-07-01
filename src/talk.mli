
type t =
  | Ack

val talk : target:string -> t -> unit Lwt.t
val select : t -> string
