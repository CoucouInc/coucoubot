open Prelude

type t =
  | Ack

val talk : string (* channel *) -> t -> unit Lwt.t
val select : t -> string
