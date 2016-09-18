open Prelude

type key = private string
type value =
  | StrList of string list
  | Int of int
type factoid = {key: key; value: value}
type t = factoid StrMap.t

val key_of_string : string -> key option

type op =
  | Get of key
  | Set of factoid
  | Append of factoid
  | Incr of key
  | Decr of key
  | Reload

val parse_op : string -> op option

val empty : t

val get : key -> t -> value
val set : factoid -> t -> t
val append : factoid -> t -> t
val incr : key -> t -> int option * t
val decr : key -> t -> int option * t

val read_file : file:string -> t Lwt.t
val write_file : file:string -> t -> unit Lwt.t

(* stateful *)

module St : sig
  val get : key -> value
  val set : factoid -> unit Lwt.t
  val append : factoid -> unit Lwt.t
  val incr : key -> int option Lwt.t
  val decr : key -> int option Lwt.t
  val reload : unit -> unit Lwt.t
end
