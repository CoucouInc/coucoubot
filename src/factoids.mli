open Prelude

type key = private string
type value = string
type factoid = {key: key; value: value list}
type t = factoid StrMap.t

val key_of_string : string -> key option

type op =
  | Get of key
  | Set of factoid
  | Append of factoid
  | Reload

val parse_op : string -> op option

val empty : t

val get : key -> t -> value list (* possibly empty *)
val set : factoid -> t -> t
val append : factoid -> t -> t

val read_file : file:string -> t Lwt.t
val write_file : file:string -> t -> unit Lwt.t

(* stateful *)

module St : sig
  val get : key -> value list
  val set : factoid -> unit Lwt.t
  val append : factoid -> unit Lwt.t
  val reload : unit -> unit Lwt.t
end
