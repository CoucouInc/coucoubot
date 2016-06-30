open Prelude

type key = private string
type value = string
type factoid = {key: key; value: value list}
type factoids = factoid StrMap.t

val key_of_string : string -> key option

type op =
  | Read of key
  | Write of factoid
  | Append of factoid

val parse_op : string -> op option
val read : key -> value list option Lwt.t
val write : factoid -> unit Lwt.t
val append : factoid -> unit Lwt.t

