type key = private string
type value = string
type factoid = {key: key; value: value}

val key_of_string : string -> key option

type op =
  | Read of key
  | Write of factoid

val parse_op : string -> op option
val read : key -> value
val write : factoid -> unit

