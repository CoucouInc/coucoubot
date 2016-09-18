open Prelude
open Containers
open Lwt.Infix

type key = string
type value =
  | StrList of string list
  | Int of int
type factoid = {key: key; value: value}
type t = factoid StrMap.t
type json = Yojson.Safe.json

type op =
  | Get of key
  | Set of factoid
  | Append of factoid
  | Incr of key
  | Decr of key
  | Reload

let key_of_string s =
  let k = s |> String.trim |> CCString.lowercase_ascii in
  if String.contains k ' ' then None
  else Some k

let mk_key key =
  match key_of_string key with
  | None -> invalid_arg "mk_key"
  | Some key -> key

let mk_factoid key value =
  let key = mk_key key in
  let value = String.trim value in
  try {key; value = Int (int_of_string value)}
  with Failure _ -> {key; value = StrList [value]}

let re_set = Str.regexp "^!\\([^!=+-]*\\)\\=\\(.*\\)$"
let re_append = Str.regexp "^!\\([^!=+-]*\\)\\+=\\(.*\\)$"
let re_get = Str.regexp "^!\\([^!=+-]*\\)$"
let re_reload = Str.regexp "^![ ]*reload[ ]*$"
let re_incr = Str.regexp "^!\\([^!=+-]*\\)\\+\\+[ ]*$"
let re_decr = Str.regexp "^!\\([^!=+-]*\\)--[ ]*$"

let parse_op msg : op option =
  let open Option in
  let mk_get k = Get (mk_key k) in
  let mk_set k v = Set (mk_factoid k v) in
  let mk_append k v = Append (mk_factoid k v) in
  let mk_incr k = Incr (mk_key k) in
  let mk_decr k = Decr (mk_key k) in
  (re_match2 mk_append re_append msg)
  <+>
  (re_match2 mk_set re_set msg)
  <+>
  (re_match1 mk_get re_get msg)
  <+>
  (re_match1 mk_incr re_incr msg)
  <+>
  (re_match1 mk_decr re_decr msg)
  <+>
  (if Str.string_match re_reload msg 0 then Some Reload else None)

(* read the json file *)
let read_json (file:string) : json option Lwt.t =
  Lwt.catch
    (fun () ->
       Lwt_io.with_file ~mode:Lwt_io.input file
         (fun ic ->
            Lwt_io.read ic >|= fun s ->
            try Yojson.Safe.from_string s |> some
            with _ -> None))
    (fun _ -> Lwt.return_none)

exception Could_not_parse

let as_str (j:json) : string = match j with
  | `String s -> s
  | _ -> raise Could_not_parse

let as_value (j: json) : value = match j with
  | `List l -> StrList (List.map as_str l)
  | `Int i -> Int i
  | _ -> raise Could_not_parse

let get key (fcs:t) : value =
  try (StrMap.find key fcs).value
  with Not_found -> StrList []

let set ({key;_} as f) (fcs:t): t =
  StrMap.add key f fcs

let append {key;value} (fcs:t): t =
  let value' =
    match (StrMap.find key fcs).value, value with
    | Int i, Int j -> Int (i+j)
    | StrList l, StrList l' -> StrList (l @ l')
    | StrList l, Int j -> StrList (string_of_int j :: l)
    | Int i, StrList l -> StrList (string_of_int i :: l)
    | exception Not_found -> value
  in
  StrMap.add key {key; value = value'} fcs

let incr key (fcs:t): int option * t =
  let value = try (StrMap.find key fcs).value with Not_found -> Int 0 in
  match value with
  | Int i ->
    let count = i + 1 in
    (Some count, StrMap.add key {key; value = Int count} fcs)
  | _ -> (None, fcs)

let decr key (fcs:t): int option * t =
  let value = try (StrMap.find key fcs).value with Not_found -> Int 0 in
  match value with
  | Int i ->
    let count = i - 1 in
    (Some count, StrMap.add key {key; value = Int count} fcs)
  | _ -> (None, fcs)

(* parsing/outputting the factoids json *)
let factoids_of_json (json: json): t option =
  try
    begin match json with
      | `Assoc l ->
        List.fold_left
          (fun acc (k, v) ->
             let v = as_value v in
             let key = match key_of_string k with
               | Some k -> k
               | None -> raise Could_not_parse
             in
             append {key;value=v} acc
          )
          StrMap.empty l
      | _ -> raise Could_not_parse
    end
    |> some
  with Could_not_parse -> None

let json_of_factoids (factoids: t): json =
  let l =
    StrMap.fold
      (fun _ {key; value} acc ->
         let jvalue = match value with
           | StrList l -> `List (List.map (fun s -> `String s) l)
           | Int i -> `Int i in
         (key, jvalue) :: acc)
      factoids
      []
  in
  `Assoc l

let dump_factoids (factoids: t): string =
  json_of_factoids factoids |> Yojson.Safe.to_string

(* operations *)

let empty = StrMap.empty

let read_file ~(file:string) : t Lwt.t =
  read_json file >|= function
  | None -> StrMap.empty
  | Some data -> factoids_of_json data |? StrMap.empty

let write_file ~file (fs: t) : unit Lwt.t =
  let file' = file ^ ".tmp" in
  let s = dump_factoids fs in
  Lwt_io.with_file ~mode:Lwt_io.output file'
    (fun oc ->
       Lwt_io.write oc s >>= fun () ->
       Lwt_io.flush oc)
  >>= fun () ->
  Sys.rename file' file;
  Lwt.return ()

(* state *)

module St = struct
  let state = ref empty

  let get k = get k !state

  let save_ () =
    write_file ~file:Config.factoids_file !state

  let set f =
    state := set f !state;
    save_ ()

  let append f =
    state := append f !state;
    save_ ()

  let incr k =
    let (count, state') = incr k !state in
    state := state';
    save_ () >|= fun _ -> count

  let decr k =
    let (count, state') = decr k !state in
    state := state';
    save_ () >|= fun _ -> count

  let reload () =
    read_file ~file:Config.factoids_file >|= fun fs ->
    state := fs;
    ()

  (* load at init time *)
  let () =
    Lwt.on_success Core.init
      (fun () ->
         print_endline "load initial factoids file...";
         Lwt.async reload)
end
