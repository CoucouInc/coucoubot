open Prelude
open Containers
open Lwt.Infix

type key = string
type value = string
type factoid = {key: key; value: value list}
type t = factoid StrMap.t
type json = Yojson.Safe.json

type op =
  | Get of key
  | Set of factoid
  | Append of factoid
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
  {key; value=[value]}

let re_set = Str.regexp "^!\\([^!=+]*\\)\\=\\(.*\\)$"
let re_append = Str.regexp "^!\\([^!=+]*\\)\\+=\\(.*\\)$"
let re_get = Str.regexp "^!\\([^!=+]*\\)$"
let re_reload = Str.regexp "^![ ]*reload[ ]*$"

let parse_op msg : op option =
  let open Option in
  let mk_get k = Get (mk_key k) in
  let mk_set k v = Set (mk_factoid k v) in
  let mk_append k v = Append (mk_factoid k v) in
  (re_match2 mk_append re_append msg)
  <+>
  (re_match2 mk_set re_set msg)
  <+>
  (re_match1 mk_get re_get msg)
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

let as_str_list (j:json) : string list = match j with
    | `List l -> List.map as_str l
    | _ -> raise Could_not_parse

let get key (fcs:t) : value list =
  try (StrMap.find key fcs).value
  with Not_found -> []

let set ({key;_} as f) (fcs:t): t =
  StrMap.add key f fcs

let append {key;value} (fcs:t): t =
  let value' =
    try (StrMap.find key fcs).value
    with Not_found -> [] in
  let value' = value @ value' in
  StrMap.add key {key; value = value'} fcs

(* parsing/outputting the factoids json *)
let factoids_of_json (json: json): t option =
  try
    begin match json with
      | `Assoc l ->
        List.fold_left
          (fun acc (k, v) ->
             let v = as_str_list v in
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
         let value = List.map (fun s->`String s) value in
         (key, `List value) :: acc)
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
