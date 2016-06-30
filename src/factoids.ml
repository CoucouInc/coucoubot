open Prelude
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
  | Save

let key_of_string s =
  let k = s |> String.trim |> CCString.lowercase_ascii in
  if String.index k ' ' >= 0 then None
  else Some k

let mk_factoid key value = {key; value=[value]}

let parse_op msg : op option =
  let mk_get k = Some (Get k) in
  let mk_set k v = Some (Set (mk_factoid k v)) in
  let mk_append k v = Some (Append (mk_factoid k v)) in
  (try Scanf.sscanf msg "! %s = %s" mk_set with _ -> None)
  <+>
  (try Scanf.sscanf msg "! %s += %s" mk_append with _ -> None)
  <+>
  (try Scanf.sscanf msg "! %s" mk_get with _ -> None)
  <+>
  (try Scanf.sscanf msg "! reload" (Some Reload) with _ -> None)
  <+>
  (try Scanf.sscanf msg "! save" (Some Save) with _ -> None)

(* read the json file *)
let read_json (file:string) : json option Lwt.t =
  Lwt_io.with_file ~mode:Lwt_io.input file
    (fun ic ->
       Lwt_io.read ic >|= fun s ->
       try Yojson.Safe.from_string s |> some
       with _ -> None)

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
      | _ -> raise Exit
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
  let cmd =
    Printf.sprintf "mv '%s' '%s'" (String.escaped file') (String.escaped file)
  in
  Lwt_log.ign_debug_f "move `%s` into `%s` with `%s`" file' file cmd;
  Lwt_unix.system cmd >|= fun _ -> ()

(* state *)

module St = struct
  let state = ref empty

  let get k = get k !state
  let set f = state := set f !state
  let append f = state := append f !state

  let reload () =
    read_file ~file:Config.factoids_file >|= fun fs ->
    state := fs;
    ()

  let save () =
    write_file ~file:Config.factoids_file !state

end
