open Prelude
open Lwt.Infix
open Git
open Git_unix

type key = string
type value = string
type factoid = {key: key; value: value list}
type factoids = factoid StrMap.t
type json = Yojson.Safe.json

type op =
  | Read of key
  | Write of factoid
  | Append of factoid

let key_of_string s =
  let k = s |> String.trim |> String.lowercase in
  if String.index k ' ' >= 0 then None
  else Some k

let mk_factoid key value = {key; value=[value]}

let parse_op msg : op option =
  let mk_read k = Some (Read k) in
  let mk_write k v = Some (Write (mk_factoid k v)) in
  let mk_append k v = Some (Append (mk_factoid k v)) in
  (try Scanf.sscanf msg "! %s = %s" mk_write with _ -> None)
  <+>
  (try Scanf.sscanf msg "! %s += %s" mk_append with _ -> None)
  <+>
  (try Scanf.sscanf msg "! %s" mk_read with _ -> None)

module Store = FS
module MySearch = Git.Search.Make(Store)

let local_repo_dir = "factoids"
let file = "factoids.json"
let coucoubot_git : Git.User.t = User.{
  name = "coucoubot";
  email = "coucoubot@coucouinc.swag";
  date = (0L, None);
}

(* Reading/Writing from/to a git store *)
let read_factoids_data store =
  Store.read_reference store Reference.master >>= function
  | None -> Lwt.return None
  | Some head ->
    MySearch.find store head (`Commit (`Path [file])) >>= function
    | None -> failwith (file ^ " not found")
    | Some sha -> Store.read_exn store sha >>= function
      | Value.Blob b -> Lwt.return (Some (Blob.to_raw b))
      | _ -> failwith "not a valid path"

let write_factoids_data store msg data =
  Store.read_reference store Reference.master >>= fun head_opt ->
  begin match head_opt with
    | None -> Lwt.return ([], [])
    | Some head_hash ->
      Store.read_exn store head_hash >>= function
      | Value.Commit head ->
        Store.read_exn store (Hash.of_tree head.Commit.tree) >>= (function
            | Value.Tree old_tree ->
              Lwt.return (old_tree, [Hash.to_commit head_hash])
            | _ -> failwith "not a valid tree")
      | _ -> failwith "not a valid commit"
  end >>= fun (old_tree, parents) ->
  let new_blob = Value.Blob (Blob.of_raw data) in
  Store.write store new_blob >>= fun new_blob_hash ->
  let new_tree = Value.Tree (
      Tree.{perm = `Normal; name = file; node = new_blob_hash} ::
      List.filter (fun {Tree.name; _} -> name <> file) old_tree
    ) in
  Store.write store new_tree >>= fun new_tree_hash ->
  let new_commit = Value.Commit (Commit.{
      tree = Hash.to_tree new_tree_hash;
      parents;
      author = coucoubot_git;
      committer = coucoubot_git;
      message = msg;
    }) in
  Store.write store new_commit >>= fun new_commit_hash ->
  Store.write_reference store Reference.master new_commit_hash

exception Could_not_parse

let as_str (j:json) : string = match j with
  | `String s -> s
  | _ -> raise Could_not_parse

let as_str_list (j:json) : string list = match j with
    | `List l -> List.map as_str l
    | _ -> raise Could_not_parse

let add_factoid {key;value} (fcs:factoids): factoids =
  let value' =
    try (StrMap.find key fcs).value
    with Not_found -> [] in
  let value' = value @ value' in
  StrMap.add key {key; value = value'} fcs

(* parsing/outputting the factoids json *)
let factoids_of_json (json: json): factoids option =
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
             add_factoid {key;value=v} acc
          )
          StrMap.empty l
      | _ -> raise Exit
    end
    |> some
  with Could_not_parse -> None

let json_of_factoids (factoids: factoids): json =
  let l =
    StrMap.fold
      (fun _ {key; value} acc ->
         let value = List.map (fun s->`String s) value in
         (key, `List value) :: acc)
      factoids
      []
  in
  `Assoc l

let parse_factoids (data: string): factoids option =
  Yojson.Safe.from_string data |> factoids_of_json

let dump_factoids (factoids: factoids): string =
  json_of_factoids factoids |> Yojson.Safe.to_string

(* state of the module *)

let store : Store.t Lwt.t =
  Store.create ~root:local_repo_dir () >>= fun store ->
  Lwt.return store

(* operations *)

let factoids (store: Store.t) : factoids Lwt.t =
  read_factoids_data store >|= function
  | None -> StrMap.empty
  | Some data -> parse_factoids data |? StrMap.empty

let write_factoids msg (fs: factoids) : unit Lwt.t =
  store >>= fun store ->
  write_factoids_data store msg (dump_factoids fs)

let read (key: key): value list option Lwt.t =
  store >>= fun store ->
  factoids store >>= fun fs ->
  try
    StrMap.find key fs
    |> fun {value; _} -> Lwt.return (Some value)
  with Not_found -> Lwt.return None

let write (f: factoid): unit Lwt.t =
  store >>= fun store ->
  factoids store >>= fun fs ->
  let fs' = add_factoid f fs in
  write_factoids (Printf.sprintf "add/update factoid %s" f.key) fs'

let append (f: factoid): unit Lwt.t =
  store >>= fun store ->
  factoids store >>= fun fs ->
  let fs' = add_factoid f fs in
  write_factoids (Printf.sprintf "add/update factoid %s" f.key) fs'
