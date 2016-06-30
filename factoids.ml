open Prelude
open Lwt.Infix
open Git
open Git_unix

type key = string
type value = string
type factoid = {key: key; value: value}

type op =
  | Read of key
  | Write of factoid

let key_of_string s =
  let k = String.lowercase s in
  if contains k (Str.regexp " ") then None
  else Some k

let parse_op msg =
  failwith "todo"


module Store = Memory
module Sync_mem = Sync.Make(Store)
module Search_mem = Git.Search.Make(Store)

let repo_uri = Git.Gri.of_string "git@github.com:CoucouInc/factoids.git"
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

(* parsing/outputting the factoids json *)
let factoids_of_json (json: Yojson.Safe.json): factoid list option =
  try
    begin match json with
      | `Assoc l ->
        List.fold_left (fun acc (k, v) ->
            match v with
            | `String s ->
              (match key_of_string k with
               | Some key -> {key; value = s} :: acc
               | None -> acc)
            | _ -> raise Exit
          ) [] l
      | _ -> raise Exit end
    |> some
  with Exit -> None

let json_of_factoids (factoids: factoid list): Yojson.Safe.json =
  `Assoc (List.map (fun {key; value} -> (key, `String value)) factoids)

let parse_factoids (data: string): factoid list option =
  Yojson.Safe.from_string data |> factoids_of_json

let dump_factoids (factoids: factoid list): string =
  json_of_factoids factoids |> Yojson.Safe.to_string

(* state of the module *)

let store : Store.t Lwt.t =
  Store.create () >>= fun store ->
  Sync_mem.clone store ~checkout:false repo_uri >>= fun _ ->
  Lwt.return store

(* operations *)

let factoids (store: Store.t) : factoid list Lwt.t =
  read_factoids_data store >|= function
  | None -> []
  | Some data -> parse_factoids data |? []

let write_factoids msg (fs: factoid list) : unit Lwt.t =
  store >>= fun store ->
  write_factoids_data store msg (dump_factoids fs)

let read (key: key): value option Lwt.t =
  store >>= fun store ->
  Sync_mem.fetch store repo_uri >>= fun _ ->
  factoids store >>= fun fs ->
  try List.find (fun {key = key'; _} -> key' = key) fs
      |> fun {value; _} -> Lwt.return (Some value)
  with Not_found -> Lwt.return None

let write (f: factoid): unit Lwt.t =
  store >>= fun store ->
  factoids store >>= fun fs ->
  write_factoids (Printf.sprintf "add/update factoid %s" f.key)
    (f :: List.filter (fun f' -> f'.key <> f.key) fs) >>= fun () ->
  Sync_mem.push store ~branch:Reference.master repo_uri >>= fun _ ->
  Lwt.return ()
