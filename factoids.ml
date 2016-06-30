open Prelude
open Lwt.Infix
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


module Sync_mem = Sync.Make(Memory)
module Search_mem = Git.Search.Make(Memory)

let repo_uri = Git.Gri.of_string "https://github.com/CoucouInc/factoids.git"
let file = "factoids.json"

let fetch_factoids_data store =
  Memory.read_reference_exn store Git.Reference.master >>= fun head ->
  Search_mem.find store head (`Commit (`Path [file])) >>= function
  | None -> failwith (file ^ " not found")
  | Some sha -> Memory.read_exn store sha >>= function
    | Git.Value.Blob b -> Lwt.return (Git.Blob.to_raw b)
    | _ -> failwith "not a valid path"

let parse_factoids data =
  let json = Yojson.Safe.from_string data in
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

(* state of the module *)

let store : Memory.t Lwt.t =
  Memory.create () >>= fun store ->
  Sync_mem.clone store ~checkout:false repo_uri >>= fun _ ->
  Lwt.return store

let factoids : factoid list Lwt.t =
  store >>= fetch_factoids_data >|= fun data ->
  parse_factoids data |? []

(* operations *)

let read _ = failwith "todo"
let write _ = failwith "todo"
