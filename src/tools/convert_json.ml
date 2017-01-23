
module C = Calculon
open C.Prelude

module J = Yojson.Safe.Util
type json = Yojson.Safe.json

module StrMap = CCMap.Make(String)

type to_tell = {
  from: string;
  on_channel: string;
  msg: string;
  tell_after: float option; (** optional; not before this deadline (UTC) *)
}


(* Data for contacts *)
type contact = {
  last_seen: float;
  to_tell: to_tell list;
  coucous : int;
}

exception Bad_json

let contact_of_json (json: json): contact option =
  let member k =
    match J.member k json with
    | `Null -> raise Bad_json
    | v -> v in
  try
    { last_seen = member "lastSeen" |> J.to_float;
      to_tell =
        member "to_tell"
        |> J.convert_each (fun j ->
          match J.convert_each J.to_string j with
            | [from; on_channel; msg] -> {from; on_channel; msg; tell_after=None}
            | [from; on_channel; msg; tell_after] ->
              let tell_after = Some (float_of_string tell_after) in
              {from; on_channel; msg; tell_after;}
          | _ -> raise Bad_json);
      coucous = member "coucous" |> J.to_int_option |? 0
    } |> some
  with Bad_json | J.Type_error (_, _) -> None

let json_of_social (c: contact): json =
  `Assoc [
    "lastSeen", `Float c.last_seen;
    "to_tell", `List (
      List.map (fun {from; on_channel; msg; tell_after} ->
        let last = match tell_after with
          | None -> []
          | Some f -> [`String (string_of_float f)]
        in
        `List ([`String from; `String on_channel; `String msg] @ last)
      ) c.to_tell
    );
  ]

let json_of_coucou (c: contact): json = `Int c.coucous

let json_of_map (f:contact->json) m : json =
  let l =
    StrMap.fold (fun k v acc -> (k, f v) :: acc) m []
  in
  `Assoc l

let contacts_of_file file : contact StrMap.t = 
  match Yojson.Safe.from_file file with
  | `Assoc l ->
    CCList.to_seq l
    |> Sequence.filter_map (fun (k, j) ->
      CCOpt.(contact_of_json j >>= fun c -> Some (k, c)))
    |> StrMap.of_seq
  | exception (Sys_error _) -> StrMap.empty
  | _ -> StrMap.empty

let factoids_of_file file : Calculon.Plugin_factoids.t =
  let j = Yojson.Safe.from_file file in
  match C.Plugin_factoids.factoids_of_json j with
    | Result.Ok l -> l
    | Result.Error msg ->
      failwith ("could not read factoids: " ^ msg)

let file_social = "social.json"
let file_factoids = "factoids.json"
let file_out = "state.json"

let () =
  let facts = factoids_of_file file_factoids in
  let contacts = contacts_of_file file_social in
  let j : json = `Assoc [
      "factoids", C.Plugin_factoids.json_of_factoids facts;
      "social", json_of_map json_of_social contacts;
      "coucou", json_of_map json_of_coucou contacts;
    ] in
  Yojson.Safe.to_file file_out j
