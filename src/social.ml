open Prelude
open Containers

module J = Yojson.Basic.Util
type json = Yojson.Basic.json

(* Data for contacts *)
type contact = {
  lastSeen: float;
  to_tell: (string   (* from       *)
            * string (* on channel *)
            * string (* message    *)
           ) list;
}

exception Bad_json

let contact_of_json (json: json): contact option =
  let member k =
    match J.member k json with
    | `Null -> raise Bad_json
    | v -> v in
  try
    { lastSeen = member "lastSeen" |> J.to_float;
      to_tell =
        member "to_tell"
        |> J.convert_each (fun j ->
          match J.convert_each J.to_string j with
          | [from_; on_channel; msg] -> (from_, on_channel, msg)
          | _ -> raise Bad_json)
    } |> some
  with Bad_json | J.Type_error (_, _) -> None

let json_of_contact (c: contact): json =
  `Assoc [
    "lastSeen", `Float c.lastSeen;
    "to_tell", `List (
      List.map (fun (from_, on_channel, msg) ->
        `List [`String from_; `String on_channel; `String msg]
      ) c.to_tell
    )
  ]

(* Contacts db *)

type t = contact StrMap.t
let db_filename = "socialdb.json"

let read_db (): t =
  match Yojson.Basic.from_file db_filename with
  | `Assoc l ->
    List.to_seq l
    |> Sequence.filter_map (fun (k, j) ->
      Option.(contact_of_json j >>= fun c -> Some (k, c)))
    |> StrMap.of_seq
  | _ -> StrMap.empty

let write_db (db: t) =
  let json = `Assoc (
    StrMap.to_seq db
    |> Sequence.map (fun (k, c) -> (k, json_of_contact c))
    |> List.of_seq
  ) in
  Yojson.Basic.to_file db_filename json

let contacts = ref (read_db ())

let is_contact nick = StrMap.mem nick !contacts

let set_data nick contact =
  contacts := StrMap.add nick contact !contacts;
  write_db !contacts

let new_contact nick =
  if not (is_contact nick) then
    set_data nick {
      lastSeen = Unix.time ();
      to_tell = []
    }

let data nick =
  if not @@ is_contact nick then new_contact nick;
  StrMap.find nick !contacts

(* Callbacks: runtime part *)

(* Update lastSeen *)
let () = Signal.on' Core.privmsg (fun msg ->
  set_data msg.Core.nick
    {(data msg.Core.nick) with lastSeen = Unix.time ()};
  Lwt.return ())

(* Tell messages *)
let () = Signal.on' Core.messages (fun msg ->
  Core.connection >>= fun connection ->
  let nick =
    match msg.Msg.command with
    | Msg.JOIN (_, _) | Msg.PRIVMSG (_, _) ->
      some @@ get_nick @@ get_some msg.Msg.prefix
    | Msg.NICK newnick ->
      Some newnick
    | _ -> None
  in
  match nick with
  | None -> Lwt.return ()
  | Some nick ->
    let contact = data nick in
    let to_tell = contact.to_tell |> List.rev in
    set_data nick {contact with to_tell = []};
    Lwt_list.iter_s (fun (author, channel, message) ->
      Irc.send_privmsg ~connection ~target:channel
        ~message:(Printf.sprintf "%s: (from %s): %s" nick author message))
      to_tell)