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
  coucous: int; (* coucou counter *)
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
          | _ -> raise Bad_json);
      coucous = member "coucous" |> J.to_int_option |? 0
    } |> some
  with Bad_json | J.Type_error (_, _) -> None

let json_of_contact (c: contact): json =
  `Assoc [
    "lastSeen", `Float c.lastSeen;
    "to_tell", `List (
      List.map (fun (from_, on_channel, msg) ->
        `List [`String from_; `String on_channel; `String msg]
      ) c.to_tell
    );
    "coucous", `Int c.coucous
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
  | exception (Sys_error _) -> StrMap.empty
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

let set_data ?(force_sync = true) nick contact =
  contacts := StrMap.add nick contact !contacts;
  if force_sync then write_db !contacts

let sync () = write_db !contacts

let new_contact nick =
  if not (is_contact nick) then
    set_data nick {
      lastSeen = Unix.time ();
      to_tell = [];
      coucous = 0;
    }

let data nick =
  if not @@ is_contact nick then new_contact nick;
  StrMap.find nick !contacts

(* Callbacks: runtime part *)

(* Update lastSeen *)
let () = Signal.on' Core.privmsg (fun msg ->
  set_data ~force_sync:false msg.Core.nick
    {(data msg.Core.nick) with lastSeen = Unix.time ()};
  Lwt.return ())

(* Update coucous *)
let is_coucou msg =
  contains msg (Str.regexp "coucou")

let incr_coucous nick =
  let d = data nick in
  set_data ~force_sync:false nick {d with coucous = d.coucous + 1}

let () = Signal.on' Core.privmsg (fun msg ->
  if is_coucou msg.Core.message then
    incr_coucous msg.Core.nick;
  Lwt.return ()
)

(* Write the db to the disk periodically. 

   We do not update the on-disk db each time lastSeen is updated (i.e. each time
   someone talks), as it's not a big deal if we lose some data about lastSeen in
   case of a crash.
*)
let () =
  let rec loop () =
    Lwt_unix.sleep 30. >>= fun () ->
    sync (); loop () in
  Lwt.async loop

(* Tell messages *)
let () = Signal.on' Core.messages (fun msg ->
  Core.connection >>= fun connection ->
  let nick =
    match msg.Msg.command with
    | Msg.JOIN (_, _) | Msg.PRIVMSG (_, _) ->
      some @@ get_nick @@ Option.get_exn msg.Msg.prefix
    | Msg.NICK newnick ->
      Some newnick
    | _ -> None
  in
  match nick with
  | None -> Lwt.return ()
  | Some nick ->
    let contact = data nick in
    let to_tell = contact.to_tell |> List.rev in
    if to_tell <> [] then set_data nick {contact with to_tell = []};
    Lwt_list.iter_s (fun (author, channel, message) ->
      if is_coucou message then incr_coucous Config.nick;
      Irc.send_privmsg ~connection ~target:channel
        ~message:(Printf.sprintf "%s: (from %s): %s" nick author message))
      to_tell)
