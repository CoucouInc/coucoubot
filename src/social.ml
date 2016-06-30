open Prelude

(* Data for contacts *)
type contact = {
  mutable lastSeen: float;
  mutable to_tell: (string   (* from       *)
                    * string (* on channel *)
                    * string (* message    *)
                   ) list;
}

let contacts = Hashtbl.create 20

let isContact = Hashtbl.mem contacts

let newContact nick =
  if not (isContact nick) then
    Hashtbl.add contacts nick
      { lastSeen = Unix.time();
        to_tell = [];
      }

let data nick =
  if not @@ isContact nick then newContact nick;
  Hashtbl.find contacts nick

(* Callbacks: runtime part *)

(* Update lastSeen *)
let () = Signal.on' Core.privmsg (fun msg ->
  (data msg.Core.nick).lastSeen <- Unix.time ();
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
    let to_tell = (data nick).to_tell |> List.rev in
    (data nick).to_tell <- [];
    Lwt_list.iter_s (fun (author, channel, message) ->
      Irc.send_privmsg ~connection ~target:channel
        ~message:(Printf.sprintf "%s: (from %s): %s" nick author message))
      to_tell)

