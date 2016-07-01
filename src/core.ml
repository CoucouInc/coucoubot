open Prelude
open Containers

let _connection =
  let open Config in
  Irc.connect_by_name
    ~username ~realname ~nick
    ~server ~port
    ()
  >>= (function
    | Some c -> Lwt.return c
    | None -> failwith "Bad server address")

let init, send_init = Lwt.wait ()

type privmsg = {
  nick: string; (* author *)
  to_: string; (* target *)
  message: string;
}

let reply_to msg =
  if msg.to_.[0] = '#'
  then msg.to_ (* on a channel *)
  else msg.nick (* in pv *)

let privmsg_of_msg msg =
  match msg.Msg.command with
  | Msg.PRIVMSG (to_, message) ->
    Some
      { nick = Option.get_exn msg.Msg.prefix |> get_nick;
        to_;
        message }
  | _ -> None

let messages = Signal.create ()
let privmsg = Signal.filter_map messages privmsg_of_msg

let messages_stream, push_message = Lwt_stream.create ()
let privmsg_stream = Lwt_stream.filter_map privmsg_of_msg messages_stream

let main =
  _connection >>= fun connection ->
  Irc.listen ~connection
    ~callback:(fun _ msg_or_err ->
      match msg_or_err with
      | `Ok msg -> push_message (Some msg); Signal.send messages msg
      | `Error err -> Printf.eprintf "%s\n%!" err; Lwt.return ()) >>= fun () ->
  Irc.send_quit ~connection

let connection =
  Lwt.wakeup send_init ();
  _connection >>= fun conn ->
  Lwt.return conn
