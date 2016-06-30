open Prelude

let _connection =
  let open Config in
  Irc.connect_by_name 
    ~username ~realname ~nick
    ~server ~port
    ()
  >>= (function
    | Some c -> Lwt.return c
    | None -> failwith "Bad server address")

type privmsg = {
  nick: string;
  channel: string;
  message: string;
}

let privmsg_of_msg msg =
  match msg.Msg.command with
  | Msg.PRIVMSG (channel, message) ->
    Some
      { nick = get_some msg.Msg.prefix |> get_nick;
        channel;
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
  _connection >>= fun conn ->
  Lwt.return conn
