val connection : Irc_client_lwt.connection_t Lwt.t

val init : unit Lwt.t

val messages : Irc_message.t Signal.t
val messages_stream : Irc_message.t Lwt_stream.t

type privmsg = {
  nick: string;
  channel: string;
  message: string;
}

val privmsg_of_msg : Irc_message.t -> privmsg option

val privmsg : privmsg Signal.t
val privmsg_stream : privmsg Lwt_stream.t

(* Must be fed to the Lwt scheduler *)
val main : unit Lwt.t
