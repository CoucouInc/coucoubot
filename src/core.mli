
(** {1 Core IRC state} *)

module Msg = Irc_message

type connection = Irc_client_lwt.connection_t

type privmsg = {
  nick: string; (* author *)
  to_: string; (* target *)
  message: string;
}

val is_chan : string -> bool
(** Is this a valid chan name? *)

val reply_to : privmsg -> string
(** find whom to reply to *)

val privmsg_of_msg : Msg.t -> privmsg option

val string_of_privmsg : privmsg -> string

module type S = sig
  val connection : connection Lwt.t

  val init : unit Lwt.t
  val exit : unit Lwt.t

  val send_exit : unit -> unit

  val messages : Msg.t Signal.t

  val privmsg : privmsg Signal.t

  val line_cut_threshold : int ref
  (** Above [!line_cut_threshold], multi-line messages are cut with "..." *)

  val send_privmsg_l :
    target:string -> messages:string list -> unit Lwt.t

  val send_privmsg :
    target:string -> message:string -> unit Lwt.t
  (** Helper for sending messages, splitting lines, etc. *)

  val send_notice_l :
    target:string -> messages:string list -> unit Lwt.t

  val send_notice :
    target:string -> message:string -> unit Lwt.t
  (** Helper for sending notices, splitting lines, etc. *)

  val send_join : channel:string -> unit Lwt.t

  val talk : target:string -> Talk.t -> unit Lwt.t

  val run : unit Lwt.t
  (** Feed to {!Lwt_main.run} *)
end

type t = (module S)

val of_conn : connection Lwt.t -> t

val of_config : Config.t -> t
