open Prelude

(* À étendre *)

type t =
  | Ack

let ack = [
  "Bien reçu";
  "OK.";
  "done.";
]

let talk_base = function
  | Ack -> ack

(******************************************************************************)

let talk ~target ty =
  let msgs = talk_base ty in
  Core.connection >>= fun connection ->
  Irc.send_privmsg ~connection ~target ~message:(select msgs)

let select ty =
  talk_base ty |> select
