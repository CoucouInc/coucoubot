open Prelude

(* On se connecte au canal *)
let () = Lwt.async (fun () ->
  Core.connection >>= fun connection ->
  Irc.send_join ~connection ~channel:Config.channel)

(******************************************************************************)

(* Logging *)
let () = Signal.on' Core.messages (fun msg ->
  Lwt_io.printf "Log: %s\n%!" (Irc_message.to_string msg))

(* Commandes *)
let () = Signal.on' Core.privmsg (fun msg ->
  Lwt_list.iter_s (fun (cmd, f) ->
    if Str.string_match cmd msg.Core.message 0 then
      let after = Str.string_after msg.Core.message (Str.match_end ()) in
      Core.connection >>= fun connection ->
      f connection msg.Core.channel msg.Core.nick after
    else Lwt.return ()
  ) Commands.commands)

(* on_join, on_nick *)
let () = Signal.on' Core.messages (fun msg ->
  Core.connection >>= fun conn ->
  match msg.Msg.command with
  | Msg.JOIN (channels, _) ->
    Lwt.return ()
  | Msg.NICK newnick ->
    Lwt.return ()
  | _ -> Lwt.return ())

(******************************************************************************)

(* Main loop *)
let () =
  Lwt_main.run Core.main
