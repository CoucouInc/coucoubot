open Prelude

(* On se connecte au canal *)
let () = Lwt.async (fun () ->
  Core.connection >>= fun connection ->
  Irc.send_join ~connection ~channel:Config.channel)

(******************************************************************************)

(* Logging *)
let () = Signal.on' Core.messages (fun msg ->
  Lwt_io.printf "Log: %s\n%!" (Irc_message.to_string msg))

(* Commandes + Factoids

   Commands have priority over factoids, and override them
*)
let () = Signal.on' Core.privmsg (fun msg ->
  let target = Core.reply_to msg in
  Core.connection >>= fun connection ->
  Lwt_list.fold_left_s (fun cmd_matched (cmd, f) ->
    if Str.string_match cmd msg.Core.message 0 then
      let after = Str.string_after msg.Core.message (Str.match_end ()) in
      f connection target msg.Core.nick after >>= fun () ->
      Lwt.return true
    else Lwt.return cmd_matched
  ) false Commands.commands >>= fun cmd_matched ->

  if not cmd_matched then
    match Factoids.parse_op msg.Core.message with
    | None -> Lwt.return_unit
    | Some (Factoids.Get k) ->
      begin match Factoids.St.get k with
        | [] -> Lwt.return_unit
        | [message] ->
          Irc.send_privmsg ~connection ~target ~message
        | l ->
          let message = DistribM.uniform l |> DistribM.run in
          Irc.send_privmsg ~connection ~target ~message
      end
    | Some (Factoids.Set f) ->
      Factoids.St.set f >>= fun () -> Talk.(talk ~target Ack)
    | Some (Factoids.Append f) ->
      Factoids.St.append f >>= fun () -> Talk.(talk ~target Ack)
    | Some Factoids.Reload ->
      Factoids.St.reload () >>= fun () -> Talk.(talk ~target Ack)
  else Lwt.return ()
)

(* on_join, on_nick *)
let () = Signal.on' Core.messages (fun msg ->
  Core.connection >>= fun _conn ->
  match msg.Msg.command with
  | Msg.JOIN (_channels, _) ->
    Lwt.return ()
  | Msg.NICK _newnick ->
    Lwt.return ()
  | _ -> Lwt.return ())

(******************************************************************************)

(* Main loop *)
let () =
  Lwt_main.run Core.main
