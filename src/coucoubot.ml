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

(* Factoids *)
let () =
  Signal.on' Core.privmsg (fun msg ->
    Core.connection >>= fun connection ->
    match Factoids.parse_op msg.Core.message with
    | None -> Lwt.return_unit
    | Some (Factoids.Get k) ->
      begin match Factoids.St.get k with
        | [] -> Lwt.return ()
        | [message] ->
          Irc.send_privmsg ~connection ~target:Config.channel ~message
        | l ->
          let message = DistribM.uniform l |> DistribM.run in
          Irc.send_privmsg ~connection ~target:Config.channel ~message
      end
    | Some (Factoids.Set f) ->
      Factoids.St.set f;
      Lwt.return_unit
    | Some (Factoids.Append f) ->
      Factoids.St.append f;
      Lwt.return_unit
    | Some Factoids.Reload -> Factoids.St.reload ()
    | Some Factoids.Save -> Factoids.St.save ()
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
