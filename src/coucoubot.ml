open Lwt.Infix

let all_ : Plugin.t list = [
  Social.plugin;
  Factoids.plugin;
]

let main conf : unit Lwt.t =
  let core = Core.of_config conf in
  let (module C) = core in
  Plugin.init core conf all_ >>= fun (cmds, cleanup_cmds) ->
  Log.logf "got %d commands from %d plugins" (List.length cmds) (List.length all_);
  (* log incoming messages, apply commands to them *)
  Signal.on' C.messages
    (fun msg ->
       Log.logf "got message: %s" (Core.Msg.to_string msg);
       match Core.privmsg_of_msg msg with
         | None -> Lwt.return_unit
         | Some msg -> Command.run core cmds msg);
  (* connect to chan *)
  C.send_join ~channel:conf.Config.channel >>= fun () ->
  (* done, wait for exit *)
  C.exit >>= cleanup_cmds

let () =
  let conf = Config.of_argv () in
  main conf |> Lwt_main.run

