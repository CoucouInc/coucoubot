module C = Calculon
module CW = Calculon_web
module CE = Calculon_extras

let all_ : C.Plugin.t list =
  [
    C.Plugin_factoids.plugin;
    C.Plugin_social.plugin;
    C.Plugin_state.plugin;
    C.Plugin_vote.plugin;
    C.Plugin_history.plugin ~n:200 ~default_len:10 ();
    CW.Plugin_movie.plugin;
    CW.Plugin_web.plugin;
    Plugin_coucou.plugin;
    Plugin_choice.plugin;
    Plugin_misc.plugin;
    Plugin_search.plugin;
  ]

let channels =
  try Sys.getenv "CHANNELS" |> String.split_on_char ','
  with _ -> [ "##arch-fr-free" ]

let config =
  {
    C.Config.default with
    C.Config.log_level = Logs.Info;
    server = "irc.libera.chat";
    port = 6697;
    username = "coucoubot";
    realname = "coucoubot";
    nick = "coucoubot";
    channels;
    tls = true;
    db_file = "coucoubot.db";
  }

let () =
  Logs.set_reporter (Logs.format_reporter ());
  try
    (* update with CLI parameters *)
    let config = C.Config.parse config Sys.argv in
    Logs.set_level ~all:true (Some config.C.Config.log_level);
    Logs.info (fun k -> k "start coucoubot");
    C.Run_main.main config all_ |> Lwt_main.run
  with
  | Arg.Help msg -> print_endline msg
  | Arg.Bad msg ->
    prerr_endline msg;
    exit 1
