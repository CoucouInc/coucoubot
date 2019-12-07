module C = Calculon
module CW = Calculon_web
module CE = Calculon_extras
module CR = Calculon_redis

let all_ : C.Plugin.t list = [
  C.Plugin_factoids.plugin;
  C.Plugin_social.plugin;
  C.Plugin_state.plugin;
  C.Plugin_vote.plugin;
  C.Plugin_history.plugin ~n:200 ~default_len:10 ();
  CW.Plugin_movie.plugin;
  CW.Plugin_web.plugin;
  CE.Plugin_markcough.plugin;
  Plugin_coucou.plugin;
  Plugin_choice.plugin;
  Plugin_misc.plugin;
  CR.make_plugin ();
  Plugin_tg.plugin;
  Plugin_search.plugin;
]

let config = {
  C.Config.default with
  C.Config.
  server = "irc.freenode.net";
  port = 7000;
  username = "coucoubot";
  realname = "coucoubot";
  nick = "coucoubot";
  channel = "#arch-fr-free";
}

let () =
  try
    (* update with CLI parameters *)
    let config = C.Config.parse config Sys.argv in
    C.Run_main.main config all_ |> Lwt_main.run
  with
    | Arg.Help msg -> print_endline msg
    | Arg.Bad msg -> prerr_endline msg; exit 1

