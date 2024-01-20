module C = Calculon
module CW = Calculon_web
module CE = Calculon_extras
module H = Tiny_httpd
module Prometheus = Tiny_httpd_prometheus

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

let setup_httpd ~http_port () : H.t * Prometheus.Registry.t =
  let server = H.create ~addr:"0.0.0.0" ~port:http_port () in

  let reg = Prometheus.global in
  Prometheus.instrument_server server reg;
  Prometheus.GC_metrics.create_and_update_before_emit reg;

  server, reg

let () =
  Logs.set_reporter (Logs.format_reporter ());

  let http_port =
    try int_of_string @@ Sys.getenv "HTTP_PORT" with _ -> 8089
  in

  let config =
    try C.Config.parse config Sys.argv with
    | Arg.Help msg ->
      print_endline msg;
      exit 0
    | Arg.Bad msg ->
      prerr_endline msg;
      exit 1
  in

  (* update with CLI parameters *)
  Logs.set_level ~all:true (Some config.C.Config.log_level);
  Logs.info (fun k -> k "start coucoubot");

  let http_server, prom = setup_httpd ~http_port () in

  Logs.info (fun k ->
      k "listening on %s:%d" (H.addr http_server) (H.port http_server));
  let msg_counters =
    List.map
      (fun c ->
        ( c,
          Prometheus.Counter.create prom
            ~tags:[ "chan", c ]
            "coucoubot_messages" ))
      channels
  in
  (* serve http in the background *)
  let _t_server : Thread.t =
    Thread.create (fun () -> H.run_exn http_server) ()
  in

  (* make sure to have metrics for IRC *)
  let on_init ((module C) : Calculon.Core.t) : unit =
    Calculon.Signal.on' C.messages (fun (msg : Irc_message.t) ->
        (match msg.command with
        | PRIVMSG (target, _) ->
          (match List.assq_opt target msg_counters with
          | None -> ()
          | Some c -> Prometheus.Counter.incr c)
        | _ -> ());
        Lwt.return_unit)
  in

  Lwt_main.run @@ C.Run_main.main ~on_init config all_
