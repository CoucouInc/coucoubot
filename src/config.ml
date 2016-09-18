open Prelude

type t = {
  server : string;
  port : int;
  username : string;
  realname : string;
  nick : string;
  channel : string;
  factoids_file : string;
}

let default = {
  server = "irc.freenode.net";
  port = 6667;
  username = "cube_bot";
  realname = "cube_bot";
  nick = "cube_bot";
  channel = "#ocaml";
  factoids_file = "factoids.json";
}

let parse conf args =
  let custom_nick = ref None in
  let custom_chan = ref None in
  let custom_server = ref None in
  let custom_factoids = ref None in
  let options = Arg.align
      [ "--nick", Arg.String (fun s -> custom_nick := Some s),
        " custom nickname (default: " ^ default.nick ^ ")"
      ; "--chan", Arg.String (fun s -> custom_chan := Some s),
        " channel to join (default: " ^ default.channel ^ ")"
      ; "--server", Arg.String (fun s -> custom_server := Some s),
        " server to join (default: " ^ default.server ^ ")"
      ; "--factoids", Arg.String (fun s -> custom_factoids := Some s),
        " file containing factoids (default: " ^ default.factoids_file ^ ")"
      ]
  in
  Arg.parse_argv args options ignore "parse options";
  { conf with
    nick = !custom_nick |? conf.nick;
    channel = !custom_chan |? conf.channel;
    server = !custom_server |? conf.server;
    factoids_file = !custom_factoids |? conf.factoids_file;
  }

let of_argv () =
  try parse default Sys.argv
  with
    | Arg.Bad msg -> print_endline msg; exit 1
    | Arg.Help msg -> print_endline msg; exit 0
