open Prelude

(* User defined config ********************************************************)

let default_server = "irc.freenode.net"
let port = 6667
let username = "coucoubot"
let realname = "coucoubot"
let default_nick = "coucoubot"
let default_channel = "#arch-fr-free"
let default_factoids = "factoids.json"
(******************************************************************************)

let custom_nick = ref None
let custom_chan = ref None
let custom_server = ref None
let custom_factoids = ref None

let options = Arg.align
    [ "--nick", Arg.String (fun s -> custom_nick := Some s),
      " custom nickname (default: " ^ default_nick ^ ")"
    ; "--chan", Arg.String (fun s -> custom_chan := Some s),
      " channel to join (default: " ^ default_channel ^ ")"
    ; "--server", Arg.String (fun s -> custom_server := Some s),
      " server to join (default: " ^ default_server ^ ")"
    ; "--factoids", Arg.String (fun s -> custom_factoids := Some s),
      " file containing factoids (default: " ^ default_factoids ^ ")"
    ]
let () = Arg.parse options ignore "coucou"

let nick = !custom_nick |? default_nick
let channel = !custom_chan |? default_channel
let server = !custom_server |? default_server
let factoids_file = !custom_factoids |? default_factoids
