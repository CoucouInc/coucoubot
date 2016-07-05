open Prelude
open Lwt.Infix
open Cohttp_lwt_unix
open Soup

(* Commandes: une commande est la donnée de :
   - un nom
   - une fonction.

   À chaque fois que quelqu'un dit "!nom [trucs]",
   la fonction correspondant à "nom" est appellée avec,
   comme arguments :

   - la connexion
   - le canal
   - le nick de la personne qui a lancé la commande
   - la chaine correspondant à [trucs]
*)

let tell (_: Irc.connection_t) channel nick s =
  try
    let (dest, msg) =
      let a = Str.bounded_split (Str.regexp " ") (String.trim s) 2 in
      (List.hd a, List.hd @@ List.tl a) in
    Social.(
      set_data dest
        {(data dest) with
         to_tell = (nick, channel, msg) :: (data dest).to_tell}
    );
    Talk.(talk ~target:channel Ack)
  with _ -> Lwt.return ()

let coucoulevel connection channel nick s =
  let s = String.trim s in
  let nick = if s <> "" then s else nick in
  let coucou_count = Social.((data nick).coucous) in
  let message = Printf.sprintf "%s est un coucouteur niveau %d"
      nick coucou_count in
  Irc.send_privmsg ~connection ~target:channel ~message


let page_title uri =
  Client.get uri >>= fun (_, body) ->
  Cohttp_lwt_body.to_string body >>= fun body ->
  parse body $ "title" |> leaf_text |> Lwt.return

let youtube_hosts = [
  "youtube.com"; "www.youtube.com";
  "youtu.be"; "www.youtu.be";
]

let yt connection channel _ s =
  let uri = Uri.of_string (String.trim s) in
  match Uri.host uri with
  | Some host when List.mem host youtube_hosts ->
    (page_title uri >>= function
     | Some title ->
       Irc.send_privmsg ~connection ~target:channel ~message:title
     | _ -> Lwt.return ())
  | _ ->
    Lwt.return ()

let refcmds = ref []
let refcmdNames = ref []

let listCommands connection target nick _s =
  let str = ref "" in
  List.iter (fun (cmd, _) -> str := !str ^ cmd ^ " ") !refcmdNames;
  Irc.send_privmsg ~connection ~target
    ~message:(nick ^ ": "^ String.trim !str)

let trigger = "!"

(* Liste des commandes : ajoutez les votres ici
   (couples nom/fonction, séparées par des ;) *)
let commandNames = [
  "help", listCommands;
  "tell", tell;
  "coucou", coucoulevel;
  "yt", yt;
]

let commands = commandNames
|> List.map (fun (c, f) -> (trigger ^ c ^ "\\b", f))
|> List.map (fun (c, f) -> (Str.regexp c, f))

let () = refcmds := commands; refcmdNames := commandNames
