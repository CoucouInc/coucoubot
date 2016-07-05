open Containers
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

let cancer_uri = Uri.of_string "https://polochon.lelele.io/cancer/quickcancer"

let cancer connection channel _ s =
  Client.get cancer_uri >>= fun (_, body) ->
  Cohttp_lwt_body.to_string body >>= fun body ->
  let fmt_link (title, url) = title ^ ":" ^ url in
  let links = Str.split (Str.regexp "\n") body
              |> List.filter ((<>) "")
              |> List.filter_map (fun s ->
                match Str.bounded_split (Str.regexp ":") s 2 with
                | [title; url] -> Some (title, url)
                | _ -> None) in
  let links_with_search =
    match String.trim s with
    | "" -> links
    | search ->
      let re = Str.regexp_case_fold search in
      List.filter_map (fun (title, url) ->
        if contains title re then Some (title, url) else None
      ) links in

  if links_with_search = [] then Lwt.return ()
  else
    let message = fmt_link @@ DistribM.(run @@ uniform links_with_search) in
    Irc.send_privmsg ~connection ~target:channel ~message

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
  "cancer", cancer;
]

let commands = commandNames
|> List.map (fun (c, f) -> (trigger ^ c ^ "\\b", f))
|> List.map (fun (c, f) -> (Str.regexp c, f))

let () = refcmds := commands; refcmdNames := commandNames
