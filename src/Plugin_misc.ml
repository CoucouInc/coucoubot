(** {1 Commands querying the Web} *)

open Calculon
open Lwt.Infix

let cancer_uri =
  Uri.of_string "https://raw.githubusercontent.com/CoucouInc/lalalaliste/master/cancer.txt"

let get_uri = Calculon_web.Plugin_web.get_body

let cmd_cancer =
  Command.make_simple
    ~prio:10 ~cmd:"cancer" ~descr:"lookup in the abyss of bad videos"
    (fun _ s ->
       Logs.debug (fun k->k "!cancer %S (now querying content)" s);
       get_uri cancer_uri >>= fun body ->
       Logs.debug (fun k->k  "got cancer page (%d bytes)" @@ String.length body);
       let fmt_link (title, url) = title ^ ":" ^ url in
       let links =
         CCString.Split.list_cpy ~by:"\n" body
         |> List.filter ((<>) "")
         |> CCList.filter_map (CCString.Split.left ~by:":")
       in
       let links_with_search =
         match String.trim s with
           | "" -> links
           | search ->
             Logs.debug (fun k->k  "cancer: lookup with query %S" search);
             let re = Re.Perl.compile_pat ~opts:[`Caseless] search in
             CCList.filter_map
               (fun (title, url) ->
                  if Prelude.contains title re then Some (title, url) else None)
               links
       in
       if links_with_search = [] then (
         Logs.debug (fun k->k  "cancer: nothing found");
         Lwt.return_none
       ) else (
         let message = fmt_link @@ Prelude.random_l links_with_search in
         Logs.debug (fun k->k "cancer: picked %S" message);
         Lwt.return (Some message)
       )
    )

let wikihow_random =
  "https://www.wikihow.com/Special:Randomizer"

let cmd_how =
  Command.make_simple
    ~prio:10 ~cmd:"how" ~descr:"how do I do things??"
    (fun _ _ ->
       Logs.debug (fun k->k "!how (now querying)");
       Lwt_preemptive.detach (fun () ->
         Curly.run ~args:["-L"]
           Curly.(Request.make ~url:wikihow_random ~meth:`GET ())) ()
       >>= function
       | Ok {Curly.Response.code; headers; _} ->
         Logs.debug (fun k->k "how: got answer (code: got %d; expected 302)" code);
         if code = 302 then (
           match List.assoc_opt "location" headers with
           | Some url ->
             Logs.debug (fun k->k "how: found page url: %S" url);
             Lwt.return (Some url)
           | None ->
             Logs.debug (fun k->k "how: no 'location' header");
             Lwt.return None
         ) else
           Lwt.return None
       | Error _e ->
         Logs.debug (fun k->k "how: curl error");
         Lwt.return None
    )

let cmd_reactions =
  let l = [
    "je suis un singe", [["ook? ook"]; ["🙈"]; ["🐵"; "👕"; "👖"]], 0.8;
    "ook ook", [["je suis un singe"];["tu es un singe‽"]], 0.7;
    "jizz", [["https://www.youtube.com/watch?v=VLnWf1sQkjY"]], 0.6;
    "NFT", [["&stupid mint an NFT for it"]], 0.3;
    "pieds", [["lickent les feetent 😋"]], 0.2;
    "feetent", [["👅🦶✨"]], 0.6;
    "la dutch", [["🏩"]], 0.1;
    "la migros", [["sah le gruyère suisse 🧀🇨🇭"]], 0.3;
    "jpp jpp", [["kouraj bb 💞"]], 0.6;
    "golem", [["gothéorème ◼️"]], 0.1;
    "en sueur", [["en sueur2sueur"]], 0.1;
    "choppé le covid", [["🦠¬💉 ⇒ PLS bb :-("]], 0.4;
    "wikifeet", [["👀🦶🤑"]], 0.6;
    "vocaroo", [["le guide du vocaroo tard?"]], 0.5;
    "yaourt", [["goûte mon yaourt aux 🍒"]], 0.1;
    "crypto", [["godecoin to the moon 🍆🍑"]], 0.6;
    "va sortir", [["go bar chrétien"]], 0.2;
    "astuce", [["go fiole d'estus"]], 0.1;
    "rasengan", [["notre grand maitre IRC"]], 0.8;
    "freenode", [["irc.com, fief de root@ ? :thin"]], 0.6;
    "https://reddit", [["cc tu connais old.reddit.com?"]], 0.99;
    "wat", [["WHAT.CD EST DOWN?"]], 0.05;
    "ladurée", [["mais bon, il parait qu'ils sont bons"]], 0.7;
  ] |> List.map (fun (s,l,p) -> String.uncapitalize_ascii s,l,p)
  in

  Command.make ~name:"reactions"
    (fun ~prefix:_ (module C) msg ->
       begin match
           let msg = String.uncapitalize_ascii msg.Core.message in
           List.find_opt (fun (s,_,_) -> CCString.mem ~sub:s msg) l
         with
         | Some (_, [], _) -> assert false
         | Some (_, choices, proba) ->
           if Random.float 1. < proba then (
             let target = Core.reply_to msg in
             let l = Prelude.random_l choices in
             Command.Cmd_match (C.send_privmsg_l ~target ~messages:l)
           ) else Command.Cmd_skip
         | None -> Command.Cmd_skip
       end)

let plugin =
  [ cmd_cancer;
    cmd_how;
    cmd_reactions;
  ] |> Plugin.of_cmds
