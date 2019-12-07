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

let plugin =
  [ cmd_cancer;
  ] |> Plugin.of_cmds
