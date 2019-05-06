(** {1 Commands querying the Web} *)

open Calculon
open Cohttp_lwt_unix
open Lwt.Infix

let cancer_uri =
  Uri.of_string "https://polochon.lelele.io/cancer/quickcancer"

let cmd_cancer =
  Command.make_simple
    ~prio:10 ~cmd:"cancer" ~descr:"lookup in the abyss of bad videos"
    (fun _ s ->
       Log.logf "!cancer %S (now querying content)" s;
       Client.get cancer_uri >>= fun (c, body) ->
       Log.logf "http response: %s" @@ Format.asprintf "%a@?" Response.pp_hum c;
       Cohttp_lwt.Body.to_string body >>= fun body ->
       Log.logf "got cancer page (%d bytes)" @@ String.length body;
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
             Log.logf "cancer: lookup with query %S" search;
             let re = Re.Perl.compile_pat ~opts:[`Caseless] search in
             CCList.filter_map
               (fun (title, url) ->
                  if Prelude.contains title re then Some (title, url) else None)
               links
       in
       if links_with_search = [] then (
         Log.logf "cancer: nothing found";
         Lwt.return_none
       ) else (
         let message = fmt_link @@ Prelude.random_l links_with_search in
         Log.logf "cancer: picked %S" message;
         Lwt.return (Some message)
       )
    )

let plugin =
  [ cmd_cancer;
  ] |> Plugin.of_cmds
