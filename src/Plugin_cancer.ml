
(** {1 Commands querying the Web} *)

open Calculon
open Cohttp_lwt_unix
open Lwt.Infix

let cancer_uri =
  Uri.of_string "https://polochon.lelele.io/cancer/quickcancer"

let cmd_cancer =
  Command.make_simple
    ~prio:10 ~prefix:"cancer" ~descr:"lookup in the abyss of bad videos"
    (fun _ s ->
       Client.get cancer_uri >>= fun (_, body) ->
       Cohttp_lwt_body.to_string body >>= fun body ->
       let fmt_link (title, url) = title ^ ":" ^ url in
       let links =
         Str.split (Str.regexp "\n") body
         |> List.filter ((<>) "")
         |> CCList.filter_map (fun s ->
           match Str.bounded_split (Str.regexp ":") s 2 with
             | [title; url] -> Some (title, url)
             | _ -> None)
       in
       let links_with_search =
         match String.trim s with
           | "" -> links
           | search ->
             let re = Str.regexp_case_fold search in
             CCList.filter_map
               (fun (title, url) ->
                  if Prelude.contains title re then Some (title, url) else None)
               links
       in

       if links_with_search = [] then Lwt.return_none
       else
         let message = fmt_link @@ Prelude.random_l links_with_search in
         Lwt.return (Some message)
    )

let plugin =
  [ cmd_cancer;
  ] |> Plugin.of_cmds
