open Lwt.Infix
open Cohttp_lwt_unix
open Soup

let page_title uri =
  Client.get uri >>= fun (_, body) ->
  Cohttp_lwt_body.to_string body >>= fun body ->
  parse body $ "title" |> leaf_text |> Lwt.return

let youtube_hosts = [
  "youtube.com"; "www.youtube.com";
  "youtu.be"; "www.youtu.be";
]

let youtube _nick msg =
  let uri = Uri.of_string msg in
  match Uri.host uri with
  | Some host when List.mem host youtube_hosts ->
      page_title uri
  | _ ->
    Lwt.return None

let triggers = [
  youtube
]

let apply nick msg =
  Lwt_list.fold_left_s (fun ret f ->
    match ret with
    | Some _ -> Lwt.return ret
    | None -> f nick msg
  ) None triggers
