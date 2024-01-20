(** {1 Search plugin} *)

open Calculon
module Db = Sqlite3_utils

(* schema is:
   [
      CREATE VIRTUAL TABLE irc using fts3(author, msg, date);
   ]
*)

type state = { db: Db.t }

let cmd_search state =
  Command.make_simple ~descr:"search in logs sneakily" ~cmd:"nsa" ~prio:10
    (fun msg s ->
      let s = String.trim s in
      if s = "" then
        Lwt.return_none
      else if not (Core.is_chan msg.Core.to_) then
        Lwt.return_some
          "accountability requires questions be asked on a public channel."
      else (
        match
          Db.exec state.db
            "select author,date,msg from irc where irc match ?\n\
            \                 and not msg like '!nsa%'\n\
            \                 order by random() limit 1;"
            ~ty:Db.Ty.(p1 text, p3 text text text, mkp3)
            s ~f:Db.Cursor.to_list
        with
        | Ok [ (author, date, msg) ] ->
          Lwt.return @@ Some (Printf.sprintf "%s %s> %s" date author msg)
        | Ok [] -> Lwt.return (Some "nothing found")
        | Ok _ -> assert false
        | exception e ->
          Printf.eprintf "exn in query:\n%s\n%!" (Printexc.to_string e);
          Lwt.return_none
        | Error e ->
          Printf.eprintf "error in query: %s\n%!" (Sqlite3.Rc.to_string e);
          Lwt.return_none
      ))

let file = "logs.sqlite"

let of_json _actions _ =
  try
    let db = Sqlite3.db_open file in
    Logs.info (fun k -> k "opened sqlite DB %S" file);
    Db.setup_timeout ~ms:500 db;
    Ok { db }
  with e ->
    Logs.err (fun k ->
        k "error when opening db %S: %s" file (Printexc.to_string e));
    Error "cannot open DB"

let to_json _ = None

let stop { db } =
  ignore (Sqlite3.db_close db : bool);
  Lwt.return ()

(* update logs *)
let on_message state _ msg =
  match Core.privmsg_of_msg msg with
  | Some msg
    when Core.is_chan msg.Core.to_ && not (CCString.prefix ~pre:"!" msg.message)
    ->
    (* log only public messages that are not commands *)
    let pp_date out () =
      ISO8601.Permissive.pp_format out "%Y-%M-%D %h:%m:%s"
        (Unix.gettimeofday ()) 0.
    in
    let date = Format.asprintf "%a@?" pp_date () in
    Db.transact state.db (fun db ->
        match
          Db.exec_no_cursor db "insert into irc values (?,?,?);"
            ~ty:Db.Ty.(p3 text text text)
            msg.nick msg.message date
        with
        | Ok () -> ()
        | exception e ->
          Logs.err (fun k ->
              k "cannot insert into DB: exn\n%s\n%!" (Printexc.to_string e))
        | Error e ->
          Logs.err (fun k ->
              k "cannot insert log into DB: %s\n%!" (Sqlite3.Rc.to_string e)));
    Lwt.return ()
  | _ -> Lwt.return_unit

let plugin =
  let commands st = [ cmd_search st ] and on_msg st = [ on_message st ] in
  Plugin.stateful ~name:"search" ~on_msg ~of_json ~to_json ~stop ~commands ()
