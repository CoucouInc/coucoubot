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
        None
      else if not (Core.is_chan msg.Core.to_) then
        Some "accountability requires questions be asked on a public channel."
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
          Some (Printf.sprintf "%s %s> %s" date author msg)
        | Ok [] -> Some "nothing found"
        | Ok _ -> assert false
        | exception e ->
          Printf.eprintf "exn in query:\n%s\n%!" (Printexc.to_string e);
          None
        | Error e ->
          Printf.eprintf "error in query: %s\n%!" (Sqlite3.Rc.to_string e);
          None
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
let stop { db } = ignore (Sqlite3.db_close db : bool)

(* update logs *)
let on_message state _ msg =
  match Core.privmsg_of_msg msg with
  | Some msg
    when Core.is_chan msg.Core.to_ && not (CCString.prefix ~pre:"!" msg.message)
    ->
    (* log only public messages that are not commands *)
    let date =
      let now = Ptime_clock.now () in
      let ((y, mo, d), ((h, m, s), _)) = Ptime.to_date_time now in
      Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" y mo d h m s
    in
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
              k "cannot insert log into DB: %s\n%!" (Sqlite3.Rc.to_string e)))
  | _ -> ()

let plugin =
  let commands st = [ cmd_search st ] and on_msg st = [ on_message st ] in
  Plugin.stateful ~name:"search" ~on_msg ~of_json ~to_json ~stop ~commands ()
