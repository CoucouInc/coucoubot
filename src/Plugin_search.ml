

(** {1 Search plugin} *)

open Calculon

module Db = Sqlite3_utils

type state = {
  db: Db.t;
}

let cmd_search state =
  Command.make_simple
    ~descr:"search in logs sneakily" ~cmd:"nsa" ~prio:10
    (fun msg s ->
       let s = String.trim s in
       if s = "" then Lwt.return_none
       else if not (Core.is_chan msg.Core.to_) then (
         Lwt.return_some "accountability requires questions be asked on a public channel."
       ) else (
         match Db.exec state.db
                 "select author,date,msg from irc where msg match ?
                 order by random() limit 1;"
                 ~ty:Db.Ty.(p1 text, p3 text text text, mkp3)
                 s ~f:Db.Cursor.to_list
         with
         | Ok [author,date,msg] ->
           Lwt.return @@ Some (Printf.sprintf "%s %s> %s" date author msg)
         | Ok [] -> Lwt.return (Some "nothing found")
         | Ok _ -> assert false
         | Error e ->
           Log.logf "error in query: %s" (Sqlite3.Rc.to_string e);
           Lwt.return_none
       )
    )

let file = "logs.sqlite"

let of_json _actions _ =
  try
    let db =Sqlite3.db_open file in
    Log.logf "opened sqlite DB %S" file;
    Lwt.return @@ Ok {db}
  with e ->
    Log.logf "error when opening db %S: %s" file (Printexc.to_string e);
    Lwt.return @@ Error "cannot open DB"

let to_json {db} =
  ignore (Sqlite3.db_close db : bool); None

(* update logs *)
let on_message state _ msg =
  match Core.privmsg_of_msg msg with
    | Some msg when Core.is_chan msg.Core.to_ ->
      (* log only public messages *)
      let pp_date out () =
        ISO8601.Permissive.pp_format out "%Y-%M-%D %h:%m:%s" (Unix.gettimeofday()) 0.
      in
      let date = Format.asprintf "%a@?" pp_date () in
      Db.transact state.db
        (fun db ->
           match Db.exec_no_cursor db "insert into irc values (?,?,?);"
                   ~ty:Db.Ty.(p3 text text text)
                   msg.nick msg.message date
           with
           | Ok () -> ()
           | Error e ->
             Log.logf "cannot insert log into DB: %s" (Sqlite3.Rc.to_string e));
      Lwt.return ()
    | _ -> Lwt.return_unit

let plugin =
  let commands st = [ cmd_search st ]
  and on_msg st = [on_message st] in
  Plugin.stateful
    ~name:"search"
    ~on_msg
    ~of_json ~to_json
    ~commands
    ()
