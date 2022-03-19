
open Calculon
open Calculon.DB_utils
open Containers

type t = DB.db

let prepare_db (self:t) : unit =
  DB.exec self
    {| CREATE TABLE IF NOT EXISTS
        coucou(name TEXT NOT NULL,
               count INTEGER NOT NULL,
               UNIQUE (name) ON CONFLICT FAIL
               ) STRICT;
    |} |> check_db_ self;
  DB.exec self
    {| CREATE INDEX IF NOT EXISTS idx_coucou on coucou(name); |}
    |> check_db_ self;
  ()

(* plugin *)

(* Update coucous *)
let is_coucou msg =
  Prelude.contains msg (Re.Perl.compile_pat "[^!]\\bcoucou\\b")
  ||
  CCString.prefix ~pre:"coucou" msg

let () =
  assert (is_coucou "coucou");
  assert (is_coucou " coucou");
  assert (is_coucou "foo bar coucou yolo");
  assert (not (is_coucou "!coucou"));
  assert (not (is_coucou "!coucou yolo"));
  ()

let get_count (self:t) nick : int =
  let@ () = wrap_failwith "coucou.get" in
  let@ stmt =
    with_stmt self
      {| SELECT (SELECT count FROM coucou WHERE name=?) |}
  in
  DB.bind_text stmt 1 nick |> check_db_ self;
  let rc = DB.step stmt in
  check_db_ self rc;
  match rc with
  | DB.Rc.DONE -> 0
  | _ -> DB.column_int stmt 0

let shift_coucou ~by (self:t) nick : unit =
  let@ () = wrap_failwith "coucou.shift" in
  let@ stmt =
    with_stmt self
      {| INSERT INTO coucou(name, count) VALUES(?1, ?2)
         ON CONFLICT(name) DO
         UPDATE SET count = count + ?2;
      |}
  in
  DB.bind_text stmt 1 nick |> check_db_ self;
  DB.bind_int stmt 2 by |> check_db_ self;
  DB.step stmt |> check_db_ self

let incr_coucou = shift_coucou ~by:1
let decr_coucou = shift_coucou ~by:~-1

let cmd_coucou (self:t) =
  Command.make_simple
    ~descr:"increment coucou level" ~cmd:"coucou" ~prio:10
    (fun msg s ->
       let s = String.trim s in
       if String.contains s ' ' then None
       else (
         let nick = if String.equal s "" then msg.Core.nick else s in
         let coucou_count = get_count self nick in
         let message =
           Printf.sprintf "%s est un·e coucouteur niveau %d"
             nick coucou_count
         in
         Some message
       )
    )

(* update coucou *)
let on_message (self:t) _ msg =
  match Core.privmsg_of_msg msg with
    | None -> ()
    | Some msg ->
      let target = Core.reply_to msg in
      if is_coucou msg.Core.message then (
        if Core.is_chan target
        then incr_coucou self msg.Core.nick
        else decr_coucou self msg.Core.nick
      )

let plugin =
  let commands state =
    [ cmd_coucou state;
    ]
  in
  Plugin.db_backed
    ~on_msg:(fun st -> [on_message st]) ~prepare_db
    ~commands
    ()

