
open Calculon
open Prelude
open Containers

module J = Yojson.Safe.Util
type json = Yojson.Safe.json

type to_tell = {
  from: string;
  on_channel: string;
  msg: string;
  tell_after: float option; (** optional; not before this deadline (UTC) *)
}

(* Data for contacts *)
type contact = {
  coucous : int;
}

exception Bad_json

let contact_of_json (json: json): contact option =
  try
    { coucous = J.to_int_option json |? 0 } |> some
  with Bad_json | J.Type_error (_, _) -> None

let json_of_contact (c: contact): json = `Int c.coucous

(* Contacts db *)

type t = contact StrMap.t

type state = {
  actions: Plugin.action_callback;
  mutable map: t;
}

let write_db (db:state) =
  Signal.Send_ref.send db.actions Plugin.Require_save

let is_contact state nick = StrMap.mem nick state.map

let set_data state ?(force_sync = true) nick contact =
  state.map <- StrMap.add nick contact state.map;
  if force_sync then Lwt.async (fun () -> write_db state)

let new_contact state nick =
  if not (is_contact state nick) then
    set_data state nick {
      coucous = 0;
    }

let data state nick =
  if not @@ is_contact state nick then new_contact state nick;
  StrMap.find nick state.map

(* plugin *)

(* Update coucous *)
let is_coucou msg =
  contains msg (Re_perl.compile_pat "[^!]\\bcoucou\\b")
  ||
  CCString.prefix ~pre:"coucou" msg

let () =
  assert (is_coucou "coucou");
  assert (is_coucou " coucou");
  assert (is_coucou "foo bar coucou yolo");
  assert (not (is_coucou "!coucou"));
  assert (not (is_coucou "!coucou yolo"));
  ()

let shift_coucou ~by state nick =
  let d = data state nick in
  set_data state ~force_sync:false nick {coucous = d.coucous + by}

let incr_coucou = shift_coucou ~by:1
let decr_coucou = shift_coucou ~by:~-1

let cmd_coucou state =
  Command.make_simple
    ~descr:"increment coucou level" ~prefix:"coucou" ~prio:10
    (fun msg s ->
       let s = String.trim s in
       if String.contains s ' ' then Lwt.return_none
       else (
         let nick = if String.equal s "" then msg.Core.nick else s in
         let coucou_count = (data state nick).coucous in
         let message =
           Printf.sprintf "%s est un coucouteur niveau %d"
             nick coucou_count
         in
         Lwt.return (Some message)
       )
    )


let of_json actions = function
  | None ->
    Lwt_err.return {actions; map=StrMap.empty}
  | Some j ->
    let map = match j with
      | `Assoc l ->
        l
        |> CCList.filter_map (fun (k, j) ->
          Option.(contact_of_json j >>= fun c -> Some (k, c)))
        |> StrMap.of_list
      | _ -> StrMap.empty
    in
    Lwt_err.return {actions; map}

let to_json (db:state) =
  let json = `Assoc (
    StrMap.to_list db.map
    |> List.map (fun (k, c) -> (k, json_of_contact c))
  ) in
  Some json

(* update coucou *)
let on_message state _ msg =
  match Core.privmsg_of_msg msg with
    | None -> Lwt.return_unit
    | Some msg ->
      let target = Core.reply_to msg in
      if is_coucou msg.Core.message then (
        if Core.is_chan target
        then incr_coucou state msg.Core.nick
        else decr_coucou state msg.Core.nick
      );
      Lwt.return ()

let plugin =
  let commands state =
    [ cmd_coucou state;
    ]
  in
  Plugin.stateful
    ~name:"coucou"
    ~on_msg:(fun st -> [on_message st])
    ~of_json ~to_json
    ~commands
    ()

