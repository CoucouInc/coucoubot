
(** {1 Command Type} *)

open Lwt.Infix

type res =
  | Cmd_match of unit Lwt.t
  | Cmd_skip
  | Cmd_fail of string

type t = {
  prio: int; (* priority. The lower, the more urgent this command is. *)
  match_: Core.t -> Core.privmsg -> res;
  name: string;
  descr: string; (* for !help *)
}

let make ?(descr="") ?(prio=99) ~name f =
  { descr; prio; name; match_=f; }

let match_prefix1 ~prefix msg =
  let re = Str.regexp (Printf.sprintf "^![ ]*%s[ ]*\\(.*\\)$" prefix) in
  Prelude.re_match1 Prelude.id re msg.Core.message

exception Fail of string

let make_simple_l ?descr ?prio ~prefix f : t =
  let match_ (module C:Core.S) msg =
    match match_prefix1 ~prefix msg with
      | None -> Cmd_skip
      | Some sub ->
        try
          let fut =
            f msg sub >>= fun lines ->
            C.send_notice_l ~target:(Core.reply_to msg)
              ~messages:lines
          in
          Cmd_match fut
        with Fail msg ->
          Cmd_fail msg
  in
  make ?descr ?prio ~name:prefix match_

let make_simple ?descr ?prio ~prefix f : t =
  make_simple_l ?descr ?prio ~prefix
    (fun msg s -> f msg s >|= function
       | None -> []
       | Some x -> [x])

let compare_prio c1 c2 = compare c1.prio c2.prio

(* help command *)
let cmd_help (l:t list): t =
  make_simple ~descr:"help message" ~prefix:"help" ~prio:5
    (fun _ s ->
       let s = String.trim s in
       let res =
         if s=""
         then
           let l = List.map (fun c -> c.name) l in
           let message =
             "!help: commands are " ^ Prelude.string_list_to_string l
           in
           Some message
         else
           try
             let c = List.find (fun c -> c.name = s) l in
             Some (Printf.sprintf "%s: %s (prio %d)" c.name c.descr c.prio)
           with Not_found ->
             Some ("error: unknown command " ^ s)
       in
       Lwt.return res
    )

let run core l msg : unit Lwt.t =
  let rec aux = function
    | [] ->
      Log.logf "no command found for %s" (Core.string_of_privmsg msg);
      Lwt.return_unit
    | c :: tail ->
      begin match c.match_ core msg with
        | Cmd_skip -> aux tail
        | Cmd_match f ->
          Log.logf "command %s succeeded for %s"
            c.name (Core.string_of_privmsg msg);
          f
        | Cmd_fail e ->
          Log.logf "command %s failed on %s with %s"
            c.name (Core.string_of_privmsg msg) e;
          aux tail
      end
  in
  aux (cmd_help l :: l)
