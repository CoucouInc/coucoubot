open Calculon

type state = {
  mutable tg: bool; (* be quiet? *)
}

(* NOTE: must have lower priority than [quiet], otherwise it'll never fire *)
let cmd_tgnon (st:state) =
  Command.make_simple ~descr:"speak yer heart ♥" ~prio:1
    ~cmd:"tgnon"
    (fun _msg s ->
       if String.trim s = "" then (
         st.tg <- false;
         Lwt.return (Some "speak, priest!")
       ) else Lwt.return None)

let cmd_quiet (st:state) =
  Command.make ~prio:2 (* as urgent as possible *)
    ~name:"<be quiet>"
    (fun ~prefix:_ _core _msg ->
       if st.tg then Command.Cmd_match (Lwt.return ())
       else Command.Cmd_skip)

let cmd_tg (st:state) =
  Command.make_simple ~descr:"be quiet now, child!" ~prio:15
    ~cmd:"tg"
    (fun _msg s ->
       if String.trim s = "" then (
         st.tg <- true;
         Lwt.return (Some "")
       ) else Lwt.return None)

let of_json _actions _js =
  Lwt.return (Ok { tg=false; })

let to_json _ = None

let plugin =
  let commands state = [
    cmd_tg state;
    cmd_tgnon state;
    cmd_quiet state;
  ] in
  Plugin.stateful
    ~name:"tg"
    ~of_json ~to_json
    ~commands
    ()

