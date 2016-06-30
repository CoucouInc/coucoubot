let string_opt_to_string = function
  | None -> "None"
  | Some s -> Printf.sprintf "Some %s" s

let string_list_to_string string_list =
  Printf.sprintf "[%s]" (String.concat "; " string_list)

let get_nick h =
  Str.split_delim (Str.regexp "!") h |> List.hd

let some x = Some x

let get_some = function
  | Some x -> x
  | None -> failwith "get_some"

let (|?) o x = match o with
  | None -> x
  | Some y -> y

let contains s x =
  try Str.search_forward x s 0 |> ignore; true with
    Not_found -> false

(* Use [Lwt_unix.sleep] instead *)
(* let rec sleep t = *)
(*   if t > 0. then *)
(*     let now = Unix.gettimeofday () in *)
(*     (try ignore (Unix.select [] [] [] t) with *)
(*     | _ -> ()); *)
(*     sleep (t -. ((Unix.gettimeofday ()) -. now)) *)

let select l = DistribM.run @@ DistribM.uniform l

let id x = x
let uncurry f (x, y) = f x y

include Lwt.Infix
module Msg = Irc_message
module Irc = struct
  include Irc_client_lwt

  let nl = Str.regexp_string "\n"

  (* Permet d'envoyer des messages multilignes *)
  let send_privmsg ~connection ~target ~message =
    Lwt_list.iter_s 
      (fun message -> Irc_client_lwt.send_privmsg ~connection ~target ~message)
      (Str.split nl message)
end 
