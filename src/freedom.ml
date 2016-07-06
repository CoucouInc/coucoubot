open Result
open Lwt.Infix

(* TODO add proper lib *)
module Time = struct
  type t = float

  let minutes x = (float_of_int x) *. 60.

  let as_mins x = int_of_float @@ x /. 60.

  let display_mins x =
    match as_mins x with
    | 0 -> "moins d'une minute"
    | 1 -> "une minute"
    | n -> Printf.sprintf "%d minutes" n

  let now = Unix.gettimeofday

end

module Vote = struct

  type vote = For | Against

  type t = {
    purpose : string;
    expire : float;
    status : (string, vote) Hashtbl.t;
    mutable quorum : int;
  }

  let start ?(quorum=5) ?(duration=Time.minutes 30) purpose = 
    { purpose; status = Hashtbl.create 10; expire = Time.(now () +. duration); quorum }

  let add_vote t nick vote =
    match Hashtbl.find t.status nick with
    | exception Not_found -> Hashtbl.add t.status nick vote
    | old_vote when old_vote = vote -> ()
    | _ -> Hashtbl.replace t.status nick vote

  let count_votes t =
    Hashtbl.fold (fun _ vote (f,a) -> match vote with For -> f + 1,a | Against -> f, a + 1) t.status (0,0)

  let vote_status t nick =
    try Some (Hashtbl.find t.status nick) with Not_found -> None

  let explain { purpose; _ } = purpose

  let show_status t =
    let (f, a) = count_votes t in
    Printf.sprintf "%s : exprimés %d / pour %d / contre %d (expire dans %s)"
      (explain t) (Hashtbl.length t.status) f a Time.(display_mins @@ t.expire -. now ())

  let missing_votes t = max 0 @@ t.quorum - Hashtbl.length t.status

  let is_complete t =
    missing_votes t = 0 && let res = count_votes t in fst res <> snd res

  let get_winner t =
    match count_votes t with
    | f, a when f > a -> Some For
    | f, a when f < a -> Some Against
    | _ -> None

  let expired now { expire; _ } = expire < now

  let string_of_vote = function
    | For -> "pour"
    | Against -> "contre"

  let vote_of_string = function
    | "pour" -> Ok For
    | "contre" -> Ok Against
    | _ -> Error "decide toi"
end

type poll = { creator : string; vote : Vote.t }

let polls = Hashtbl.create 10

let max_polls_per_nick = 1
let max_polls = 5

let nb_polls_per_nick nick = Hashtbl.fold begin fun _ { creator;_ } count -> if creator = nick then count + 1 else count end polls 0

let show_status name { creator; vote } =
  Printf.sprintf "sondage %s en cours créé par %s : %s" name creator (Vote.show_status vote)

let create_poll nick name purpose =
  match Hashtbl.length polls with
  | cur_len when cur_len >= max_polls -> Error "trop de sondages en cours"
  | _ ->
    match nb_polls_per_nick nick with
    | cur_polls when cur_polls >= max_polls_per_nick -> Error (Printf.sprintf "impossible de créer plus de %d sondages à la fois" max_polls_per_nick)
    | _ ->
      match Hashtbl.find polls name with
      | poll -> Error (show_status name poll)
      | exception Not_found ->
        Hashtbl.add polls name { creator = nick; vote = Vote.start purpose };
        Ok None

let vote nick name vote =
  match Hashtbl.find polls name with
  | exception Not_found -> Error "pas de sondage en cours"
  | poll ->
    match Vote.vote_of_string vote with
    | Error _ as e -> e
    | Ok vote ->
      Vote.add_vote poll.vote nick vote;
      match Vote.is_complete poll.vote with
      | true ->
        Hashtbl.remove polls name;
        Ok (Some (Printf.sprintf "sondage terminé : decision %s" (CCOpt.get "egalité" @@ CCOpt.map Vote.string_of_vote @@ Vote.get_winner poll.vote)))
      | _ -> Ok (Some (Vote.show_status poll.vote))

let show_vote name nick =
  match Hashtbl.find polls name with
  | exception Not_found -> Error "pas de sondage en cours"
  | poll ->
    let vote = CCOpt.get "indécis vis à vis de" @@ CCOpt.map Vote.string_of_vote @@ Vote.vote_status poll.vote nick in
   Ok (Some (Printf.sprintf "%s est %s %s" nick vote name))

let vote_status name =
  match Hashtbl.find polls name with
  | exception Not_found -> Error "pas de sondage en cours"
  | poll ->
    Ok (Some (show_status name poll))

let rec collector () =
  let now = Time.now () in
  Hashtbl.iter begin fun name { vote; _ } -> if Vote.expired now vote then Hashtbl.remove polls name end polls;
  Lwt_unix.sleep (Time.minutes 1) >>=
  collector

let _ = Lwt.async collector
