open Calculon
open Prelude

(** Separate a string into n parts using the given separator **)
let extract_choices_sep sep str =
  let sep = String.lowercase_ascii sep in
  let rec list_to_string l =
    match l with
      | s::ss -> s ^ " " ^ list_to_string ss
      | [] -> ""
  in
  let split_list_on_element el l =
    let rec f_aux el l =
      match l with
        | e::es ->
          begin
            if String.equal (String.lowercase_ascii e) el then (
              let (l1, l) = f_aux el es in
              ([] ,l1::l)
            ) else (
              let l1, l = f_aux el es in
              (e::l1, l)
            )
          end
        | [] -> ([], [])
    in
    let l1, l = f_aux el l in
    l1::l
  in
  match String.split_on_char ' ' str |> List.filter (fun s -> s <> "") with
    | [] -> None
    | [_] -> None
    | [s1;s2] -> Some [s1; s2]
    | l ->
      begin
        let l = split_list_on_element sep l
          |> List.map list_to_string
          |> List.filter (fun s -> s <> "")
        in
        if List.length l < 2 then None else Some l
      end

(** Make several successives separator attempts on the string **)
let rec extract_choices_seps sep_list str =
  match sep_list with
    | [] -> None
    | s::ss ->
      match extract_choices_sep s str with
        | None -> extract_choices_seps ss str
        | Some c -> Some c

(** Just separate the string with spaces, used if no separators work **)
let extract_basic_choices str =
  match String.split_on_char ' ' str |> List.filter (fun s -> s <> "") with
    | [] -> None
    | [_] -> None
    | l -> Some l

let extract_choices sep_list str =
    match extract_choices_seps sep_list str with
      | Some c -> Some c
      | None -> extract_basic_choices str

let supported_separators = ["|"; "||"; "&"; "&&"; "vs"; "or"; "and"; "ou"; "et";
  "contre"; "versus"]

(* Gives an arbitrary choice based on a arbitrary hash: gives a total order *)
let determinate_choice l =
    let val_of_str a = Hashtbl.hash (a |> String.trim |> String.lowercase_ascii) in
    List.sort (fun a b -> val_of_str a - (val_of_str b)) l |> List.hd

(* setup choice commands *)
let cmds_choice =
  (* create a choice command using a specific choice_function *)
  let cmd_function choice_function result_message failure_message =
    (fun _ str ->
       let choice_opt = String.trim str
         |> extract_choices supported_separators |> map_opt choice_function in
      let msg = choice_opt |> map_opt (fun s -> result_message ^ s) |? failure_message in
      Lwt.return (some msg)
    )
  in
  (* easily define a choice command *)
  let command_make_choice command_name command_descr choice_function result_message failure_message =
      Command.make_simple ~descr:command_descr ~prefix:command_name ~prio:10
        (cmd_function choice_function result_message failure_message)
  in

  [command_make_choice "choix" "Faire un choix entre deux options" random_l "Et c'est décidé ! Ce sera "
     "Malheureusement, il n'est pas possible de parser votre indécision, même avec les dernières percées de nos IA multi-cloud-ready bare metal. Faites un effort !";
   command_make_choice "choice" "Make a choice between two options" random_l "And it's decided! It's gonna be "
     "Sorry, unable to parse your query here. Please consider upgrading your bot to a modern language like golang or ruby ;). Who still uses OCaml nowadays?";
   command_make_choice "battle" "Qui que c'est le plus fort ?" determinate_choice
     "Évidemment, ça va être " "Pas facile de dire… Z'êtes sûrs de la question ?"
  ]

let plugin = Plugin.of_cmds cmds_choice
