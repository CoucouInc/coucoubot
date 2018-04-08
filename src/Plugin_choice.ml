open Calculon
open Prelude

(** Separate a string into two parts using the given separator **)
let extract_choices sep str =
  let sep = String.lowercase_ascii sep in
  let rec list_to_string l =
    match l with
      | s::ss -> s ^ " " ^ list_to_string ss
      | [] -> ""
  in
  let rec split_list_on_element el l =
    match l with
      | e::es ->
        if String.equal (String.lowercase_ascii e) el then
          [], es
        else
          let l1, l2 = split_list_on_element el es in
          e::l1, l2
      | [] -> ([], [])
  in
  let map_couple f (a,b) = (f a, f b) in
  match String.split_on_char ' ' str |> List.filter (fun s -> s <> "") with
    | [] -> None
    | [_] -> None
    | [s1;s2] -> Some (s1, s2)
    | l ->
      begin match split_list_on_element sep l |> map_couple list_to_string with
        | ("",_) -> None
        | (_,"") -> None
        | c -> Some c
      end

(** Make several successives separator attempts on the string **)
let rec extract_choices_seps sep_list str =
  match sep_list with
    | [] -> None
    | s::ss ->
      match extract_choices s str with
        | None -> extract_choices_seps ss str
        | Some c -> Some c

let cmd_choice =
  let list_of_couple (a,b) = [a; b] in
  let supported_separators = ["vs"; "|"; "||"; "&"; "&&"; "or"; "and"; "ou"; "et";
    "contre"; "versus"] in
  let cmd_function =
    (fun _ str ->
       let choice_opt = 
         String.trim str
         |> extract_choices_seps supported_separators
         |> map_opt list_of_couple |> map_opt random_l in
      let msg = choice_opt
        |> map_opt (fun s -> "Et c'est décidé ! Ce sera " ^ s)
        |? "Malheureusement, il n'est pas possible de parser votre indécision, même avec les dernières percées de nos IA multi-cloud-ready bare metal. Faîtes un effort !"
      in
      Lwt.return (some msg)
    )
  in
  Command.make_simple
    ~descr:"Faire un choix entre deux options"
    ~prefix:"choix"
    ~prio:10
    cmd_function

let plugin = Plugin.of_cmd cmd_choice
