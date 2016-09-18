#!/usr/bin/env ocaml

#use "topfind";;
#require "uutf";;
#require "sequence";;
#require "containers";;

open Sequence.Infix;;

(* repair unicode text that was broken by {!String.lowercase} *)

(* is [s] a single unicode char *)
let is_utf8_char s =
  let d = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  match Uutf.decode d with
    | `Uchar c -> Uutf.decode d = `End
      (* Uutf.cp_to_string c = s *)
    | `Malformed _ -> false
    | `Await -> assert false
    | `End -> false

let input = CCIO.with_in "factoids.json" CCIO.read_all ;;

let l = Uutf.String.fold_utf_8 (fun acc i c -> (i,c)::acc) [] input |> List.rev;;

Printf.printf "there are %d broken chunks\n%!"
  (l |> List.filter (function (_,`Malformed _) -> true | _ -> false) |> List.length);;

(* function that repairs a single malformed string *)
let repair1 s: string option =
  let rec aux l = match l with
    | [] -> Sequence.return []
    | c :: tail ->
      Sequence.of_list [c; Char.uppercase c] >>= fun c ->
      aux tail >|= fun tl ->
      c::tl
  in
  CCString.to_list s
  |> aux
  |> Sequence.map CCString.of_list
  |> Sequence.filter is_utf8_char
  |> Sequence.head
;;

let is_ascii c = Char.code c  < 128

(* repair a malformed chunk. First tries to repair it fully, then tries
   to split and repair it recursively *)
let rec repair s: string option =
  let len = String.length s in
  2 -- len
  |> Sequence.filter_map
    (fun i ->
       let s1 = String.sub s 0 i in
       let s2 = String.sub s i (len-i) in
       match repair1 s1 with
         | Some u1 when s2="" -> Some u1
         | Some u1 when CCString.for_all is_ascii s2 -> Some (u1 ^ s2)
         | Some u1 ->
           begin match repair s2 with
             | Some u2 -> Some (u1 ^ u2)
             | None -> None
           end
         | None -> None)
  |> Sequence.head
;;

let res =
  let buf = Buffer.create (String.length input) in
  let n_repair = ref 0 in
  let n_fail = ref 0 in
  List.iter
    (function
      | (_, `Uchar c) -> Uutf.Buffer.add_utf_8 buf c
      | (_, `Malformed s) ->
        match repair s with
          | Some s' ->
            Printf.printf "repaired %S into %S\n%!" s s';
            incr n_repair;
            Buffer.add_string buf s'
          | None ->
            Printf.printf "could not repair %S\n%!" s;
            incr n_fail;
            Buffer.add_string buf s
    )
    l;
  Printf.printf "num repair: %d, num fail: %d\n%!" !n_repair !n_fail;
  Buffer.contents buf ;;

CCIO.with_out "factoids.new.json" (fun oc -> output_string oc res);;
