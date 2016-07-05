
let triggers = [
]

let apply nick msg =
  Lwt_list.fold_left_s (fun ret f ->
    match ret with
    | Some _ -> Lwt.return ret
    | None -> f nick msg
  ) None triggers
