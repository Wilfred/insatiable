module S = Astring.String;;

let lines = S.cuts ~sep:"\n";;

let unlines = String.concat "\n";;

let remove_comments s =
  let s_lines = lines s in
  let content_lines = List.filter (fun s -> not (S.is_prefix "c" s)) s_lines in
  unlines content_lines;;
