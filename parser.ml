module S = Astring.String;;

let lines = S.cuts ~sep:"\n";;

let unlines = String.concat "\n";;

let remove_comments s =
  let s_lines = lines s in
  let content_lines = List.filter (fun s -> not (S.is_prefix "c" s)) s_lines in
  unlines content_lines;;

type problem_spec =
  { num_variables : int;
    num_clauses : int;
  };;

let parse_problem (p_line: string) : problem_spec =
  let parts = S.cuts ~sep:" " p_line in
  match parts with
    [p; cnf; vars; clauses] ->
     (* TODO: check that cnf = "cnf" and likewise p. *)
     { num_variables = int_of_string vars;
       num_clauses = int_of_string clauses;
     }
  | _ ->
     (* TODO: use a result type. *)
     failwith "Wrong number of items in the problem line";;

parse_problem "p cnf 3 4";;
