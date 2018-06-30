
type variable = int

type binding = (variable * bool)
type assignment = binding list

type literal =
  | Var of variable
  | NegatedVar of variable;;

(* Literals joined by OR. *)
type clause = literal list;;

(* Clauses joined by AND. *)
type expr = clause list;;

type sat_result =
  | Unsatisfiable
  | Satisfiable of assignment;;

let possible_bindings (cl : clause) : binding list =
  List.map
    (fun var -> match var with
         (Var v) -> (v, true)
       | (NegatedVar nv) -> (nv, false)
    )
    cl;;

let opt_map f v =
  match v with
    Some v' -> Some (f v')
  | None -> None;;

let push_head v vs =
  v::vs;;

let rec all_some items =
  match items with
    [] -> true
  | (x::xs) ->
    match x with
      (Some _) -> all_some xs
    | None -> false;;

let rec flat_opt_list (items : 'a option list) : 'a list option =
  match items with
    [] -> Some []
  | (x::xs) ->
    (match (x, flat_opt_list xs) with
      (Some x', Some xs') -> Some (x'::xs')
    | _ -> None);;
    
let rec clause_assign (cl : clause) (b : binding) : clause option =
  let (var, var_val) = b in
  match cl with
    [] -> Some cl
  | ((Var v)::vs) ->
    if v == var then
      (if var_val then
        clause_assign vs b
      else
        None)
    else
      opt_map (push_head (Var v)) (clause_assign vs b) 
  | ((NegatedVar v)::vs) ->
    if var == v then
      (if var_val then
        None
      else
        clause_assign vs b)
    else
      opt_map (push_head (NegatedVar v)) (clause_assign vs b);;

(* Apply assignment to this clause. Return the new clause if the
   assignment can be applied, or None if the clause cannot be
   satisfied with this assignment. *)
let rec simplify (cl : clause) (a : assignment) : clause option =
  match a with
    [] -> Some cl
  | (b::bs) ->
    match clause_assign cl b with
      Some cl' -> simplify cl' bs
    | None -> None;;

(* Given a SAT expression and a list of permissible assignments,
   attempt to find an assignment that satisfies the entire
   expression. *)
let rec satisfy (e : expr) (a : assignment) : assignment option =
  match e with
    [] -> Some a
  | (c::cs) ->
    (* Get all the possible satisfying assignments for this first clause. *)
    let rec find_assignment binds =
      match binds with
        [] ->
        (* Ran out of possible assignments that we could try. *)
        None
      | (b::bs) ->
        (* Try this assignment on the remaining clauses. *)
        let remaining = List.map (fun cl -> clause_assign cl b) cs in
        (match (flat_opt_list remaining) with
           (Some remaining') ->
           (* If this assignment worked for all the other clauses, keep
              going. *)
           (match (satisfy remaining' (b::a)) with
              (Some final_a) ->
              (* We managed to find an assignmentent continuing from the
                 current assignments. *)
              Some final_a
            | None ->
              (* No possible assignment from this point, try another
                 possibility. *)
              find_assignment bs)
         | None -> find_assignment bs)
        
    in
    find_assignment (possible_bindings c);;
        
satisfy [[(Var 1)]; [(NegatedVar 2)]; [(Var 2); (NegatedVar 3)]] [];;

(* CNF sample files:
   https://people.sc.fsu.edu/~jburkardt/data/cnf/cnf.html
   
*)
