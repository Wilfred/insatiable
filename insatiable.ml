
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
  
let try_satisfy e =
  let rec loop assignments =
    match e with
      [] -> Satisfiable assignments
    | [_] -> loop []
    | _ -> Unsatisfiable
  in
  loop [];;

try_satisfy []
