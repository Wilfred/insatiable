type variable = int

type binding = variable * bool

let string_of_binding (v, b) =
  "(" ^ (string_of_int v) ^ ", " ^ (string_of_bool b) ^ ")";;

type assignment = binding list

let pp_list formatter items =
  let parts = List.map formatter items in
  "[" ^ (String.concat "; " parts) ^ "]";;

let string_of_assignment (a: assignment) = pp_list string_of_binding a;;

type literal = Var of variable | NegatedVar of variable

let string_of_literal l =
  match l with
    Var v -> "Var " ^ (string_of_int v)
  | NegatedVar v -> "NegatedVar " ^ (string_of_int v);;

(* Literals joined by OR. *)
type clause = literal list
(* TODO: need to enforce that clauses never contain duplicates. *)

let string_of_clause (c: clause) = pp_list string_of_literal c;;

type clause_result =
  | ClauseUnsatisfiable
  | ClauseSatisfied
  | StillRequires of literal list;;

(* Clauses joined by AND. *)
type expr = clause list

let string_of_expr (e: expr) = pp_list string_of_clause e;;

let string_of_expr_option (e: expr option) =
  match e with
    Some e' -> "Some " ^ (string_of_expr e')
  | None -> "None";;

type sat_result = Unsatisfiable | Satisfiable of assignment

let possible_bindings (cl: clause) : binding list =
  List.map
    (fun var -> match var with
         (Var v) -> (v, true)
       | (NegatedVar nv) -> (nv, false)
    )
    cl;;

type literal_match = Matches | DoesntMatch | Unrelated

let str_of_literal_match lm =
  match lm with
    Matches -> "Matches"
  | DoesntMatch -> "DoesntMatch"
  | Unrelated -> "Unrelated";;

let str_of_literal_match_list lml =
  pp_list str_of_literal_match lml;;

let literal_matches (b: binding) (l: literal) : literal_match =
  let (var, var_val) = b in
  match l with
    (Var v) when v = var ->
    if var_val then Matches else DoesntMatch
  | (NegatedVar v) when v = var ->
    if var_val then DoesntMatch else Matches
  | _ -> Unrelated;;

(* Remove all instances of v from items. *)
let rec drop_value v items =
  List.filter ((<>) v) items;;

(* Apply this binding to this clause.

   Clauses are literals ORed together. *)
let rec clause_assign (cl: clause) (b: binding) : clause_result =
  assert(List.length cl > 0);
  let matches = List.map (literal_matches b) cl in
  if (List.mem Matches matches) then
    ClauseSatisfied
  else
    (* Assuming our clause has no duplicates, then if we have a single
       literal that doesn't match our binding, we can't satisfy this
       clause. *)
    (if (matches = [DoesntMatch]) then
       ClauseUnsatisfiable
     else
       StillRequires (List.filter (fun l -> (literal_matches b l) = Unrelated) cl));;

let rec unwrap_requires assigned_cl =
  match assigned_cl with
    [] -> []
  | (StillRequires r) :: rs -> r :: unwrap_requires rs
  | _ -> failwith "tried to unwrap a clause_result that isn't StillRequires";;
  
(* Given a SAT expression and a list of permissible assignments,
   attempt to find an assignment that satisfies the entire
   expression. *)
let rec satisfy (e: expr) (a: assignment) : assignment option =
  match e with
    [] -> Some a
  | c :: cs ->
    (* Get all the possible satisfying assignments for this first clause. *)
    let rec find_assignment binds =
      match binds with
        [] ->
        (* Ran out of possible assignments that we could try. *)
        None
      | b :: bs ->
        (* Try this assignment on the remaining clauses. *)
        let remaining = List.map (fun cl -> clause_assign cl b) cs in
        let remaining = drop_value ClauseSatisfied remaining in
        if (List.mem ClauseUnsatisfiable remaining) then
          (* Clauses are ANDed together. If any are unsatisfiable with
             the binding, just try the next one. *)
          find_assignment bs
        else
          let remaining = unwrap_requires remaining in
          match (satisfy remaining (b :: a)) with
            Some final_a -> Some final_a
          | None -> find_assignment bs

    in
    find_assignment (possible_bindings c);;

(* CNF sample files:
   https://people.sc.fsu.edu/~jburkardt/data/cnf/cnf.html
   
*)
