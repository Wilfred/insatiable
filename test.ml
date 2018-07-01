(* Build with `ocamlbuild -pkg alcotest test.byte` and run with
   ./test.byte. *)

let flatten_some_list () =
  let result = Insatiable.flat_opt_list [Some 1; Some 2] in
  Alcotest.(check (option (list int))) "same lists" (Some [1; 2]) result

let flatten_mixed_list () =
  let result = Insatiable.flat_opt_list [Some 1; None; Some 2] in
  Alcotest.(check (option (list int))) "both none" None result;;

let simplify_single_clause () =
  let result = Insatiable.satisfy [[Var 1]] [] in
  let success = result = (Some [(1, true)]) in
  Alcotest.(check bool) "expected result" success true

let simplify_unsatisfiable () =
  (* x and not x. *)
  let result = Insatiable.satisfy [[Var 1]; [NegatedVar 1]] [] in
  let success = result = None in
  Alcotest.(check bool) "expected result" success true

let test_set =
  [ ("Flatten list", `Quick, flatten_some_list)
  ; ("Flatten mixed list", `Quick, flatten_mixed_list)
  ; ("Simplify single clause", `Quick, simplify_single_clause)
  ; ("Simplify unsatisfiable", `Quick, simplify_unsatisfiable)
  ]

let () = Alcotest.run "Unit tests" [("test_set", test_set)]
