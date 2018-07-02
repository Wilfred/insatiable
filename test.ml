(* Build with `ocamlbuild -pkg alcotest test.byte` and run with
   ./test.byte. *)

let simplify_single_clause () =
  let result = Insatiable.satisfy [[Var 1]] [] in
  let success = result = (Some [(1, true)]) in
  Alcotest.(check bool) "expected result" success true

let simplify_unsatisfiable () =
  (* x and not x. *)
  let result = Insatiable.satisfy [[Var 1]; [NegatedVar 1]] [] in
  let success = result = None in
  Alcotest.(check bool) "expected result" success true

let simplify_or () =
  (* We can only satisfy the second clause by setting its second variable. *)
  let result = Insatiable.satisfy [[Var 1]; [NegatedVar 1; Var 2]] [] in
  let success = result = Some [(2, true); (1, true)] in
  Alcotest.(check bool) "expected result" success true

let test_set =
  [ ("Simplify single clause", `Quick, simplify_single_clause)
  ; ("Simplify unsatisfiable", `Quick, simplify_unsatisfiable)
  ; ("Simplify OR clause", `Quick, simplify_or)
  ]

let () = Alcotest.run "Unit tests" [("test_set", test_set)]
