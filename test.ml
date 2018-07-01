(* Build with `ocamlbuild -pkg alcotest test.byte` and run with
   ./test.byte. *)

let flatten_some_list () =
  let result = Insatiable.flat_opt_list [Some 1; Some 2] in
  Alcotest.(check (option (list int))) "same lists" (Some [1; 2]) result

let flatten_mixed_list () =
  let result = Insatiable.flat_opt_list [Some 1; None; Some 2] in
  Alcotest.(check (option (list int))) "both none" None result

let test_set =
  [ ("Flatten list", `Quick, flatten_some_list)
  ; ("Flatten mixed list", `Quick, flatten_mixed_list) ]

let () = Alcotest.run "Unit tests" [("test_set", test_set)]
