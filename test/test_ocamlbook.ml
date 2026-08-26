open! Core
open! Ocamlbook

let%expect_test "addition" =
  print_s [%sexp (1 + 2 : int)];
  [%expect {| 3 |}]
;;
