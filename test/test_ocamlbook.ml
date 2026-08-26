open! Core
open! Ocamlbook

(* 

   Simple testing rules 
    - Test one behaviour at a time.
    - Give the test a behavioural name.
    - Test public results, not internal structure.
    - Include normal, boundary, and invalid cases.
    - Keep tests deterministic.
    - Use let%test_unit for values.
    - Use let%expect_test for displayed or multi-step results.

*)

let%expect_test "addition" =
  print_s [%sexp (1 + 2 : int)];
  [%expect {| 3 |}]
;;

let%test_unit "clear behavioural name" =
  let initial_order = Books.Order.order 1 Bid 100 1 in
  let initial_book = Books.Book.empty in
  let updated_book = Books.Book.add initial_book initial_order in
  let top_book = Books.Book.top updated_book Bid in
  match top_book with
  | None -> failwith "Expected a value"
  | Some (_, top_queue) ->
    (match Fqueue.peek top_queue with
     | None -> failwith "Expected a value"
     | Some top_order -> assert (Books.Order.equal top_order initial_order))
;;
