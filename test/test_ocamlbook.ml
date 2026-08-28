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

let%test "top of empty book" =
  let book = Exchange.Book.empty in
  match Exchange.Book.best Bid book with
  | None -> true
  | Some x -> failwith "unexpected"
;;

let%test_unit "top of book" =
  let initial_order = Exchange.Order.order 1 Bid 100 1 in
  let initial_book = Exchange.Book.empty in
  let updated_book = Exchange.Book.add initial_book initial_order in
  let top_book = Exchange.Book.best Bid updated_book in
  match top_book with
  | None -> failwith "Expected a value"
  | Some (_, top_queue) ->
    (match Fqueue.peek top_queue with
     | None -> failwith "Expected a value"
     | Some top_order -> assert (Exchange.Order.equal top_order initial_order))
;;
