open! Core

module Order = struct
  type side =
    | Bid
    | Ask
  [@@deriving equal, sexp]

  type t =
    { id : int
    ; side : side
    ; price : int
    ; qty : int
    }
  [@@deriving equal, sexp]

  let order id side price qty = { id; side; price; qty }
end

module Book = struct
  type book = Order.t Fqueue.t Map.M(Int).t
  type t = book

  let empty = Map.empty (module Int)

  let add (book : book) (order : Order.t) =
    let possible_queue = Map.find book order.price in
    let queue =
      match possible_queue with
      | None -> Fqueue.empty
      | Some queue -> queue
    in
    Map.set book ~key:order.price ~data:(Fqueue.enqueue queue order)
  ;;

  let top book (side : Order.side) =
    match side with
    | Bid -> Map.max_elt book
    | Ask -> Map.min_elt book
  ;;
end
