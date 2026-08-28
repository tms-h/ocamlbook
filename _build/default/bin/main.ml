open! Core
open! Async
open! Ocamlbook

let handle_client _addr reader writer =
  let%bind msg = Reader.read_line reader in
  match msg with
  | `Eof -> return ()
  | `Ok msg ->
    printf "got: %s\n" msg;
    Writer.write_line writer "OK";
    return ()
;;

let main () =
  print_endline "start\n%!";
  let%bind _server =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port 8067)
      handle_client
  in
  print_endline "listening\n%!";
  Deferred.never ()
;;

let command = Command.async ~summary:"toy exchange" (Command.Param.return main)
let () = Command_unix.run command
