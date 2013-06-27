let rec askpass create =
  lwt password = Query.handle_query 0 "Please enter your password: " in
  lwt password =
    if create then
      lwt again = Query.handle_query 0 "\rPlease enter your password again: " in
      if again <> password then (
        print_endline "\nYour passwords do not match";
        askpass create
      ) else (
        Lwt.return password
      )
    else
      Lwt.return password
  in

  Lwt.return (Auth.hash_pw password)



let askuser () =
  lwt create =
    Query.handle_query 0
      "Do you want to create a [n]ew account or log into an [e]xisting? [e] "
  in
  let create = create = "n" in

  lwt username =
    Query.handle_query 0
      "Please enter your username: "
  in

  ignore (Sys.command "stty -echo");
  lwt password = askpass create in
  ignore (Sys.command "stty echo");

  print_endline "\nLogging in...";
  Lwt.return (create, username, password)



let username = "anakinskywalker"
let username = "obiwankenobi"
let username = "quigonjinn"
let username = "jarjarbinks"
let password = Auth.hash_pw username
let create = false


let game () =
  (*lwt create, username, password = askuser () in*)

  lwt recv, send = Network.connect "testserver.deliantra.net" 13327 in

  let cmdparser =
    let (|+) = Parser.add_handler in

    Parser.make send
    |+ Ext.handler
    |+ Resources.handler
    |+ Handshake.handler create username password
    |+ MapRenderer.handler
    |+ Query.handler
    (* PacketLogger comes last *)
    |+ PacketLogger.handler
  in

  let env = Game.create () in

  Lwt.join [
    Handshake.initiate send;
    recv (Parser.parse cmdparser) env;
  ]


let main () =
  try
    Valgrind.Callgrind.instrumented
      Lwt_main.run (game ())
  with
  | exn ->
      Exn.print exn


let () =
  if Array.length Sys.argv == 2 && Sys.argv.(1) = "-backtrace" then (
    Printexc.record_backtrace true;
  );
  Sys.catch_break true;
  Unix.handle_unix_error main ()
