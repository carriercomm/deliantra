let handle_query flags prompt =
  Lwt_io.print prompt >>
  Lwt_io.(read_line stdin)


let handler = CommandHandler.({ default with
  query = fun flags prompt send env ->
    let open Lwt in
    handle_query flags prompt >>= send "reply" >>

    Lwt.return env
})
