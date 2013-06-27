open CommandHandler


let handle_extcmd env arg =
  match Ext_j.cmd_of_string arg with
  | Ext_t.ChannelInfo info ->
      env, (fun handler -> handler.channel_info info)
  | Ext_t.AmbientMusic faces ->
      env, (fun handler -> handler.ambient_music faces)


let reply_id reply =
  match ExtString.without_prefix "reply-" reply with
  | Some id -> int_of_string id
  | None    -> assert false


let handle_nicklist_update =
  let open Types in
  List.rev_map (function
    | <:json<[$str:nick$, 0, $int:count$, $str:uuid$]>> ->
        NT_PLAYER (nick, count, uuid)
    | <:json<[$str:nick$, 1, $str:description$]>> ->
        NT_SERVICE (nick, description)
    | <:json<[$str:nick$, 2, $str:ntype$]>> ->
        NT_OTHER (nick, ntype)
    | <:json<[$str:nick$, $int:n$, $str:ntype$]>> ->
        Exn.protocol_error ("unhandled nick type in update: " ^ string_of_int n)
    | _ ->
        Exn.invalid_format "nicklist update"
  )


let handle_nicklist_delete =
  let open Types in
  List.rev_map (function
    | <:json<$str:nick$>> ->
        nick
    | _ ->
        Exn.invalid_format "nicklist delete"
  )


let handle_unknown env arg =
  (* Try it manually *)
  match Yojson.Basic.from_string arg with
  | <:json<["capabilities" .. $args$]>> ->
      let caps = Capabilities.of_json args in
      env, fun handler -> handler.capabilities caps

  | <:json<[$str:reply$ .. $args$]>> when BatString.starts_with reply "reply-" ->
      let id = reply_id reply in
      env, fun handler -> handler.reply id args

  | <:json<["nicklist", [.. $update$], [.. $delete$]]>> ->
      let update = handle_nicklist_update update in
      let delete = handle_nicklist_delete delete in
      env, fun handler -> handler.nicklist update delete

  | <:json<["nonces", $str:a$, $str:b$]>> ->
      let a = Utf8.decode a in
      let b = Utf8.decode b in
      env, fun handler -> handler.nonces a b

  | <:json<["command_list" .. $faces$]>> ->
      let faces =
        List.map (function
          | <:json<$int:idx$>> ->
              idx
          | _ ->
              Exn.invalid_format "expected face numbers in argument of command_list"
        ) faces
      in
      env, fun handler -> handler.command_list faces

  | _ ->
      Exn.protocol_error ("unhandled ext cmd or bad format: " ^ arg)


let protocol_handler env arg =
  try
    handle_extcmd env arg
  with Ag_oj_run.Error msg when BatString.starts_with msg "Unsupported variant" ->
    handle_unknown env arg


let next_id requests =
  IntMap.fold (fun i _ r -> max i r) requests 0 + 1


let exti send env cmd args f =
  let open Types in

  let id = next_id env.ext_requests in

  let exti =
    match args with
    | <:json<[..$args$]>> ->
        <:json<[$str:cmd$, $int:id$ ..$args$]>>
    | arg ->
        <:json<[$str:cmd$, $int:id$, $arg$]>>
  in

  let req = send "exti" (Yojson.Basic.to_string exti) in

  req, { env with ext_requests = IntMap.add id f env.ext_requests }


let sendi send env cmd args f =
  let req, env = exti send env cmd args f in
  req >>
  Lwt.return env


let resource send env args f =
  exti send env "resource"
    (`List (List.map (fun arg -> `String arg) args))
    f


let nickmon send env opt f =
  exti send env "nickmon"
    (`Int (if opt then 1 else 0))
    f


let handler = CommandHandler.({
  default with

  reply = (fun id args send env ->
    let open Types in
    let handler = IntMap.find id env.ext_requests in

    lwt env = handler args send env in

    (* ext handlers are one-shot *)
    Lwt.return { env with
      ext_requests = IntMap.remove id env.ext_requests
    }
  );

})
