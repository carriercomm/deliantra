open CorePervasives


let default_setup = Types.({
  excmd      = 1;
  extcmd     = 2;
  extmap     = 1;
  frag       = 1;
  fxix       = 3;
  itemcmd    = 2;
  lzf        = 0;
  map1acmd   = 1;
  map1cmd    = 0;
  mapinfocmd = 1;
  mapsize    = "31x31";
  msg        = 1;
  spellmon   = 2;
  tileset    = 0;
})


let initiate send =
  let client_version = Types.({
    protver = 1;
    client = "DClient";
    clientver = "0.2";
    osver = Sys.os_type;
  }) in

  send "version" (Message_j.string_of_client_version client_version)


let request_resources send env =
  let open Types in

  Ext.resource send env ["exp_table"; "skill_info"; "spell_paths"] (fun result send env ->
    match result with
    | [`Int exp_table; `Int skill_info; `Int spell_paths] ->
        let exp_req, env =
          Resources.request send env exp_table (fun data send env ->
            let exp_table = Resources_j.exp_table_of_string data in
            Lwt.return { env with res = { env.res with exp_table } }
          )
        in

        let skill_req, env =
          Resources.request send env skill_info (fun data send env ->
            let skill_info = Resources_j.skill_info_of_string data in
            Lwt.return { env with res = { env.res with skill_info } }
          )
        in

        let spell_req, env =
          Resources.request send env spell_paths (fun data send env ->
            let spell_paths = Resources_j.spell_paths_of_string data in
            Lwt.return { env with res = { env.res with spell_paths } }
          )
        in

        (* Send the requests. *)
        Lwt.join [exp_req; skill_req; spell_req] >>

        Lwt.return env

    | _ ->
        Exn.invalid_format "resource reply"
  )


let handler create user hash_pw = CommandHandler.({ default with

  version = (fun version send env ->
    let open Types in

    let fx_want = <:json<{"0":1,"3":1,"5":1,"6":1}>> in
    Ext.sendi send env "fx_want" fx_want (fun result send env ->
      Lwt.return env
    )
  );


  capabilities = (fun capab send env ->
    let open Types in
    assert (List.length capab > 0);

    (* Find the text tileset *)
    let fs_id =
      List.fold_left (fun id -> function
        | Capabilities.Tileset tilesets ->
            assert (List.length tilesets > 0);
            CoreList.foldl_until (function
              | { fs_flags; fs_id; fs_edgelen = 2 } when fs_flags land 2 != 0 -> fs_id
              | _ -> -1
            ) id tilesets
      ) (-1) capab
    in

    if fs_id == -1 then (
      print_endline "could not find default text tileset; available tilesets:";
      List.iter (function
        | Capabilities.Tileset tilesets ->
            List.iter (function
              | { fs_name } ->
                  Printf.printf " - %d: %s\n" fs_id fs_name
            ) tilesets
      ) capab;

      assert false
    );

    (* Request the tileset id. *)
    let setup = { default_setup with tileset = fs_id } in
    let setup_req = send "setup" (Message_j.string_of_setup setup) in

    (* Request exp table, etc. *)
    let resource_req, env = request_resources send env in

    (* Enable nick monitoring. *)
    let nickmon_req, env =
      Ext.nickmon send env true (fun result send env ->
        Lwt.return env
      )
    in

    (* Send setup and resource requests. *)
    Lwt.join [setup_req; resource_req; nickmon_req] >>

    Lwt.return env
  );


  nonces = (fun a b send env ->
    if a = b then
      Exn.crypto_error "nonces are equal"
    else if String.length a < 32 then
      Exn.crypto_error "first nonce is too short"
    else if String.length b < 32 then
      Exn.crypto_error "second nonce is too short";

    Lwt.return { env with Types.nonces = Some (a, b) }
  );


  setup = (fun setup send env ->
    let open Types in

    let env =
      Scanf.sscanf setup.mapsize "%dx%d" (fun w h ->
        { env with
          setup;
          map = { env.map with
            w;
            h;
          };
        }
      )
    in

    if not create then (
      match env.nonces with
      | None ->
          Exn.protocol_error "got setup before nonces"

      | Some (n1, n2) ->
          let login = <:json<[$str:user$, $str:Auth.auth_pw hash_pw n1 n2$]>> in

          Ext.sendi send env "login" login (fun result send env ->
            match result with
            | [ <:json<0>>; <:json<$str:msg$>> ] ->
                failwith "login rejected"

            | [ <:json<1>>; <:json<$str:msg$>> ] ->
                send "command" "wizpass" >>
                send "command" "goto *scorn" >>
                (*send "command" "run 4" >>*)

                Lwt.return env

            | _ ->
                Exn.invalid_format "login reply"
          )
    ) else (
      let login = <:json<[$str:user$, $str:Utf8.encode hash_pw$]>> in

      Ext.sendi send env "create_login" login (fun result send env ->
        match result with
        | [ <:json<0>>; <:json<$str:msg$>> ] ->
            failwith ("user creation rejected: " ^ msg)

        | [ <:json<1>>; <:json<$str:msg$>> ] ->
            Lwt.return env

        | _ ->
            Exn.invalid_format "login reply"
      )
    )
  );

})
