let empty = Types.({
  exp_table = [||];
  skill_info = [];
  spell_paths = [];
  commands = IntMap.empty;

  partial_faces = IntMap.empty;
})


let request send env face f =
  let open Types in
  let req = send "askface" (string_of_int face) in

  req, { env with face_requests = IntMap.add face f env.face_requests }


let fold_left_p f env l =
  List.fold_left (fun (threads, env) elt ->
    let thread, env = f env elt in
    (thread :: threads, env)
  ) ([], env) l


let get_partial_finish num data partial_faces =
  try
    (* look up the partial face *)
    let partial = IntMap.find num partial_faces in
    (* remove it from the map *)
    let partial_faces = IntMap.remove num partial_faces in
    (* write last fragment, completing it *)
    String.blit data 0 partial 0 (String.length data);
    partial_faces, partial
  with Not_found ->
    (* this is the only fragment; the fragment is the entire face *)
    partial_faces, data


let finish_ix num data send env =
  let open Types in

  (* the entire face has arrived *)
  let partial_faces, complete =
    get_partial_finish num data env.res.partial_faces
  in

  (* get the handler for this request *)
  let handler = IntMap.find num env.face_requests in

  (* and call it *)
  lwt env = handler complete send env in

  Lwt.return { env with
    (* remove the face request handler *)
    face_requests = IntMap.remove num env.face_requests;
    (* update the map *)
    res = { env.res with partial_faces };
  }


let get_partial_more num ofs len partial_faces =
  try
    (* look up the partial face *)
    partial_faces, IntMap.find num partial_faces
  with Not_found ->
    (* this is the first fragment; create a partial face *)
    let partial = String.make (len + ofs) 'A' in
    (* add it to the partial faces map *)
    let partial_faces = IntMap.add num partial partial_faces in
    partial_faces, partial


let partial_ix num ofs data env =
  let open Types in

  let len = String.length data in

  (* a partial face arrived *)
  let partial_faces, partial =
    get_partial_more num ofs len env.res.partial_faces
  in

  (* write partial data to the face *)
  String.blit data 0 partial ofs len;

  Lwt.return { env with
    (* update the map *)
    res = { env.res with partial_faces };
  }


let handler = CommandHandler.({ default with

(*
  ambient_music = (fun faces send env ->
    let reqs, env =
      fold_left_p (fun env face ->
        request send env face (fun data send env ->
          Lwt.return env
        )
      ) env faces
    in

    Lwt.join reqs >>
    Lwt.return env
  );
*)

  command_list = (fun faces send env ->
    let reqs, env =
      fold_left_p (fun env face ->
        request send env face (fun data send env ->
          let open Types in

          let commands = Ext_j.string_list_of_string data in

          Lwt.return { env with
            res = { env.res with
              commands = IntMap.add face commands env.res.commands;
            }
          }
        )
      ) env faces
    in

    Lwt.join reqs >>
    Lwt.return env
  );

  ix = (fun num ofs data send env ->
    match ofs with
    | 0 ->
        finish_ix num data send env

    | ofs ->
        partial_ix num ofs data env
  );

})
