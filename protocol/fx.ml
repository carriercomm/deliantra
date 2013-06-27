let face_type_of_int = let open Types in function
  | 0 (* 0 * 2 + 0 *) -> FT_IMAGE
  | 3 (* 1 * 2 + 1 *) -> FT_MUSIC
  | 5 (* 2 * 2 + 1 *) -> FT_SOUND
  | 6 (* 3 * 2 + 0 *) -> FT_RSRC
  | t -> Exn.protocol_error ("unknown face type: " ^ string_of_int t)


let string_of_face_type = let open Types in function
  | FT_IMAGE -> "FT_IMAGE"
  | FT_MUSIC -> "FT_MUSIC"
  | FT_SOUND -> "FT_SOUND"
  | FT_RSRC  -> "FT_RSRC"


let parse_fx data =
  let faces, _ =
    unpack "(w C/a)*" (List.fold_left (fun (faces, fx_type) (facenum, data) ->
      match facenum with
      | 0 ->
          if false then
            print_endline "changing face type";
          unpack "w" (fun fx_type ->
            faces, face_type_of_int fx_type
          ) (Bitstream.of_string data)

      | fx_facenum ->
          let fx_name = data in

          if false then
            Printf.printf "got fx [type=%s] %d: [len=%d] %s\n"
              (string_of_face_type fx_type)
              fx_facenum
              (String.length fx_name)
              (String.escaped fx_name);

          let faces =
            match fx_type with
            | Types.FT_IMAGE ->
                let fx_glyph = Glyph.parse fx_name in
                Types.({ fx_type; fx_facenum; fx_glyph; }) :: faces
            | _ ->
                faces
          in

          faces, fx_type
    ) ([], Types.FT_IMAGE)) (Bitstream.of_string data)
  in

  flush stdout;

  faces
