type t =
  | Tileset of Types.tileset list


let handle_tileset = function
  | `List tilesets ->
      List.map (function
        | <:json<[$int:fs_id$, $str:fs_name$, $int:fs_flags$, $int:fs_edgelen$]>> ->
            Types.({
              fs_id;
              fs_name;
              fs_flags;
              fs_edgelen;
            })
        | _ ->
            Exn.invalid_format "tile set format error"
      ) tilesets

  | _ -> Exn.protocol_error "expected list in tilesets capability"


let handle_capability key value =
  match key with
  | "tileset" -> Tileset (handle_tileset value)
  | key -> Exn.protocol_error ("unhandled capability: " ^ key)


let rec handle_capabilities caps = function
  | `String key :: value :: tl ->
      let caps = (handle_capability key value) :: caps in
      handle_capabilities caps tl
  | _::_ ->
      Exn.protocol_error "odd number of arguments to capabilities"
  | [] ->
      caps


let of_json args =
  handle_capabilities [] args
