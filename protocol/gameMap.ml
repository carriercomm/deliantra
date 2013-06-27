let ensure_coord map x y =
  let open Types in
  Deque.ensure_index map.rows y Deque.create;
  let row = Deque.get map.rows y in
  Deque.ensure_index row x (fun () -> GameCell.empty);
;;


let get_cell map x y =
  let open Types in
  ensure_coord map x y;
  let row = Deque.get map.rows y in
  Deque.get row x


let set_cell map x y cell =
  let open Types in
  ensure_coord map x y;
  let row = Deque.get map.rows y in
  Deque.set row x cell;
  map


let blank map x y w h =
  ()


let create w h =
  Types.({
    rows = Deque.create ();
    x = 0;
    y = 0;
    w; h;
  })


let cell_count map =
  let open Types in

  Deque.fold_left (fun count row ->
    count + Deque.length row
  ) 0 map.rows


let clear map =
  let open Types in

  Deque.iter (fun row ->
    Deque.iteri (fun x cell ->
      Deque.set row x GameCell.empty
    ) row
  ) map.rows;

  { map with
    x = 0;
    y = 0;
  }


let rec map1a_ext_loop cell stream =
  let open Binary in
  let open Types in

  let ext = read_u08 stream in

  let cell =
    match ext land 0x7f with
    | cmd when cmd < 4 ->
        { cell with cell_darkness = 255 - ext * 64 + 1 }
    | 5 -> (* health *)
        {
          cell with
          stat_width = 1;
          stat_hp = read_u08 stream;
        }
    | 6 -> (* monster width *)
        { cell with stat_width = read_u08 stream + 1 }
    | 0x47 ->
        begin match read_u08 stream with
        | 1 -> { cell with player = read_u08 stream }
        | 2 -> { cell with player = read_u16 stream }
        | 3 -> { cell with player = read_u24 stream }
        | 4 -> { cell with player = read_u32 stream }
        | n -> ignore (CharStream.drop n stream); cell
        end
    | 8 -> (* cell flags *)
        { cell with flags = read_u08 stream }
    | _ when ext land 0x40 <> 0 -> (* unknown, multibyte => skip *)
        ignore (CharStream.drop (read_u08 stream) stream);
        cell
    | cmd ->
        Exn.protocol_error ("map1a got extcmd " ^ (string_of_int cmd))
  in

  if ext land 0x80 <> 0 then
    map1a_ext_loop cell stream
  else
    cell


let map1a_cell extmap flags cell stream =
  let open Types in

  if flags <> 0 then
    (* Default cell_darkness = 256. *)
    let cell =
      if cell.cell_darkness = 0 then
        { cell with cell_darkness = 256 }
      else
        cell
    in

    (* Parse ext command or set darkness. *)
    let cell =
      if flags land 8 <> 0 then
        if extmap then
          map1a_ext_loop cell stream
        else (* no extmap *)
          { cell with cell_darkness = Binary.read_u08 stream }
      else
        cell
    in

    (* Read faces for each layer. *)
    let read_face z face =
      if flags land (4 lsr z) <> 0 then
        Binary.read_u16 stream
      else
        face
    in

    (* Decompose layers. *)
    let face0, face1, face2 = cell.layers in

    (* Update layers. *)
    let face0 = read_face 0 face0 in
    let face1 = read_face 1 face1 in
    let face2 = read_face 2 face2 in

    (* Compose cell with updated layers. *)
    { cell with layers = face0, face1, face2 }

  else
    (* If all the flag values are 0, this then means the space is considered
     * blocked from view, and should be drawn as black.  This conserves
     * bandwidth for sending blocked spaces, which occur pretty frequently.
     * Once a space is marked as block, if it re-appears within view, the 3
     * layers should be marked is blank. *)
    GameCell.clear cell



let map1a extmap =
  CharStream.parse (fun map stream ->
    (* the coord values are flags + x + y values.  The value itself, but
     * the data represented looks like this: *)
    let coord = Binary.read_u16 stream in

    (* first 6 bits: the x coordinate *)
    let coord_x = (coord lsr 10) land 63 in
    (* next 6 bits: the y coordinate *)
    let coord_y = (coord lsr  4) land 63 in
    (* last 4 bits: MSB - true if we send darkness
     *              MSB-1 - will send floor face
     *              MSB-2 - will send intermediate face
     *              MSB-3 (aka LSB) - will send top face
     *)
    let flags = coord land 15 in

    let x, y =
      let open Types in
      map.x + coord_x,
      map.y + coord_y
    in

    let cell = get_cell map x y in

    let cell = map1a_cell extmap flags cell stream in
     
    set_cell map x y cell
  )


let map_scroll map dx dy =
  let open Types in

  if dx > 0 then blank map (map.x             ) (map.y             ) ( dx) map.h;
  if dx < 0 then blank map (map.x + map.w + dx) (map.y             ) (-dx) map.h;

  if dy > 0 then blank map (map.x             ) (map.y             ) map.w ( dy);
  if dy < 0 then blank map (map.x             ) (map.y + map.h + dy) map.w (-dy);

  { map with
    x = map.x + dx;
    y = map.y + dy;
  }
