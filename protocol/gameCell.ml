let empty =
  Types.({
    layers		= 0, 0, 0;
    player		= 0;
    cell_darkness	= 0;
    stat_width		= 0;
    stat_hp		= 0;
    flags		= 0;
  })


let clear cell =
  let open Types in

  let layers =
    if cell.player <> 0 then
      let face0, face1, face2 = cell.layers in
      face0, face1, 0
    else
      cell.layers
  in

  { empty with layers }
