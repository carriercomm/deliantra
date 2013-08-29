let width  = 29
let height = 27


let clear = lazy (print_string "\027[H\027[2J")

let draw_map win =
  Lazy.force clear;
  print_string "\027[0;0H";

  print_string "╔";
  for x = 0 to width - 1 do
    print_string "══";
  done;
  print_string "╗\n";

  for y = 0 to height - 1 do
    print_string "║";
    for x = 0 to width - 1 do
      List.iter print_string win.(y * width + x)
    done;
    print_string "║\n";
  done;

  print_string "╚";
  for x = 0 to width - 1 do
    print_string "══";
  done;
  print_string "╝\n";

  flush stdout


let grapheme text =
  BatUTF8.validate text;
  Types.([
    { foreground = 1; background = 13; text; };
    { foreground = 1; background = 13; text; };
  ])


let space   = grapheme " "
let player  = grapheme "@"
let no_face = grapheme "\027[5;1;33m╳"


let select_face = function
  | _, _, face when face != 0 -> face
  | _, face, _ when face != 0 -> face
  | face, _, _ when face != 0 -> face
  | _ -> 0


let i = ref 0
let handler = CommandHandler.({
  default with

  map = fun map send env ->
    let open Types in

    incr i;
    if !i > 100000 then
      exit 0;

    let win = Array.make (height * width) ["__"]

    and playerx = (width  - (if width  mod 2 == 0 then 1 else 0)) / 2
    and playery = (height - (if height mod 2 == 0 then 1 else 0)) / 2

    and offx = (map.w - width ) / 2
    and offy = (map.h - height) / 2

    in

    for y = 0 to height - 1 do
      let mapy = y + map.y + offy in
      if Deque.has map.rows mapy then
        let row = Deque.get map.rows mapy in

        for x = 0 to width - 1 do
          let mapx = x + map.x + offx in
          if Deque.has row mapx then
            let cell = Deque.get row mapx in

            let face = select_face cell.layers in

            let grapheme =
              if x == playerx && y == playery then
                player
              else if face != 0 then
                IntMap.find_default no_face face env.tiles
              else
                space
            in

            win.(y * width + x) <- List.rev_map (Render.grapheme cell.cell_darkness) grapheme
        done
    done;

    (*draw_map win;*)

    Lwt.return env

})
