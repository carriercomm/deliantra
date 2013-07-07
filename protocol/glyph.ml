open CorePervasives


let codepoint chars =
  let s =
    List.rev chars
    |> BatString.of_list
    |> BatUTF8.adopt
  in
  assert (BatUTF8.length s = 1);
  BatUTF8.get s 0


(** Returns whether we can stop parsing UTF-8 characters. Once
    the first non-combining character was read, we stop. *)
let is_combining_char cp =
  match BatUChar.category cp with
  (* Combining characters (marks). *)
  | `Mc | `Me | `Mn -> true
  | _ -> false


(** Append codepoint [cp] to character [glyph] either if it is a combining
    character with zero width or if it is a non-combining character. This
    ensures that the display width of a glyph is always what the server
    indended. Note that we do not do any checks whether what the server
    intended makes sense. *)
let append glyph cp =
  let is_combining = is_combining_char cp in
  if is_combining && Ncurses.wcwidth cp != 0 then
    (* Skip non-zero-width combining characters. *)
    (glyph, -1, []), not is_combining
  else
    (cp :: glyph, -1, []), not is_combining


(** Parse a single glyph from a stream. The format is:

      foreground (1 byte)
      background (1 byte)
      combining characters (UTF-8 encoded)
      base character (UTF-8 encoded)

    There can be 0 or more combining characters and 1 base character.

    This function returns a tuple of
      (foreground : int, background : int, glyph : BatUTF8.t)
 *)
let parse_glyph_part data =
  let fg = int_of_char (CharStream.next data) in
  let bg = int_of_char (CharStream.next data) in

  let ch, _, _ =
    CharStream.foldl (fun (glyph, todo, chars) chr ->
      match todo with

      (* Start of a character. *)
      | 0 ->
          begin match BatUTF8.length0 (int_of_char chr) with
          (* Single-byte. *)
          | 1 ->
              let cp = codepoint [chr] in
              append glyph cp
          (* Multi-byte. *)
          | n ->
              (glyph, n - 1, [chr]), false
          end

      (* End of a multi-byte character. *)
      | 1 ->
          let cp = codepoint (chr :: chars) in
          append glyph cp

      (* Inside a multi-byte character. *)
      | n ->
          (glyph, n - 1, chr :: chars), false

    ) ([], 0, []) data
  in

  Types.({
    foreground = fg;
    background = bg;
    text = BatUTF8.of_enum (BatList.enum ch);
  })


(** Parses a stream of glyphs given in the format described above. In general,
    we will get 1 or 2 glyphs, depending on the display width of a glyph.
    @raise CharStream.Failure if the format is incorrect. *)
let parse_glyph data =
  CharStream.parse (fun glyphs stream ->
    parse_glyph_part data :: glyphs
  ) [] data


(** Parse a glyph string. *)
let parse data =
  CharStream.of_string data
  |> parse_glyph
