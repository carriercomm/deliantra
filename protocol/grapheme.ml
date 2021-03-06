open CorePervasives
open CamomileLibraryDefault.Camomile


let codepoint chars =
  let s =
    List.rev chars
    |> BatString.of_list
  in
  UTF8.validate s;
  assert (UTF8.length s = 1);
  UTF8.get s 0


(** Returns whether we can stop parsing UTF-8 characters. Once
    the first non-combining character was read, we stop. *)
let is_combining_char cp =
  match UCharInfo.general_category cp with
  (* Combining characters (marks). *)
  | `Mc | `Me | `Mn -> true
  | _ -> false


(** Append codepoint [cp] to character [grapheme] either if it is a combining
    character with zero width or if it is a non-combining character. This
    ensures that the display width of a grapheme is always what the server
    indended. Note that we do not do any checks whether what the server
    intended makes sense. *)
let append grapheme cp =
  let is_combining = is_combining_char cp in
  if is_combining && Ncurses.wcwidth cp != 0 then
    (* Skip non-zero-width combining characters. *)
    (grapheme, -1, []), not is_combining
  else
    (cp :: grapheme, -1, []), not is_combining


(** Parse a single grapheme from a stream. The format is:

      foreground (1 byte)
      background (1 byte)
      combining characters (UTF-8 encoded)
      base character (UTF-8 encoded)

    There can be 0 or more combining characters and 1 base character.

    This function returns a tuple of
      (foreground : int, background : int, grapheme : BatUTF8.t)
 *)
let parse_grapheme_part data =
  let fg = int_of_char (CharStream.next data) in
  let bg = int_of_char (CharStream.next data) in

  let ch, _, _ =
    CharStream.foldl (fun (grapheme, todo, chars) chr ->
      match todo with

      (* Start of a character. *)
      | 0 ->
          begin match BatUTF8.length0 (int_of_char chr) with
          (* Single-byte. *)
          | 1 ->
              let cp = codepoint [chr] in
              append grapheme cp
          (* Multi-byte. *)
          | n ->
              (grapheme, n - 1, [chr]), false
          end

      (* End of a multi-byte character. *)
      | 1 ->
          let cp = codepoint (chr :: chars) in
          append grapheme cp

      (* Inside a multi-byte character. *)
      | n ->
          (grapheme, n - 1, chr :: chars), false

    ) ([], 0, []) data
  in

  let ch = Array.of_list ch in

  Types.({
    foreground = fg;
    background = bg;
    text = UTF8.init (Array.length ch) (Array.get ch);
  })


(** Parses a stream of graphemes given in the format described above. In general,
    we will get 1 or 2 graphemes, depending on the display width of a grapheme.
    @raise CharStream.Failure if the format is incorrect. *)
let parse_grapheme data =
  CharStream.parse (fun graphemes stream ->
    parse_grapheme_part data :: graphemes
  ) [] data


(** Parse a grapheme string. *)
let parse data =
  CharStream.of_string data
  |> parse_grapheme
