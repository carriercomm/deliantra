open CamomileLibraryDefault.Camomile


let encode str =
  (* Encode the binary string as UTF-8 *)
  let buf = UTF8.Buf.create (String.length str * 2) in
  for i = 0 to String.length str - 1 do
    UTF8.Buf.add_char buf (UChar.chr (int_of_char str.[i]))
  done;

  let encoded = UTF8.Buf.contents buf in
  assert (UTF8.length encoded == String.length str);

  encoded


let decode n =
  UTF8.validate n;
  let b = Buffer.create (UTF8.length n) in
  UTF8.iter (fun c ->
    Buffer.add_char b (UChar.char_of c)
  ) n;
  Buffer.contents b
