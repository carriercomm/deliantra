let encode str =
  (* Encode the binary string as UTF-8 *)
  let buf = BatUTF8.Buf.create (String.length str * 2) in
  for i = 0 to String.length str - 1 do
    BatUTF8.Buf.add_char buf (BatCamomile.UChar.chr (int_of_char str.[i]))
  done;

  let encoded = BatUTF8.Buf.contents buf in
  assert (BatUTF8.length encoded == String.length str);

  (encoded :> string)


let decode n =
  let n = BatUTF8.adopt n in
  let b = Buffer.create (BatUTF8.length n) in
  BatUTF8.iter (fun c ->
    Buffer.add_char b (BatUChar.to_char c)
  ) n;
  Buffer.contents b
