open CorePervasives


(* XXX: Server's FLOAT_MULTF *)
let float_multf = 100000.0


let read_u08 stream =
  int_of_char (CharStream.next stream)


let read_u16 stream =
  let hi = read_u08 stream in
  let lo = read_u08 stream in

  (hi lsl  8) lor lo


let read_u24 stream =
  let hi = read_u08 stream in
  let lo = read_u16 stream in

  (hi lsl 16) lor lo


let read_u32 stream =
  let hi = read_u16 stream in
  let lo = read_u16 stream in

  Int32.(shift_left (of_int hi) 16 |> logor (of_int lo))


let read_s08 stream =
  let result =
    match read_u08 stream with
    | u08 when u08 > 0x7f ->
        (lnot u08) land 0xff - 1
    | u08 ->
        u08
  in

  result


let read_s16 stream =
  let result =
    match read_u16 stream with
    | u16 when u16 > 0x7fff ->
        (lnot u16) land 0xffff - 1
    | u16 ->
        u16
  in

  result


let read_s32 stream =
  let result =
    match read_u32 stream with
    | u32 when u32 > 0x7fffffffl ->
        Int32.((lognot u32) |> logand 0xffffffffl |> pred)
    | u32 ->
        u32
  in

  result


let read_u31 stream =
  Int32.to_int (read_u32 stream)


let read_u64 stream =
  let hi = read_u32 stream in
  let lo = read_u32 stream in

  Int64.(shift_left (of_int32 hi) 32 |> logor (of_int32 lo))


let read_string read_length stream =
  let n = read_length stream in
  CharStream.take n stream


let read_ber stream =
  CharStream.foldl (fun value chr ->
    let digit = int_of_char chr in
    let value = value lor (digit land 0x7f) in

    if digit land 0x80 != 0 then
      value lsl 7, false
    else
      value, true
  ) 0 stream


let read_string_ber = read_string read_ber
let read_string08 = read_string read_u08
let read_string16 = read_string read_u16
let read_string32 = read_string read_u31


let read_float stream =
  1.0 /. float_multf *. Int32.to_float (read_u32 stream)


let range_check name min max value =
  if min > value || value > max then
    Exn.protocol_error (Printf.sprintf "%s %d exceeds range [%d..%d]"
      name value min max)
  else
    value


let read_u08 stream =
  range_check "u08" 0 0xff (read_u08 stream)

let read_u16 stream =
  range_check "u16" 0 0x7fff (read_u16 stream)

let read_u32 stream =
  range_check "u32" 0 0x7fffffff (read_u31 stream)

let read_u64 stream =
  let result = read_u64 stream in
  assert (result <= 0x7fffffffffffffffL);
  result
