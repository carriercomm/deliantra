let bswap8 a b s =
  let tmp = s.[a] in
  s.[a] <- s.[b];
  s.[b] <- tmp;
;;

let bswap64 offset s =
  for i = 0 to 3 do
    bswap8 (i + offset) (7 - i + offset) s
  done

let bswap512 s =
  for i = 0 to 7 do
    bswap64 (i * 8) s
  done


let digest msg =
  let digest = Sha512.to_bin (Sha512.string msg) in
  bswap512 digest;
  assert (String.length digest == 64);
  digest


(* Guarantees a string of length [[len]], pads with '\000' if necessary. *)
let substr s len =
  if String.length s <= len then
    s
  else
    String.sub s 0 len


let xor_str a b =
  for i = 0 to String.length b - 1 do
    a.[i] <- Char.chr (Char.code a.[i] lxor Char.code b.[i])
  done


(* Hashes a cleartext password into the binary password used in the protocol. *)
let hash_pw pw =
  (* we primarily want to protect the password itself, and
   * secondarily want to protect us against pre-image attacks.
   * we don't want to overdo it, to keep implementation simple. *)
  let pass = substr pw 64 in

  let rec loop rounds hash =
    match rounds with
    | 0 -> hash
    | n ->
        xor_str hash pass;
        loop (n - 1) (digest hash)
  in

  loop 499 (digest pass)


(* Authenticates a (hashed) password using the given nonce. *)
let auth_pw hash nonce1 nonce2 =
  assert (String.length nonce1 == 64);
  assert (String.length nonce2 == 64);

  (* simple HMAC application *)
  let digest = digest (nonce1 ^ digest (nonce2 ^ hash)) in
  assert (String.length digest == 64);

  Utf8.encode digest
