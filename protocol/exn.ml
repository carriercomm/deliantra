open CorePervasives


type t =
  | Invalid_format	of string
  | Crypto_error	of string
  | Protocol_error	of string

exception Exception of t

let fail exn = raise (Exception exn)

let invalid_format	msg = fail (Invalid_format	msg)
let crypto_error	msg = fail (Crypto_error	msg)
let protocol_error	msg = fail (Protocol_error	msg)


let to_string = function
  | Failure msg ->
      "Failure: " ^ msg
  | Sys.Break ->
      "Interrupted"
  | Exception exn ->
      begin match exn with
      | Invalid_format msg ->
          "Invalid format: " ^ msg
      | Crypto_error msg ->
          "Crypto error: " ^ msg
      | Protocol_error msg ->
          "Protocol error: " ^ msg
      end
  | exn ->
      Printexc.to_string exn


let print =
  to_string %> print_endline
