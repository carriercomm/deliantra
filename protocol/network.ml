type recv_t = {
  recvfd   : Lwt_unix.file_descr;
  (** Socket file descriptor. *)
  recvbuf  : string;
  (** Receive buffer. *)
  position : int;
  (** Current position in the receive buffer. *)
  expected : int;
  (** Expected packet length. *)
}


let rec process_packets process env comm =
  let comm = { comm with
    expected =
      if comm.position >= 2 && comm.expected = 0 then
        (int_of_char comm.recvbuf.[0] lsl 8) lor int_of_char comm.recvbuf.[1]
      else
        comm.expected
  } in

  if comm.position < comm.expected + 2 then
    Lwt.return (env, comm)
  else
    (* process one packet *)
    lwt env =
      let packet = String.sub comm.recvbuf 2 comm.expected in
      process env packet
    in

    (* shift the remaining packet data *)
    let position = comm.position - comm.expected - 2 in

    String.blit comm.recvbuf (comm.expected + 2) comm.recvbuf 0 position;
    let comm = {
      comm with position;
      expected = 0;
    } in

    process_packets process env comm


let rec recv_on comm process env =
  lwt rlen = Lwt_unix.read
    comm.recvfd
    comm.recvbuf
    comm.position
    (String.length comm.recvbuf - comm.position)
  in

  if rlen = 0 then
    Lwt.return ()
  else
    let comm = { comm with position = comm.position + rlen } in

    (* Process ready packets in order. *)
    lwt env, comm = process_packets process env comm in

    (* Wait for more data. *)
    recv_on comm process env


type send_t = {
  sendfd  : Lwt_unix.file_descr;
  (** Socket file descriptor. *)
}


let concat = function
  | cmd, "" ->
      cmd
  | cmd, arg ->
      cmd ^ " " ^ arg


let write comm s =
  Lwt_unix.write comm.sendfd s 0 (String.length s)

let send_on comm cmd arg =
  lwt () = Lwt_log.notice_f "[%.6f] send: %s %s" (Unix.gettimeofday ()) cmd arg in

  let data = concat (cmd, arg) in

  let wlen = String.length data in
  let lencode = "  " in
  lencode.[0] <- char_of_int (wlen lsr 8);
  lencode.[1] <- char_of_int (wlen land 0xff);

  lwt len = write comm (lencode ^ data) in
  assert (len == 2 + wlen);

  Lwt.return ()


let connect server port =
  let open Lwt_unix in

  lwt () = Lwt_log.notice "creating socket" in
  let sockfd = socket PF_INET SOCK_STREAM 0 in

  lwt () = Lwt_log.notice_f "resolving hostname: %s" server
  and serverent = gethostbyname server in

  let addr = ADDR_INET (serverent.h_addr_list.(0), port) in
  lwt () = Lwt_log.notice_f "connecting to: %s:%d" server port
  and () = connect sockfd addr in

  Lwt.return (
    recv_on {
      recvfd = sockfd;
      recvbuf = String.create 65538;
      position = 0;
      expected = 0;
    },
    send_on {
      sendfd = sockfd;
    }
  )
