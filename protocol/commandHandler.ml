open Types

exception Uninstall
let uninstall () = Lwt.fail Uninstall


type 'a t = {
  addspell 	: 					sendfun -> 'a -> 'a Lwt.t;
  anim 		: int -> int -> int list -> 		sendfun -> 'a -> 'a Lwt.t;
  delinv 	: int -> 				sendfun -> 'a -> 'a Lwt.t;
  delitem 	: int list -> 				sendfun -> 'a -> 'a Lwt.t;
  fx 		: Types.fx list -> 			sendfun -> 'a -> 'a Lwt.t;
  ix 		: int -> int -> string ->		sendfun -> 'a -> 'a Lwt.t;
  item2 	: int -> Types.item list -> 		sendfun -> 'a -> 'a Lwt.t;
  map 		: Types.map ->	 			sendfun -> 'a -> 'a Lwt.t;
  map_scroll 	: int -> int -> 			sendfun -> 'a -> 'a Lwt.t;
  mapinfo 	: Types.mapinfo -> 			sendfun -> 'a -> 'a Lwt.t;
  msg 		: int -> string -> string -> 		sendfun -> 'a -> 'a Lwt.t;
  newmap 	: 					sendfun -> 'a -> 'a Lwt.t;
  player 	: int -> int -> int -> string -> 	sendfun -> 'a -> 'a Lwt.t;
  query 	: int -> string -> 			sendfun -> 'a -> 'a Lwt.t;
  sc	 	: Types.sound list -> 			sendfun -> 'a -> 'a Lwt.t;
  setup 	: Types.setup -> 			sendfun -> 'a -> 'a Lwt.t;
  stats 	: Statistics.t -> 			sendfun -> 'a -> 'a Lwt.t;
  upditem 	: int -> int -> Types.item -> 		sendfun -> 'a -> 'a Lwt.t;
  (*void updspell () { } // TODO*)
  version 	: Types.server_version -> 		sendfun -> 'a -> 'a Lwt.t;

  (* ext *)
  reply		: int -> Yojson.Basic.json list ->	sendfun -> 'a -> 'a Lwt.t;
  nicklist	: Types.nick list -> string list ->	sendfun -> 'a -> 'a Lwt.t;
  capabilities	: Capabilities.t list ->		sendfun -> 'a -> 'a Lwt.t;
  nonces	: string -> string ->			sendfun -> 'a -> 'a Lwt.t;
  channel_info	: Types.channel_info ->			sendfun -> 'a -> 'a Lwt.t;
  ambient_music	: int list ->				sendfun -> 'a -> 'a Lwt.t;
  command_list	: int list ->				sendfun -> 'a -> 'a Lwt.t;
}


let default = let open Lwt in {
  addspell	= (fun				send a -> return a);
  anim		= (fun num flags faces		send a -> return a);
  delinv	= (fun tag			send a -> return a);
  delitem	= (fun items			send a -> return a);
  fx		= (fun faces			send a -> return a);
  ix		= (fun num ofs data		send a -> return a);
  item2		= (fun location items		send a -> return a);
  map		= (fun map			send a -> return a);
  map_scroll	= (fun dx dy			send a -> return a);
  mapinfo	= (fun mapinfo			send a -> return a);
  msg		= (fun level channel msg	send a -> return a);
  newmap	= (fun				send a -> return a);
  player	= (fun tag weight face name	send a -> return a);
  query		= (fun flags prompt		send a -> return a);
  sc		= (fun sounds			send a -> return a);
  setup		= (fun setup			send a -> return a);
  stats		= (fun stats			send a -> return a);
  upditem	= (fun flags location item	send a -> return a);
  (*void updspell () { } // TODO*)
  version	= (fun server_version		send a -> return a);

  reply		= (fun id args			send a -> return a);
  nicklist	= (fun update delete		send a -> return a);
  capabilities	= (fun capab			send a -> return a);
  nonces	= (fun a b			send a -> return a);
  channel_info	= (fun info			send a -> return a);
  ambient_music	= (fun faces			send a -> return a);
  command_list	= (fun faces			send a -> return a);
}
