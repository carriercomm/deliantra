let log = Printf.sprintf

let (>|) log a =
  Lwt_log.notice_f "[%.6f] %s" (Unix.gettimeofday ()) log >> Lwt.return a


let string_of_jsons l =
  String.concat "," (List.map Yojson.Basic.to_string l)


let handler = CommandHandler.({
  addspell	= (fun				send a -> log "recv: addspell"								>| a);
  anim		= (fun num flags faces		send a -> log "recv: anim"								>| a);
  delinv	= (fun tag			send a -> log "recv: delinv %d" tag							>| a);
  delitem	= (fun items			send a -> log "recv: delitem"								>| a);
  fx		= (fun faces			send a -> log "recv: fx"								>| a);
  ix		= (fun num ofs data		send a -> log "recv: ix %d %d (len=%d)" num ofs (String.length data)			>| a);
  item2		= (fun location items		send a -> log "recv: item2"								>| a);
  map		= (fun map			send a -> log "recv: map"								>| a);
  map_scroll	= (fun dx dy			send a -> log "recv: map_scroll %d %d" dx dy						>| a);
  mapinfo	= (fun mapinfo			send a -> log "recv: mapinfo"								>| a);
  msg		= (fun level channel msg	send a -> log "recv: msg %d %s %s" level channel msg					>| a);
  newmap	= (fun				send a -> log "recv: newmap"								>| a);
  player	= (fun tag weight face name	send a -> log "recv: player %d %d %d %s" tag weight face name				>| a);
  query		= (fun flags prompt		send a -> log "recv: query %d %s" flags prompt						>| a);
  sc		= (fun sounds			send a -> log "recv: sc (%d sounds)" (List.length sounds)				>| a);
  setup		= (fun setup			send a -> log "recv: setup"								>| a);
  stats		= (fun stats			send a -> log "recv: stats"								>| a);
  upditem	= (fun flags location item	send a -> log "recv: upditem %d %d [item]" flags location				>| a);
  (*void updspell () { } // TODO*)
  version	= (fun server_version		send a -> log "recv: version"								>| a);

  (* ext *)
  reply		= (fun id args			send a -> log "recv: ext reply %d [%s]" id (string_of_jsons args)			>| a);
  nicklist	= (fun update delete		send a -> log "recv: ext nicklist"							>| a);
  capabilities	= (fun capab			send a -> log "recv: ext capabilities"							>| a);
  nonces	= (fun a b			send a -> log "recv: ext nonces"							>| a);
  channel_info	= (fun info			send a -> log "recv: ext channel_info %s" (Ext_j.string_of_channel_info info)		>| a);
  ambient_music	= (fun faces			send a -> log "recv: ext ambient_music %s" (Ext_j.string_of_int_list faces)		>| a);
  command_list	= (fun faces			send a -> log "recv: ext command_list %s" (Ext_j.string_of_int_list faces)		>| a);
})
