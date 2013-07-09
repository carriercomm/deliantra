install Library ".DEFAULT" [
  (* Target *)
  Name		"deliantra";
  Description	"Deliantra protocol implementation";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Auth";
    "Binary";
    "Capabilities";
    "CommandHandler";
    "Deque";
    "Exn";
    "Ext_j";
    "Ext_t";
    "Ext";
    "Fx";
    "Game";
    "GameCell";
    "GameMap";
    "Grapheme";
    "Handshake";
    "Item";
    "MapRenderer";
    "Message_j";
    "Message_t";
    "Network";
    "PacketLogger";
    "Protocol";
    "Query";
    "Render";
    "Resources_j";
    "Resources_t";
    "Resources";
    "Statistics";
    "Types";
    "Utf8";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "atdgen";
    "baselib";
    "batteries";
    "json.syntax";
    "lwt.syntax";
    "lwt.unix";
    "ncurses";
    "sha";
    "unpack.syntax";
  ];

  (* Camlp4 *)
  Flags [
    "capabilities.ml",	"-syntax camlp4o";
    "ext.ml",		"-syntax camlp4o";
    "fx.ml",		"-syntax camlp4o";
    "handshake.ml",	"-syntax camlp4o";
    "network.ml",	"-syntax camlp4o";
    "packetLogger.ml",	"-syntax camlp4o";
    "protocol.ml",	"-syntax camlp4o";
    "query.ml",		"-syntax camlp4o";
    "resources.ml",	"-syntax camlp4o";
  ];
]