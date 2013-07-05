install Program ".DEFAULT" [
  (* Target *)
  Name		"dclient";

  (* Sources *)
  Modules [
    "Client";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "deliantra";
    "lwt.syntax";
  ];

  (* Camlp4 *)
  Flags [
    "client.ml",	"-syntax camlp4o";
  ];
]
