let create () =
  Types.({
    res			= Resources.empty;
    nonces		= None;
    setup		= Handshake.default_setup;
    tiles		= IntMap.empty;
    map			= GameMap.create 0 0;
    stats		= Statistics.empty;
    ext_requests	= IntMap.empty;
    face_requests	= IntMap.empty;
  })
