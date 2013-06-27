type 'a parsefun = 'a -> string -> 'a CommandHandler.t list -> 'a Lwt.t


type 'a t = {
  parsers  : 'a parsefun StringMap.t;
  handlers : 'a CommandHandler.t list;
}


let parse this env data =
  let cmd, arg =
    try
      BatString.split data " "
    with Not_found ->
      data, data
  in

  let env =
    let cmdparser =
      try
        StringMap.find cmd this.parsers
      with Not_found ->
        Exn.protocol_error ("unhandled command: " ^ String.escaped data)
    in

    try_lwt
      cmdparser env arg this.handlers
    with exn ->
      Printf.printf "Exception in cmd %s:\n  %s\nwhile processing\n  %s\n"
        cmd (Exn.to_string exn) (String.escaped data);
      Lwt.fail exn
  in

  flush stdout;

  env


let protocol_parsers = let open CommandHandler in [

  "addspell", (
    fun env arg ->
      env, fun handler -> handler.addspell
  );

  "anim", (
    fun env arg ->
      let stream = Bitstream.of_string arg in

      unpack "nnn*" (fun anim_num flags faces ->
        env, fun handler -> handler.anim anim_num flags faces
      ) stream
  );

  "delinv", (
    fun env arg ->
      let tag = int_of_string arg in
      env, fun handler -> handler.delinv tag
  );

  "delitem", (
    fun env arg ->
      unpack "N*" (fun items ->
        env, fun handler -> handler.delitem items
      ) (Bitstream.of_string arg)
  );

  "ext", Ext.protocol_handler;

  "fx", (
    fun env arg ->
      let faces = Fx.parse_fx arg in

      let open Types in
      let env =
        List.fold_left (fun env { fx_facenum; fx_glyph } ->
          { env with tiles = IntMap.add fx_facenum fx_glyph env.tiles }
        ) env faces
      in

      env, fun handler -> handler.fx faces
  );

  "ix", (
    fun env arg ->
      unpack "wwa*" (fun num ofs data ->
        env, fun handler -> handler.ix num ofs data
      ) (Bitstream.of_string arg)
  );

  "item2", (
    fun env arg ->
      unpack "N (NNNN C/a nC Nn)*" (fun location items ->
        let items =
          List.map (fun (
            tag, itemflags, weight, face,
            names,
            anim, animspeed, nrof, itemtype) ->

              let singular, plural = BatString.split names "\000" in

              Types.({
                tag;
                itemflags;
                weight;
                face;
                singular;
                plural;
                anim;
                animspeed;
                nrof;
                itemtype;
              })
          ) items
        in

        env, fun handler -> handler.item2 location items
      ) (Bitstream.of_string arg)
  );

  "map_scroll", (
    fun env arg ->
      Scanf.sscanf arg "%d %d" (fun dx dy ->
        let env = Types.({
          env with map = GameMap.map_scroll env.map dx dy;
        }) in

        env, fun handler -> handler.map_scroll dx dy
      )
  );

  "map1a", (
    fun env arg ->
      let extmap, map =
        let open Types in

        env.setup.extmap != 0,
        env.map
      in

      let map = GameMap.map1a extmap map (CharStream.of_string arg) in

      env, fun handler -> handler.map map
  );

  "mapinfo", (
    fun env arg ->
      let token, mapflags, mapx, mapy, mapw, maph, path =
        match BatString.nsplit arg " " with

        | [token; "spatial"; flags; mapx; mapy; mapw; maph; path] ->
            token,
            int_of_string flags,
            int_of_string mapx,
            int_of_string mapy,
            int_of_string mapw,
            int_of_string maph,
            path

        | ["current"] ->
            Exn.protocol_error "mapinfo current unhandled"

        | [token; "error"] ->
            Exn.protocol_error ("mapinfo token error: " ^ token)
        | [token; "nomap"] ->
            Exn.protocol_error ("mapinfo token nomap: " ^ token)
        | [token; "unsupported"] ->
            Exn.protocol_error ("mapinfo token unsupported: " ^ token)

        | _ ->
            Exn.protocol_error ("mapinfo unknown arg: " ^ arg)
      in

      let mapinfo =
        Types.({ token; mapflags; mapx; mapy; mapw; maph; path; })
      in

      env, fun handler -> handler.mapinfo mapinfo
  );

  "msg", (
    fun env arg ->
      let level, channel, message =
        if arg.[0] = '[' then
          match Yojson.Basic.from_string arg with
          | <:json<[$int:level$, $str:channel$, $str:message$]>> ->
              level, channel, message
          | _ ->
              Exn.protocol_error "msg parse error"
        else
          let level, arg = BatString.split arg " " in
          let channel, message = BatString.split arg " " in
          int_of_string level, channel, message
      in

      env, fun handler -> handler.msg level channel message
  );

  "newmap", (
    fun env arg ->
      let open Types in
      let env = { env with map = GameMap.clear env.map } in
      env, fun handler -> handler.newmap
  );

  "player", (
    fun env arg ->
      unpack "NNNC/a" (fun tag weight face name ->
        env, fun handler -> handler.player tag weight face name
      ) (Bitstream.of_string arg)
  );

  "query", (
    fun env arg ->
      let flags, msg = BatString.split arg " " in
      let flags = int_of_string flags in
      env, fun handler -> handler.query flags msg
  );

  "sc", (
    fun env arg ->
      let sounds =
        unpack "(w/a)*" (List.fold_left (fun sounds data ->
          let sound =
            unpack "CwccC" (fun sc_type sc_facenum sc_dx sc_dy sc_volume ->
              Types.({
                sc_type;
                sc_facenum;
                sc_dx;
                sc_dy;
                sc_volume;
              })
            ) (Bitstream.of_string data)
          in

          sound :: sounds
        ) []) (Bitstream.of_string arg)
      in

      env, fun handler -> handler.sc sounds
  );

  "setup", (
    fun env arg ->
      let setup = Message_j.setup_of_string arg in
      env, fun handler -> handler.setup setup
  );

  "stats", (
    fun env arg ->
      let env = Types.({
        env with stats =
          CharStream.parse (fun stats stream ->
            let stat = Statistics.stat_of_int (Binary.read_u08 stream) in

            Statistics.update stats stat stream
          ) env.stats (CharStream.of_string arg)
      }) in

      env, fun handler -> handler.stats env.Types.stats
  );

  "upditem", (
    fun env arg ->
      let stream = Bitstream.of_string arg in

      let flags = Unpack.uint8 stream in
      let tag   = Unpack.net32 stream in

      let update read flag =
        if Item.want_update flags flag then
          read stream
        else
          0
      in

      let location  = update Unpack.net32 Item.UPD_LOCATION in
      let itemflags = update Unpack.net32 Item.UPD_FLAGS in
      let weight    = update Unpack.net32 Item.UPD_WEIGHT in
      let face      = update Unpack.net32 Item.UPD_FACE in

      let singular, plural =
        if Item.(want_update flags UPD_NAME) then
          let len = Unpack.uint8 stream in
          BatString.split (Unpack.cstring len stream) "\000"
        else
          "", ""
      in

      let anim      = update Unpack.net16 Item.UPD_ANIM in
      let animspeed = update Unpack.uint8 Item.UPD_ANIMSPEED in
      let nrof      = update Unpack.net32 Item.UPD_NROF in

      let item =
        Types.({ tag; itemflags; weight; face; anim; animspeed; nrof; itemtype = 0; singular; plural; })
      in

      env, fun handler -> handler.upditem flags location item
  );

  "version", (
    fun env arg ->
      let version = Message_j.server_version_of_string arg in
      env, fun handler -> handler.version version
  );
]


let make send =
  let parsers =
    List.fold_left (fun parsers (cmd, parse) ->
      if StringMap.mem cmd parsers then
        failwith ("parser for `" ^ cmd ^ "' already registered");
      StringMap.add cmd (fun env arg handlers ->
        let env, handlerfun = parse env arg in

        Lwt_list.fold_left_s (fun env handler ->
          (handlerfun handler) send env
        ) env handlers
      ) parsers
    ) StringMap.empty protocol_parsers
  in

  { parsers; handlers = [] }


let add_handler this handler =
  { this with handlers = handler :: this.handlers }
