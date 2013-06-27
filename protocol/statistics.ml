(** TODO: Get this value (actually 48) from the handshake. *)
let cs_num_skills = 50


type t = Types.statistics


let string_of_stat = let open Types in function
  | CS_STAT_HP              -> "CS_STAT_HP"
  | CS_STAT_MAXHP           -> "CS_STAT_MAXHP"
  | CS_STAT_SP              -> "CS_STAT_SP"
  | CS_STAT_MAXSP           -> "CS_STAT_MAXSP"
  | CS_STAT_STR             -> "CS_STAT_STR"
  | CS_STAT_INT             -> "CS_STAT_INT"
  | CS_STAT_WIS             -> "CS_STAT_WIS"
  | CS_STAT_DEX             -> "CS_STAT_DEX"
  | CS_STAT_CON             -> "CS_STAT_CON"
  | CS_STAT_CHA             -> "CS_STAT_CHA"
  | CS_STAT_EXP             -> "CS_STAT_EXP"
  | CS_STAT_LEVEL           -> "CS_STAT_LEVEL"
  | CS_STAT_WC              -> "CS_STAT_WC"
  | CS_STAT_AC              -> "CS_STAT_AC"
  | CS_STAT_DAM             -> "CS_STAT_DAM"
  | CS_STAT_ARMOUR          -> "CS_STAT_ARMOUR"
  | CS_STAT_SPEED           -> "CS_STAT_SPEED"
  | CS_STAT_FOOD            -> "CS_STAT_FOOD"
  | CS_STAT_WEAP_SP         -> "CS_STAT_WEAP_SP"
  | CS_STAT_RANGE           -> "CS_STAT_RANGE"
  | CS_STAT_TITLE           -> "CS_STAT_TITLE"
  | CS_STAT_POW             -> "CS_STAT_POW"
  | CS_STAT_GRACE           -> "CS_STAT_GRACE"
  | CS_STAT_MAXGRACE        -> "CS_STAT_MAXGRACE"
  | CS_STAT_FLAGS           -> "CS_STAT_FLAGS"
  | CS_STAT_WEIGHT_LIM      -> "CS_STAT_WEIGHT_LIM"
  | CS_STAT_EXP64           -> "CS_STAT_EXP64"
  | CS_STAT_SPELL_ATTUNE    -> "CS_STAT_SPELL_ATTUNE"
  | CS_STAT_SPELL_REPEL     -> "CS_STAT_SPELL_REPEL"
  | CS_STAT_SPELL_DENY      -> "CS_STAT_SPELL_DENY"
  | CS_STAT_RES_PHYS        -> "CS_STAT_RES_PHYS"
  | CS_STAT_RES_MAG         -> "CS_STAT_RES_MAG"
  | CS_STAT_RES_FIRE        -> "CS_STAT_RES_FIRE"
  | CS_STAT_RES_ELEC        -> "CS_STAT_RES_ELEC"
  | CS_STAT_RES_COLD        -> "CS_STAT_RES_COLD"
  | CS_STAT_RES_CONF        -> "CS_STAT_RES_CONF"
  | CS_STAT_RES_ACID        -> "CS_STAT_RES_ACID"
  | CS_STAT_RES_DRAIN       -> "CS_STAT_RES_DRAIN"
  | CS_STAT_RES_GHOSTHIT    -> "CS_STAT_RES_GHOSTHIT"
  | CS_STAT_RES_POISON      -> "CS_STAT_RES_POISON"
  | CS_STAT_RES_SLOW        -> "CS_STAT_RES_SLOW"
  | CS_STAT_RES_PARA        -> "CS_STAT_RES_PARA"
  | CS_STAT_TURN_UNDEAD     -> "CS_STAT_TURN_UNDEAD"
  | CS_STAT_RES_FEAR        -> "CS_STAT_RES_FEAR"
  | CS_STAT_RES_DEPLETE     -> "CS_STAT_RES_DEPLETE"
  | CS_STAT_RES_DEATH       -> "CS_STAT_RES_DEATH"
  | CS_STAT_RES_HOLYWORD    -> "CS_STAT_RES_HOLYWORD"
  | CS_STAT_RES_BLIND       -> "CS_STAT_RES_BLIND"
  | CS_STAT_SKILLINFO skill -> "CS_STAT_SKILLINFO " ^ string_of_int skill


let int_of_stat = let open Types in function
  | CS_STAT_HP              ->   1
  | CS_STAT_MAXHP           ->   2
  | CS_STAT_SP              ->   3
  | CS_STAT_MAXSP           ->   4
  | CS_STAT_STR             ->   5
  | CS_STAT_INT             ->   6
  | CS_STAT_WIS             ->   7
  | CS_STAT_DEX             ->   8
  | CS_STAT_CON             ->   9
  | CS_STAT_CHA             ->  10
  | CS_STAT_EXP             ->  11
  | CS_STAT_LEVEL           ->  12
  | CS_STAT_WC              ->  13
  | CS_STAT_AC              ->  14
  | CS_STAT_DAM             ->  15
  | CS_STAT_ARMOUR          ->  16
  | CS_STAT_SPEED           ->  17
  | CS_STAT_FOOD            ->  18
  | CS_STAT_WEAP_SP         ->  19
  | CS_STAT_RANGE           ->  20
  | CS_STAT_TITLE           ->  21
  | CS_STAT_POW             ->  22
  | CS_STAT_GRACE           ->  23
  | CS_STAT_MAXGRACE        ->  24
  | CS_STAT_FLAGS           ->  25
  | CS_STAT_WEIGHT_LIM      ->  26
  | CS_STAT_EXP64           ->  28
  | CS_STAT_SPELL_ATTUNE    ->  29
  | CS_STAT_SPELL_REPEL     ->  30
  | CS_STAT_SPELL_DENY      ->  31
  | CS_STAT_RES_PHYS        -> 100
  | CS_STAT_RES_MAG         -> 101
  | CS_STAT_RES_FIRE        -> 102
  | CS_STAT_RES_ELEC        -> 103
  | CS_STAT_RES_COLD        -> 104
  | CS_STAT_RES_CONF        -> 105
  | CS_STAT_RES_ACID        -> 106
  | CS_STAT_RES_DRAIN       -> 107
  | CS_STAT_RES_GHOSTHIT    -> 108
  | CS_STAT_RES_POISON      -> 109
  | CS_STAT_RES_SLOW        -> 110
  | CS_STAT_RES_PARA        -> 111
  | CS_STAT_TURN_UNDEAD     -> 112
  | CS_STAT_RES_FEAR        -> 113
  | CS_STAT_RES_DEPLETE     -> 114
  | CS_STAT_RES_DEATH       -> 115
  | CS_STAT_RES_HOLYWORD    -> 116
  | CS_STAT_RES_BLIND       -> 117
  | CS_STAT_SKILLINFO skill -> 140 + skill


let stat_of_int = let open Types in function
  |   1 -> CS_STAT_HP
  |   2 -> CS_STAT_MAXHP
  |   3 -> CS_STAT_SP
  |   4 -> CS_STAT_MAXSP
  |   5 -> CS_STAT_STR
  |   6 -> CS_STAT_INT
  |   7 -> CS_STAT_WIS
  |   8 -> CS_STAT_DEX
  |   9 -> CS_STAT_CON
  |  10 -> CS_STAT_CHA
  |  11 -> CS_STAT_EXP
  |  12 -> CS_STAT_LEVEL
  |  13 -> CS_STAT_WC
  |  14 -> CS_STAT_AC
  |  15 -> CS_STAT_DAM
  |  16 -> CS_STAT_ARMOUR
  |  17 -> CS_STAT_SPEED
  |  18 -> CS_STAT_FOOD
  |  19 -> CS_STAT_WEAP_SP
  |  20 -> CS_STAT_RANGE
  |  21 -> CS_STAT_TITLE
  |  22 -> CS_STAT_POW
  |  23 -> CS_STAT_GRACE
  |  24 -> CS_STAT_MAXGRACE
  |  25 -> CS_STAT_FLAGS
  |  26 -> CS_STAT_WEIGHT_LIM
  |  28 -> CS_STAT_EXP64
  |  29 -> CS_STAT_SPELL_ATTUNE
  |  30 -> CS_STAT_SPELL_REPEL
  |  31 -> CS_STAT_SPELL_DENY
  | 100 -> CS_STAT_RES_PHYS
  | 101 -> CS_STAT_RES_MAG
  | 102 -> CS_STAT_RES_FIRE
  | 103 -> CS_STAT_RES_ELEC
  | 104 -> CS_STAT_RES_COLD
  | 105 -> CS_STAT_RES_CONF
  | 106 -> CS_STAT_RES_ACID
  | 107 -> CS_STAT_RES_DRAIN
  | 108 -> CS_STAT_RES_GHOSTHIT
  | 109 -> CS_STAT_RES_POISON
  | 110 -> CS_STAT_RES_SLOW
  | 111 -> CS_STAT_RES_PARA
  | 112 -> CS_STAT_TURN_UNDEAD
  | 113 -> CS_STAT_RES_FEAR
  | 114 -> CS_STAT_RES_DEPLETE
  | 115 -> CS_STAT_RES_DEATH
  | 116 -> CS_STAT_RES_HOLYWORD
  | 117 -> CS_STAT_RES_BLIND

  | stat when stat >= 140 && stat < 140 + cs_num_skills ->
      CS_STAT_SKILLINFO (stat - 140)

  | _ -> failwith "stat_of_int"


let empty = Types.({
  s_skillinfo		= IntMap.empty;
  s_exp64		= 0L;

  s_range		= "";
  s_title		= "";

  s_weight_lim		= 0;
  s_spell_attune	= 0;
  s_spell_repel		= 0;
  s_spell_deny		= 0;

  s_exp			= 0;

  s_hp			= 0;
  s_maxhp		= 0;
  s_sp			= 0;
  s_maxsp		= 0;
  s_str			= 0;
  s_int			= 0;
  s_wis			= 0;
  s_dex			= 0;
  s_con			= 0;
  s_cha			= 0;
  s_level		= 0;
  s_wc			= 0;
  s_ac			= 0;
  s_dam			= 0;
  s_armour		= 0;
  s_food		= 0;
  s_pow			= 0;
  s_grace		= 0;

  s_speed		= 0.0;
  s_weap_sp		= 0.0;

  s_maxgrace		= 0;
  s_flags		= 0;

  s_res_phys		= 0;
  s_res_mag		= 0;
  s_res_fire		= 0;
  s_res_elec		= 0;
  s_res_cold		= 0;
  s_res_conf		= 0;
  s_res_acid		= 0;
  s_res_drain		= 0;
  s_res_ghosthit	= 0;
  s_res_poison		= 0;
  s_res_slow		= 0;
  s_res_para		= 0;
  s_turn_undead		= 0;
  s_res_fear		= 0;
  s_res_deplete		= 0;
  s_res_death		= 0;
  s_res_holyword	= 0;
  s_res_blind		= 0;
})


let update stats stat stream =
  let open Binary in
  let open Types in

  (*print_endline (string_of_stat stat);*)

  match stat with
  | CS_STAT_HP              -> { stats with s_hp           = read_s16 stream }
  | CS_STAT_MAXHP           -> { stats with s_maxhp        = read_u16 stream }
  | CS_STAT_SP              -> { stats with s_sp           = read_s16 stream }
  | CS_STAT_MAXSP           -> { stats with s_maxsp        = read_u16 stream }
  | CS_STAT_STR             -> { stats with s_str          = read_u16 stream }
  | CS_STAT_INT             -> { stats with s_int          = read_u16 stream }
  | CS_STAT_WIS             -> { stats with s_wis          = read_u16 stream }
  | CS_STAT_DEX             -> { stats with s_dex          = read_u16 stream }
  | CS_STAT_CON             -> { stats with s_con          = read_u16 stream }
  | CS_STAT_CHA             -> { stats with s_cha          = read_u16 stream }
  | CS_STAT_LEVEL           -> { stats with s_level        = read_u16 stream }
  | CS_STAT_WC              -> { stats with s_wc           = read_s16 stream }
  | CS_STAT_AC              -> { stats with s_ac           = read_s16 stream }
  | CS_STAT_DAM             -> { stats with s_dam          = read_u16 stream }
  | CS_STAT_ARMOUR          -> { stats with s_armour       = read_u16 stream }
  | CS_STAT_FOOD            -> { stats with s_food         = read_s16 stream }
  | CS_STAT_POW             -> { stats with s_pow          = read_u16 stream }
  | CS_STAT_GRACE           -> { stats with s_grace        = read_s16 stream }
  | CS_STAT_MAXGRACE        -> { stats with s_maxgrace     = read_u16 stream }
  | CS_STAT_FLAGS           -> { stats with s_flags        = read_u16 stream }
  | CS_STAT_RES_PHYS        -> { stats with s_res_phys     = read_s16 stream }
  | CS_STAT_RES_MAG         -> { stats with s_res_mag      = read_s16 stream }
  | CS_STAT_RES_FIRE        -> { stats with s_res_fire     = read_s16 stream }
  | CS_STAT_RES_ELEC        -> { stats with s_res_elec     = read_s16 stream }
  | CS_STAT_RES_COLD        -> { stats with s_res_cold     = read_s16 stream }
  | CS_STAT_RES_CONF        -> { stats with s_res_conf     = read_s16 stream }
  | CS_STAT_RES_ACID        -> { stats with s_res_acid     = read_s16 stream }
  | CS_STAT_RES_DRAIN       -> { stats with s_res_drain    = read_s16 stream }
  | CS_STAT_RES_GHOSTHIT    -> { stats with s_res_ghosthit = read_s16 stream }
  | CS_STAT_RES_POISON      -> { stats with s_res_poison   = read_s16 stream }
  | CS_STAT_RES_SLOW        -> { stats with s_res_slow     = read_s16 stream }
  | CS_STAT_RES_PARA        -> { stats with s_res_para     = read_s16 stream }
  | CS_STAT_TURN_UNDEAD     -> { stats with s_turn_undead  = read_s16 stream }
  | CS_STAT_RES_FEAR        -> { stats with s_res_fear     = read_s16 stream }
  | CS_STAT_RES_DEPLETE     -> { stats with s_res_deplete  = read_s16 stream }
  | CS_STAT_RES_DEATH       -> { stats with s_res_death    = read_s16 stream }
  | CS_STAT_RES_HOLYWORD    -> { stats with s_res_holyword = read_s16 stream }
  | CS_STAT_RES_BLIND       -> { stats with s_res_blind    = read_s16 stream }

  | CS_STAT_EXP             -> { stats with s_exp          = read_u32 stream }
  | CS_STAT_WEIGHT_LIM      -> { stats with s_weight_lim   = read_u32 stream }
  | CS_STAT_SPELL_ATTUNE    -> { stats with s_spell_attune = read_u32 stream }
  | CS_STAT_SPELL_REPEL     -> { stats with s_spell_repel  = read_u32 stream }
  | CS_STAT_SPELL_DENY      -> { stats with s_spell_deny   = read_u32 stream }

  | CS_STAT_EXP64           -> { stats with s_exp64        = read_u64 stream }

  | CS_STAT_RANGE           -> { stats with s_range        = read_string08 stream }
  | CS_STAT_TITLE           -> { stats with s_title        = read_string08 stream }

  | CS_STAT_SPEED           -> { stats with s_speed        = read_float stream }
  | CS_STAT_WEAP_SP         -> { stats with s_weap_sp      = read_float stream }

  | CS_STAT_SKILLINFO skill ->
      let level = read_u08 stream in
      let value = read_u64 stream in

      { stats with s_skillinfo = IntMap.add skill (level, value) stats.s_skillinfo }
