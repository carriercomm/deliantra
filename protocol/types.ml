include Ext_t
include Message_t
include Resources_t


type glyph = {
  foreground		: int;
  background		: int;
  text			: BatUTF8.t;
}

type tile = glyph list


type face_type =
  | FT_IMAGE
  | FT_MUSIC
  | FT_SOUND
  | FT_RSRC


type fx = {
  fx_type		: face_type;
  fx_facenum		: int;
  fx_glyph		: glyph list;
}


type sound = {
  sc_type		: int;
  sc_facenum		: int;
  sc_dx			: int;
  sc_dy			: int;
  sc_volume		: int;
}


type map_cell = {
  player		: int;
  layers		: int * int * int;
  cell_darkness		: int;
  stat_width		: int;
  stat_hp		: int;
  flags			: int;
}

type map = {
  rows			: map_cell Deque.t Deque.t;
  x			: int;
  y			: int;
  w			: int;
  h			: int;
}


type item = {
  tag		: int;
  itemflags	: int;
  weight	: int;
  face		: int;

  singular	: string;
  plural	: string;

  anim		: int;
  animspeed	: int;
  nrof		: int;
  itemtype	: int;
}


type stat =
  | CS_STAT_HP
  | CS_STAT_MAXHP
  | CS_STAT_SP
  | CS_STAT_MAXSP
  | CS_STAT_STR
  | CS_STAT_INT
  | CS_STAT_WIS
  | CS_STAT_DEX
  | CS_STAT_CON
  | CS_STAT_CHA
  | CS_STAT_EXP
  | CS_STAT_LEVEL
  | CS_STAT_WC
  | CS_STAT_AC
  | CS_STAT_DAM
  | CS_STAT_ARMOUR
  | CS_STAT_SPEED
  | CS_STAT_FOOD
  | CS_STAT_WEAP_SP
  | CS_STAT_RANGE
  | CS_STAT_TITLE
  | CS_STAT_POW
  | CS_STAT_GRACE
  | CS_STAT_MAXGRACE
  | CS_STAT_FLAGS
  | CS_STAT_WEIGHT_LIM
  | CS_STAT_EXP64
  | CS_STAT_SPELL_ATTUNE
  | CS_STAT_SPELL_REPEL
  | CS_STAT_SPELL_DENY
  | CS_STAT_RES_PHYS
  | CS_STAT_RES_MAG
  | CS_STAT_RES_FIRE
  | CS_STAT_RES_ELEC
  | CS_STAT_RES_COLD
  | CS_STAT_RES_CONF
  | CS_STAT_RES_ACID
  | CS_STAT_RES_DRAIN
  | CS_STAT_RES_GHOSTHIT
  | CS_STAT_RES_POISON
  | CS_STAT_RES_SLOW
  | CS_STAT_RES_PARA
  | CS_STAT_TURN_UNDEAD
  | CS_STAT_RES_FEAR
  | CS_STAT_RES_DEPLETE
  | CS_STAT_RES_DEATH
  | CS_STAT_RES_HOLYWORD
  | CS_STAT_RES_BLIND
  | CS_STAT_SKILLINFO of int


type statistics = {
  s_skillinfo		: (int * int64) IntMap.t;
  s_exp64		: int64;

  s_range		: string;
  s_title		: string;

  s_weight_lim		: int;
  s_spell_attune	: int;
  s_spell_repel		: int;
  s_spell_deny		: int;

  s_exp			: int;

  s_hp			: int;
  s_maxhp		: int;
  s_sp			: int;
  s_maxsp		: int;
  s_str			: int;
  s_int			: int;
  s_wis			: int;
  s_dex			: int;
  s_con			: int;
  s_cha			: int;
  s_level		: int;
  s_wc			: int;
  s_ac			: int;
  s_dam			: int;
  s_armour		: int;
  s_food		: int;
  s_pow			: int;
  s_grace		: int;

  s_speed		: float;
  s_weap_sp		: float;

  s_maxgrace		: int;
  s_flags		: int;

  s_res_phys		: int;
  s_res_mag		: int;
  s_res_fire		: int;
  s_res_elec		: int;
  s_res_cold		: int;
  s_res_conf		: int;
  s_res_acid		: int;
  s_res_drain		: int;
  s_res_ghosthit	: int;
  s_res_poison		: int;
  s_res_slow		: int;
  s_res_para		: int;
  s_turn_undead		: int;
  s_res_fear		: int;
  s_res_deplete		: int;
  s_res_death		: int;
  s_res_holyword	: int;
  s_res_blind		: int;
}


type mapinfo = {
  token			: string;
  mapflags		: int;
  mapx			: int;
  mapy			: int;
  mapw			: int;
  maph			: int;
  path			: string;
}


type tileset = {
  fs_id		: int;
  fs_name	: string;
  fs_flags	: int;
  fs_edgelen	: int;
}


type nick =
  | NT_PLAYER  of (* nick *)string * (* count *)int * (* uuid *)string
  | NT_SERVICE of (* nick *)string * (* description *)string
  | NT_OTHER   of (* nick *)string * (* type *)string


type resources = {
  exp_table		: exp_table;
  skill_info		: skill_info;
  spell_paths		: spell_paths;

  commands		: string list IntMap.t;

  partial_faces		: string IntMap.t;
}

type nonces = string * string


type sendfun	= string -> string -> unit Lwt.t
type 'a ext_cb	= (Yojson.Basic.json list -> sendfun -> 'a -> 'a Lwt.t)
type 'a face_cb	= (string -> sendfun -> 'a -> 'a Lwt.t)

type env = {
  res			: resources;
  nonces		: nonces option;
  setup			: setup;
  tiles			: tile IntMap.t;
  map			: map;
  stats			: statistics;
  ext_requests		: env ext_cb IntMap.t;
  face_requests		: env face_cb IntMap.t;
}
