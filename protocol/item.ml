type flag =
  | UPD_LOCATION
  | UPD_FLAGS
  | UPD_WEIGHT
  | UPD_FACE
  | UPD_NAME
  | UPD_ANIM
  | UPD_ANIMSPEED
  | UPD_NROF


let want_update flags = function
  | UPD_LOCATION  -> flags land 0x01 <> 0
  | UPD_FLAGS     -> flags land 0x02 <> 0
  | UPD_WEIGHT    -> flags land 0x04 <> 0
  | UPD_FACE      -> flags land 0x08 <> 0
  | UPD_NAME      -> flags land 0x10 <> 0
  | UPD_ANIM      -> flags land 0x20 <> 0
  | UPD_ANIMSPEED -> flags land 0x40 <> 0
  | UPD_NROF      -> flags land 0x80 <> 0
