type player

val get_name : player -> string

val get_properties : player -> Property.property list

val player_money : player -> int

val get_position : player -> Property.property

val change_pos : player -> Property.property -> unit

val update_player_money : player -> int -> unit
