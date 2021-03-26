type t

val create_player : string -> Property.t -> t

val get_name : t -> string

val get_properties : t -> Property.t list

val player_money : t -> int

val get_position : t -> Property.t

val change_pos : t -> Property.t -> unit

val update_player_money : t -> int -> unit

val add_monopoly : t -> Property.space_type -> unit
