type t

val create_player : string -> Property.t -> t

val get_name : t -> string

val get_properties : t -> Property.t list

val clear_properties : t -> unit

val pp_properties : t -> string

val pp_monopolies : t -> string

val player_money : t -> int

val get_position : t -> Property.t

val change_pos : t -> Property.t -> unit

val add_property : t -> Property.t -> unit

val buy_property : t -> Property.t -> unit

val update_player_money : t -> int -> unit

val calculate_rent : Property.t -> t -> int

val add_monopoly : t -> Property.space_type -> unit

val has_monopoly : t -> Property.t -> bool

val check_monopoly : t -> Property.t -> unit

val is_building_evenly : Property.t list -> Property.t -> bool -> bool

val roll_dice : unit -> int

val out_of_cash : int -> t -> bool

val is_bankrupt : int -> t -> bool

val can_build_houses_hotel : t -> Property.t -> bool

val mortgage_allowed : t -> Property.t -> bool

val no_houses_on_monopoly : t -> Property.t -> bool

val owns_property : t -> Property.t -> bool

val get_prop_of_name : t -> string -> Property.t

val assets : t -> unit
