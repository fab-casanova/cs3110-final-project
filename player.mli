type t

val create_player : string -> Property.t -> t

val get_name : t -> string

val get_properties : t -> Property.t list

val player_money : t -> int

val get_position : t -> Property.t

val change_pos : t -> Property.t -> unit

val add_property : t -> Property.t -> unit

val buy_property : t -> Property.t -> unit

val update_player_money : t -> int -> unit

val collect_rent : t -> t -> Property.t -> unit

val collect_tax : t -> Property.t -> unit

val add_monopoly : t -> Property.space_type -> unit

val has_monopoly : t -> Property.t -> bool

val building_evenly : t -> Property.t -> (int -> int -> int) -> bool

val roll_dice : unit -> int

val out_of_cash : int -> t -> bool

val is_bankrupt : int -> t -> bool

val build_houses_hotel : t -> Property.t -> unit
