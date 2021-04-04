type t

(** [create_player player_name start] is a player with the name [player_name]
    and initializes their position at [start] *)
val create_player : string -> Property.t -> t

(** [get_name player] is the name of [player] *)
val get_name : t -> string

(** [get_properties player] is the list of properties owned by [player] *)
val get_properties : t -> Property.t list

(** [net_worth player] is the net worth of [player] *)
val net_worth : t -> int

(** [clear_properties player] empties the list of properties held by [player]
    and sets the owner to the empty string*)
val clear_properties : t -> unit

(** [pp_properties player] is the string containing the properties of [player] *)
val pp_properties : t -> string

(** [pp_monopolies player] is the string containing the monopolies of [player] *)
val pp_monopolies : t -> string

(** [player_money player] is the amount of money owned by [player] *)
val player_money : t -> int

(** [get_position player] is the position of [player] *)
val get_position : t -> Property.t

(** [change_pos player new_pos] changes the position of [player] to [new_pos] *)
val change_pos : t -> Property.t -> unit

(** [add_property player prop] adds [prop] to the list of properties held by
    [player] *)
val add_property : t -> Property.t -> unit

(** [buy_property player prop] purchases [prop] for [player]: adds it to their
    list of properties, sets [player] as the owner of [prop], decreases their
    amount of money by the price of [prop], and adds [prop] to the monopoly if
    it exists *)
val buy_property : t -> Property.t -> unit

(** [update_player_money player amount] adds [amount] to [player]'s current
    amount of money *)
val update_player_money : t -> int -> unit

(** [calculate_owned_rent prop owner] is the rent of [prop] owned by [owner].
    Value depends on if it is a railroad, utility, or normal colored property *)
val calculate_owned_rent : Property.t -> t -> int

(** [add_monopoly player prop_type] adds [prop_type] to [player]'s list of
    monopolies *)
val add_monopoly : t -> Property.space_type -> unit

(** [has_monopoly player prop] checks if [player] has a monopoly that includes
    [prop] *)
val has_monopoly : t -> Property.t -> bool

(** [check_monopoly player new_prop] adds a new monopoly to [player] if
    acquiring [new_prop] creates a monopoly *)
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

val print_assets : t -> unit

val player_status : int -> t -> int

val return_prop_to_bank : t -> Property.t -> unit

val remove_property : t -> Property.t -> unit

val swap_owner : t -> t -> Property.t -> unit
