(** Represents players in Monopoly *)

(** The abstract type of values representing players. *)
type t

(** [create_player player_name start] is a player with the name [player_name]
    and initial position [start] *)
val create_player : string -> Property.t -> t

(** [create_ai ai_name start] is an AI player with the name [ai_name] and
    initial position [start] *)
val create_ai : string -> Property.t -> t

(** [is_real_player player] is true iff [player] is not an AI player *)
val is_real_player : t -> bool

(** [get_name player] is the name of [player] *)
val get_name : t -> string

(** [get_properties player] is the list of properties owned by [player] *)
val get_properties : t -> Property.t list

(** [net_worth player] is the net worth of [player] *)
val net_worth : t -> int

(** [clear_properties player] empties the list of properties held by [player]
    and sets the owner to the bank *)
val clear_properties : t -> unit

(** [pp_property_list lst] is the string listing the properties in [lst] *)
val pp_property_list : Property.t list -> string

(** [pp_properties player] is the string listing the properties of [player] *)
val pp_properties : t -> string

(** [pp_monopolies player] is the string listing the monopolies of [player] *)
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

(** [is_building_evenly props prop is_building] takes in list of [props] and
    checks if a house can be either built or destroyed on [prop] depending on
    the value of [is_building]. [is_building] is true if you are building on
    [prop] and false if you are destroying. It ensures that you are building
    evenly over all property types *)
val is_building_evenly : Property.t list -> Property.t -> bool -> bool

(** [roll_dice] is a tuple of two random numbers from 0-5 and adds 1 to each of
    them to simulate rolling two die with face values of 1-6 *)
val roll_dice : unit -> int * int

(** [sum_dice dice] is the sum of the tuple [dice] which represents two die with
    values ranging from 1-6 *)
val sum_dice : int * int -> int

(** [out_of_cash amount_owed player] is true if [amount_owed] is greater than
    [player]'s amount of money and false otherwise *)
val out_of_cash : int -> t -> bool

(** [is_bankrupt amount_owed player] is true if [amount_owed] is greater than
    [player]'s net worth and false otherwise *)
val is_bankrupt : int -> t -> bool

(** [buildable_props player] is the list of properties owned by [player] that
    can currently have houses (or a hotel) built on them. A property is included
    if the player has a monopoly that includes said property, if they have
    enough money to buy a house on said property, if said property is
    upgradeable, and if the building on said property ensures that [player] is
    building evenly over all properties of that color *)
val buildable_props : t -> Property.t list

(** [mortgage_allowed player prop] is true if [player] is the owner of [prop],
    [prop] is not currently being mortgaged, and there are no houses on the
    color of [prop] *)
val mortgage_allowed : t -> Property.t -> bool

(** [mortgageable_props player] is a list of all properties owned by [player]
    that can be mortgaged *)
val mortgageable_props : t -> Property.t list

(** [unmortgageable_props player] is a list of all properties owned by [player]
    that can be unmortgaged *)
val unmortgageable_props : t -> Property.t list

(** [can_transfer player prop] is whether [prop] is owned by [player] and can be
    transfered *)
val can_transfer : t -> Property.t -> bool

(** [transferable_props player] is a list of all properties owned by [player]
    that can be transfered *)
val transferable_props : t -> Property.t list

(** [no_houses_on_monopoly player prop] is true if [player] doesn't have any
    houses on the color of [prop] *)
val no_houses_on_monopoly : t -> Property.t -> bool

(** [owns_property player prop] is true is [player] is an owner of [prop] *)
val owns_property : t -> Property.t -> bool

(** [print_assets player] prints the assets of [player], including [player]'s
    money, properties, monopolies, and consecutive doubles *)
val print_assets : t -> unit

(** [player_status dues player] is 0 if [player] is not out of cash due to their
    [dues], 1 if they are out of cash but not bankrupt, and 2 if they are out of
    cash and bankrupt *)
val player_status : int -> t -> int

(* [landed heads ()] is whether a coin landed on heads *)
val landed_heads : unit -> bool

(* [three_sided_die ()] is [0], [1], or [2] with equal probability *)
val three_sided_die : unit -> int

(** [random_property player] is a randomly-chosen property owned by [player] *)
val random_property : t -> Property.t

(** [return_prop_to_bank player prop] removes [prop] from [player]'s list of
    properties, sets the owner of [prop] to the empty string (means it's now
    owned by the bank), and resets the stage of [prop] back to Zero *)
val return_prop_to_bank : t -> Property.t -> unit

(** [remove_property player prop] removes [prop] from [player]'s list of
    properties *)
val remove_property : t -> Property.t -> unit

(** [swap_owner giver receiver prop] swaps ownership of [prop] from [giver] to
    [receiver]: sets the owner to [receiver], adds [prop] to [receiver]'s list
    of properties, and removes [prop] from [giver]'s list of properties *)
val swap_owner : t -> t -> Property.t -> unit

(** [hand_over_all_properties giver receiver] swaps ownership of all properties
    owned by [giver] to [receiver]: sets the owner to [receiver], adds each
    property to [receiver]'s list of properties, and removes [prop] from
    [giver]'s list of properties *)
val hand_over_all_properties : t -> t -> unit

(** [in_jail player] is true if [player] is currently in jail *)
val in_jail : t -> bool

(** [put_in_jail player] places [player] in jail for 3 turns *)
val put_in_jail : t -> unit

(** [un_jail player] removes [player] from jail *)
val un_jail : t -> unit

(** [served_a_turn player] decreases the amount of turns [player] has to be in
    jail by 1 *)
val served_a_turn : t -> unit

(** [time_left player] is the amount of turns left for [player] to be in jail *)
val time_left : t -> int

(** [num_doubles player] is the number of consequtive doubles [player] has
    rolled *)
val num_doubles : t -> int

(** [reset_doubles player] resets the number of doubles rolled by [player] to 0 *)
val reset_doubles : t -> unit

(** [add_double player] increments the amount of doubles rolled by [player] *)
val add_double : t -> unit

(** [num_jail_free_cards] returns the number of get out of jail free cards
    [player] has *)
val num_jail_free_cards : t -> int

(** [add_jail_free_card] gives [player] a get out of jail free card from the
    deck represented by [jail_card] *)
val add_jail_free_card : string -> t -> unit

(** [owns_jail_free_card deck player] returns true if [player] has a get out of
    jail free card from the [deck] *)
val owns_jail_free_card : string -> t -> bool

(** [remove_jail_free_card] removes a get out of jail free card from [player]*)
val remove_jail_free_card : t -> unit

(** [remove_monopoly] removes the color of [prop] from [player]'s monopolies*)
val remove_monopoly : t -> Property.t -> unit

(** [sellable_bldg_props player] is the list of properties owned by [player]
    with house that can be currently sold *)
val sellable_bldg_props : t -> Property.t list
