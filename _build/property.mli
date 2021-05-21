(** Representation of a property square in Monopoly *)

(** The type representing the kind of space *)
type space_type

(** The type representing the stage of the property *)
type property_stage

(** The abstract type of values representing properties. *)
type t

(** [prop_name prop] is the string containing the name of the [prop] *)
val prop_name : t -> string

(** [pp_space_type] maps each space type to a string that is the name of the
    space type *)
val pp_space_type : space_type -> string

(** [prop_space_type prop] is the string containing the space type of [prop] *)
val prop_space_type : t -> string

(** [is_owned prop] is true if [prop] is owned by a player *)
val is_owned : t -> bool

(** [is_utilities prop] is true if [prop] is a utilities property *)
val is_utilities : t -> bool

(** [is_free_parking prop] is true if [prop] is a free parking space *)
val is_free_parking : t -> bool

(** [is_tax prop] is true if [prop] is a tax property *)
val is_tax : t -> bool

(** [is_railroad prop] is true if [prop] is a railroad property *)
val is_railroad : t -> bool

(** [is_go_to_jail prop] is true if [prop] is the go to jail space *)
val is_go_to_jail : t -> bool

(** [is_jail prop] is true if [prop] is the jail space *)
val is_jail : t -> bool

(** [is_com_or_chance prop] is true if [prop] is a community chest space or a
    chance space *)
val is_com_or_chance : t -> bool

(** [can_be_purchased prop] is true if [prop] is purchasable *)
val can_be_purchased : t -> bool

(** [can_have_houses prop] is true if [prop] can have houses built on it *)
val can_have_houses : t -> bool

(** [can_be_upgraded prop] is true if [prop] is an upgradable space *)
val can_be_upgraded : t -> bool

(** [what_stage prop] is the string containing what stage [prop] is at *)
val what_stage : t -> string

(** [create_buyable_card name card_type prices price house_price] creates a card
    with the name of [name], the type of [card_type], a purchase price of
    [price], the cost to build a house of [house_price], and the cost for rent
    being [prices], where [prices] is an array with its index corresponding to
    the number of houses built on the property *)
val create_buyable_card : string -> string -> int array -> int -> int -> t

(** [create_unbuyable_card name card_type rent] creates a card with the name of
    [name], the type of [card_type], and the rent price of [rent] *)
val create_unbuyable_card : string -> string -> int -> t

(** [calculate_rent_or_tax prop] is the cost of rent or tax on [prop] *)
val calculate_rent_or_tax : t -> int

(** [purchase_price prop] is the cost to purchase [prop] *)
val purchase_price : t -> int

(** [house_cost prop] is the cost to build a house on [prop] *)
val house_cost : t -> int

(** [get_owner_name prop] is the name of the owner of [prop] *)
val get_owner_name : t -> string

(** [set_owner prop owner] sets the person who owns [prop] to [owner] *)
val set_owner : t -> string -> unit

(** [is_mortgaged prop] is true if [prop] is currently mortgaged *)
val is_mortgaged : t -> bool

(** [current_stage prop] is the current stage of [prop] *)
val current_stage : t -> property_stage

(** [create_mortgage prop] creates a mortgage for [prop] *)
val create_mortgage : t -> unit

(** [unmortgage prop] removes the mortgage from [prop] *)
val unmortgage : t -> unit

(** [get_type prop] is the property type of [prop] *)
val get_type : t -> space_type

(** [upgrade_property prop] upgrades [prop] by one stage *)
val upgrade_property : t -> unit

(** [downgrade_property prop] downgrades [prop] by one stage *)
val downgrade_property : t -> unit

(** [num_for_monopoly prop] is the number of properties needed to have a
    monopoly for a the color of [prop] *)
val num_for_monopoly : t -> int

(** [num_houses prop] is the number of houses on [prop] *)
val num_houses : t -> int

(** [prop_name prop] is the property name for [prop] *)
val prop_name : t -> string

(** [get_value prop] is the price of the [prop] based on whether or not it is
    mortgaged *)
val get_value : t -> int

(** [reset_stage prop] resets [prop] to its lowest stage *)
val reset_stage : t -> unit

(** [release_property prop] sets the bank as the owner of [prop] *)
val release_property : t -> unit
