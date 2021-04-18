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
    [price], the cost to add a house of [house_price], and the cost for rent
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

val current_stage : t -> property_stage

val create_mortgage : t -> unit

val unmortgage : t -> unit

val get_type : t -> space_type

val upgrade_property : t -> unit

val downgrade_property : t -> unit

val num_for_monopoly : t -> int

val num_houses : t -> int

val prop_name : t -> string

val get_value : t -> int

val reset_stage : t -> unit

val release_property : t -> unit
