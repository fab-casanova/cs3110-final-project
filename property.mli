type space_type

type property_stage

type t

val prop_name : t -> string

val prop_space_type : t -> string

val is_owned : t -> bool

val is_utilities : t -> bool

val is_tax : t -> bool

val is_railroad : t -> bool

val is_com_or_chance : t -> bool

val can_be_purchased : t -> bool

val can_have_houses : t -> bool

val can_be_upgraded : t -> bool

val what_stage : t -> string

val create_buyable_card : string -> string -> int array -> int -> int -> t

val create_unbuyable_card : string -> string -> int -> t

val calculate_rent_or_tax : t -> int

val purchase_price : t -> int

val house_cost : t -> int

val get_owner : t -> string

val set_owner : t -> string -> unit

val return_prop_to_bank : t -> unit

val is_mortgaged : t -> bool

val current_stage : t -> property_stage

val create_mortgage : t -> unit

val unmortgage : t -> unit

val get_type : t -> space_type

val upgrade_property : t -> unit

val num_for_monopoly : t -> int

val num_houses : t -> int

val prop_name : t -> string

val get_value : t -> int
