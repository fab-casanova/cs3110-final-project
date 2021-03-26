type space_type

type property_stage

type t

val create_buyable_card : string -> string -> int array -> int -> int -> t

val create_unbuyable_card : string -> string -> int -> t

val calculate_rent : t -> int

val get_owner : t -> string

val set_owner : t -> string -> unit

val get_price : t -> int

val is_mortaged : t -> bool

val create_mortgage : t -> unit

val unmortgage : t -> unit

val get_type : t -> space_type

val upgrade_prop : t -> int -> t

val downgrade_prop : t -> int -> t

val num_for_monopoly : t -> int
