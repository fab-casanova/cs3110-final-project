type property_type

type property_stage

type property

val calculate_rent : property -> int

val get_owner : property -> string

val set_owner : property -> string -> unit

val get_price : property -> int

val create_mortgage : property -> unit

val unmortgage : property -> unit

val get_type : property -> property_type
