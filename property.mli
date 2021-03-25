type property_type

type property_stage

type t

val calculate_rent : t -> int

val get_owner : t -> string

val set_owner : t -> string -> unit

val get_price : t -> int

val is_mortaged : t -> bool

val create_mortgage : t -> unit

val unmortgage : t -> unit

val get_type : t -> property_type

val num_for_monopoly : t -> int
