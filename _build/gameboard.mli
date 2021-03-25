open Player

type gameboard

val get_index : gameboard -> Property.property -> int

val get_prop_at_index : int -> gameboard -> Property.property
