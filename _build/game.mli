type t

val roll_dice : unit -> int

val move_player : Player.t -> t -> unit

val collect_rent : Player.t -> Property.t -> t -> unit

val buy_property : Player.t -> Property.t -> unit
