type players

type t

val roll_dice : unit -> int

val get_players : t -> players

val find_player : string -> players -> Player.t

val move_player : Player.t -> t -> unit

val collect_rent : Player.t -> Property.t -> t -> unit
