type players

type gameboard

type t

val create_list_of_players : Player.t list -> players

val create_gameboard : Property.t list -> gameboard

val current_player_name : t -> string

val create_game : gameboard -> players -> t

val num_players : t -> int

val get_start_pos : t -> Property.t

val add_a_player : t -> Player.t -> unit

val get_index : t -> Property.t -> int

val get_prop_at_index : int -> t -> Property.t

val get_players : t -> players

val find_player : string -> players -> Player.t

val move_player : Player.t -> t -> unit

val play_a_turn : t -> Player.t -> unit

(*TODO: Add auction*)
