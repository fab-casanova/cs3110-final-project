type players

type gameboard

type t

val create_players : Player.t list -> players

val create_gameboard : Property.t list -> gameboard

val current_player_name : t -> string

val current_player : t -> Player.t

val move_to_next_player : t -> unit

val create_game : gameboard -> players -> t

val num_players : t -> int

val get_start_pos : t -> Property.t

val add_a_player : t -> Player.t -> unit

val get_index : t -> Property.t -> int

val get_prop_at_index : int -> t -> Property.t

val get_players : t -> players

val find_player : string -> players -> Player.t

val move_player : Player.t -> t -> unit

val collect_rent : Player.t -> Player.t -> Property.t -> t -> unit

val collect_tax : Player.t -> Property.t -> t -> unit

(*
val buying_prompt : 
*)
(*TODO: Add auction*)
