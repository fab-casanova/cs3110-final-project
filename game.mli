type players

type gameboard

type t

val create_players : Player.t list -> players

val create_gameboard : Property.t list -> gameboard

val current_player_name : t -> string

val current_player : t -> Player.t

val move_to_next_player : t -> unit

val create_game : gameboard -> players -> t

val add_to_pot : t -> int -> unit

val cash_out_pot : t -> Player.t -> unit

val num_players : t -> int

val get_jail : t -> Property.t

val still_in_game : Player.t -> t -> bool

val get_start_pos : t -> Property.t

val add_a_player : t -> Player.t -> unit

val get_index : t -> Property.t -> int

val get_prop_at_index : int -> t -> Property.t

val get_players : t -> players

val find_player : string -> players -> Player.t

val get_owner : Property.t -> t -> Player.t

val move_player : Player.t -> t -> int -> bool -> unit

val shuffle_deck : unit -> int list

val calculate_dues : Property.t -> t -> int

val collect_dues : Player.t -> Property.t -> int -> t -> unit

val owns_property_of_name : Player.t -> string -> t -> bool

val get_property_of_name : string -> t -> Property.t

val bankruptcy : Player.t -> Property.t -> t -> unit

val auction : Property.t -> t -> unit

(* val buying_prompt : *)
