(** Represents the current status of a Monopoly game *)

(**The type representing the players *)
type players

(** The type representing the gameboard *)
type gameboard

(** The abstract type representing the game *)
type t

(** [pp_players game] prints out the players of the [game] *)
val pp_players : t -> string

(** [pp_other_players player game] prints out all players of the [game] aside
    from [player]*)
val pp_other_players : Player.t -> t -> string

(** [print_game_status game] prints the position and assets of each active
    player in [game].*)
val print_game_status : t -> unit

(** [create_gameboard lst] creates a gameboard out of a [lst] of properties *)
val create_gameboard : Property.t list -> gameboard

(** [current_player game] is the current player whose turn it is in the [game] *)
val current_player : t -> Player.t

(** [move_to_next_player game] sets the current player of the [game] to the
    player whose turn is next *)
val move_to_next_player : t -> unit

(** [create_game board] creates a game with a given [board] *)
val create_game : gameboard -> t

(** [pot_amount game] is the amount of money in the pot of the [game] *)
val pot_amount : t -> int

(** [add_to_pot game cash] adds the given amount of [cash] to the pot of the
    [game] *)
val add_to_pot : t -> int -> unit

(** [cash_out_pot game player] gives the amount of money in the [game]'s pot to
    [player] and sets the amount in the pot to zero *)
val cash_out_pot : t -> Player.t -> unit

(** [num_players game] is the number of players in the [game] *)
val num_players : t -> int

(** [get_jail game] is the property that is the jail space in the [game] *)
val get_jail : t -> Property.t

(** [still_in_game player game] is true if [player] is still in the [game] *)
val still_in_game : Player.t -> t -> bool

(** [get_start_pos game] is the property that all players start on in the [game] *)
val get_start_pos : t -> Property.t

(** [add_a_player game player] adds [player] to the [game] *)
val add_a_player : t -> Player.t -> unit

(** [get_index game prop] is the index of [prop] in the [game] *)
val get_index : t -> Property.t -> int

(** [get_prop_at_index index game] is the property that is located at the
    specified [index] of the [game] *)
val get_prop_at_index : int -> t -> Property.t

(** [get_players game] is the list of players in the [game] *)
val get_players : t -> Player.t list

(** [find_player name game] is the player of a given [name] in the [game] *)
val find_player : string -> t -> Player.t

(** [random_other_player game] is a randomly-chosen player in the [game] that is
    not [player] *)
val random_other_player : Player.t -> t -> Player.t

(** [acceptable_default_name game is_ai] returns a default name of the form
    "playern" iff [not is_ai] or "ain", where n is the lowest integer which
    creates a name that is not taken in [game] *)
val acceptable_default_name : t -> bool -> string

(** [get_owner prop game] is the player who owns [prop] in the [game] *)
val get_owner : Property.t -> t -> Player.t

(** [move_player player game given_moves special_move] moves [player] in the
    [game] by a certain number of moves depending on the value of the rolled
    dice. If [special_move] is true then the player gets moved by the value of
    [given_moves] *)
val move_player : Player.t -> t -> int -> bool -> unit

(** [shuffle_deck ()] randomly shuffles the deck of cards *)
val shuffle_deck : unit -> int list

(** [calculate_dues prop game] is the value of the rent of a [prop] in the
    [game] if it is owned and the value of its tax if it is unowned *)
val calculate_dues : Property.t -> t -> int

(** [collect_dues player prop dues game] removes the amount of [dues] from
    [player]'s money amount and gives it to the owner of [prop] if the [prop] is
    owned. If [prop] is not owned, those [dues] get added to the pot of the
    [game] *)
val collect_dues : Player.t -> Property.t -> int -> t -> unit

(** [owns_property_of_name player name game] is true if a property with a
    specific [name] exists in the [game] and if [player] is the owner of that
    property *)
val owns_property_of_name : Player.t -> string -> t -> bool

(** [get_property_of_name name game] is the property with the specified [name]
    in the [game] *)
val get_property_of_name : string -> t -> Property.t

(** [bankruptcy player prop game] gives all cash and assets of [player] to the
    owner of [prop]. If [prop] is unowned, then [player]'s money gets added to
    the pot and their properties are given to the bank. [player] is removed from
    the list of players in the [game] *)
val bankruptcy : Player.t -> Property.t -> t -> unit

(** [forfeit player game] adds all the money of [player] to the pot, gives all
    of their properties to the bank, and removes them from the list of players
    in the [game]*)
val forfeit : Player.t -> t -> unit

(** [pp_remaining_properties game player prop] is the remaining number of
    properties of the same color as [prop] that [player] needs to gain a
    monopoly in the [game] *)
val pp_remaining_properties : t -> Player.t -> Property.t -> string

(** [last_one_standing game] is true if there is only 1 player in the [game] *)
val last_one_standing : t -> bool

(** [set_deck new_deck card_type game] sets the deck that matches [card_type] to
    [new_deck]*)
val set_deck : int list -> string -> t -> unit

(** [get_deck card_type game] returns the deck that matches [card_type]*)
val get_deck : string -> t -> int list
