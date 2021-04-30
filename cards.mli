(** The abstract type of values representing cards *)
type t

(** [chance_cards] is the list of chance cards *)
val chance_cards : t list

(** [com_chest] is the list of community chest cards *)
val com_chest : t list

(** [get_card] returns a card of [card_type] and the [card_id]*)
val get_card : string -> int -> t

(** [card_text] returns the text of [card]*)
val card_text : t -> string

(** [card_effect] performs the effect of the card and returns a list of any
    monetary transactions required in the form of (player giving money, amount
    of money, player recieveing money)*)
val card_effect : t -> Game.t -> (Player.t option * int * Player.t option) list
