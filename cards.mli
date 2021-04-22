(** The abstract type of values representing cards *)
type t

(** [chance_cards] is the list of chance cards *)
val chance_cards : t list

(** [com_chest] is the list of community chest cards *)
val com_chest : t list
