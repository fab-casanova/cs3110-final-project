open Player
open Property

type gameboard = property list

let rec get_index_helper prop board acc =
  match board with
  | [] -> raise Not_found
  | h :: t -> if prop = h then acc else get_index_helper prop t (acc + 1)

let get_index (prop : property) (board : gameboard) =
  get_index_helper prop board 0
