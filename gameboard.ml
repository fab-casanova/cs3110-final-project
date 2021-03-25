type t = Property.t list

let rec get_index_helper board (prop : Property.t) acc =
  match board with
  | [] -> raise Not_found
  | h :: t -> if prop = h then acc else get_index_helper t prop (acc + 1)

let get_index (board : t) (prop : Property.t) = get_index_helper board prop 0

let rec get_prop_at_index index (board : t) =
  match board with
  | [] -> raise Not_found
  | h :: t -> if index = 0 then h else get_prop_at_index (index - 1) t
