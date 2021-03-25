type property_type =
  | Brown
  | LBlue
  | Pink
  | Orange
  | Red
  | Yellow
  | Green
  | DBlue
  | Jail
  | GoToJail
  | Go
  | FreeParking
  | Railroad
  | Chance
  | ComChest

type property_stage = CannotBuy | Zero | One | Two | Three | Four | Hotel

type t = {
  name : string;
  property_type : property_type;
  mutable owner : string;
  mutable stage : property_stage;
  mutable mortgaged : bool;
  rent_prices : (property_stage * int) list;
  price : int;
}

let calculate_rent prop = List.assoc prop.stage prop.rent_prices

let get_owner prop = prop.owner

let set_owner prop owner = prop.owner <- owner

let get_price prop = prop.price

let current_stage prop = prop.stage

let create_mortgage prop = prop.mortgaged <- true

let unmortgage prop = prop.mortgaged <- false

let is_mortaged prop = prop.mortgaged

let get_type prop = prop.property_type

let num_for_monopoly prop =
  match get_type prop with
  | Brown -> 2
  | LBlue -> 3
  | Pink -> 3
  | Orange -> 3
  | Red -> 3
  | Yellow -> 3
  | Green -> 3
  | DBlue -> 2
  | Jail -> -1
  | GoToJail -> -1
  | Go -> -1
  | FreeParking -> -1
  | Railroad -> -1
  | Chance -> -1
  | ComChest -> -1
