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

type property_stage = CannotBuy | Zero | One | Two | Three | Four | Hotel

type property = {
  name : string;
  property_type : property_type;
  mutable owner : string;
  mutable stage : property_stage;
  mutable mortgaged : bool;
  rent_prices : (property_stage * int) list;
}

let calculate_rent prop = List.assoc prop.stage prop.rent_prices

let get_owner prop = prop.owner

let current_stage prop = prop.stage

let mortgage prop = prop.mortgaged <- true

let unmortgage prop = prop.mortgaged <- false

let is_mortaged prop = prop.mortgaged
