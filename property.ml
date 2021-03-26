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

let is_mortaged prop = prop.mortgaged

let current_stage prop = prop.stage

let create_mortgage prop = prop.mortgaged <- true

let unmortgage prop = prop.mortgaged <- false

let is_mortaged prop = prop.mortgaged

let get_type prop = prop.property_type

let next_stage prop =
  match prop.stage with
  | CannotBuy -> ()
  | Zero -> prop.stage <- One
  | One -> prop.stage <- Two
  | Two -> prop.stage <- Three
  | Three -> prop.stage <- Four
  | Four -> prop.stage <- Hotel
  | Hotel -> ()

let prev_stage prop =
  match prop.stage with
  | CannotBuy -> ()
  | Zero -> ()
  | One -> prop.stage <- Zero
  | Two -> prop.stage <- One
  | Three -> prop.stage <- Two
  | Four -> prop.stage <- Three
  | Hotel -> prop.stage <- Four

let rec change_stage prop f = function
  | 0 -> prop
  | i ->
      f prop;
      change_stage prop f (i - 1)

let rec upgrade_prop prop = change_stage prop next_stage

let rec downgrade_prop prop = change_stage prop prev_stage

let num_for_monopoly prop =
  match get_type prop with
  | Brown | DBlue -> 2
  | LBlue | Pink | Orange | Red | Yellow | Green -> 3
  | Jail | GoToJail | Go | FreeParking | Chance | Railroad | ComChest -> -1
