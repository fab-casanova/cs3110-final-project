type space_type =
  | Brown
  | LBlue
  | Pink
  | Orange
  | Red
  | Yellow
  | Green
  | DBlue
  | OtherColor of string
  | Railroad
  | Jail
  | GoToJail
  | Go
  | FreeParking
  | Chance
  | ComChest
  | IncomeTax
  | OtherNonpurchase of string

let can_purchase_type = function
  | Brown | LBlue | Pink | Orange | Red | Yellow | Green | DBlue | Railroad
  | OtherColor _ ->
      true
  | _ -> false

type property_stage =
  | CannotBuy
  | Other
  | Zero
  | One
  | Two
  | Three
  | Four
  | Hotel

type t = {
  name : string;
  property_type : space_type;
  mutable owner : string;
  mutable stage : property_stage;
  mutable mortgaged : bool;
  rent_prices : (property_stage * int) list;
  price : int;
  house_price : int;
}

let assign_purchaseable = function
  | "brown" -> Brown
  | "light blue" -> LBlue
  | "pink" -> Pink
  | "orange" -> Orange
  | "red" -> Red
  | "yellow" -> Yellow
  | "green" -> Green
  | "dark blue" -> DBlue
  | "railroad" -> Railroad
  | str -> OtherColor str

let assign_non_purchaseable = function
  | "jail" -> Jail
  | "go to jail" -> GoToJail
  | "go" | "pass go" -> Go
  | "free parking" -> FreeParking
  | "chance" -> Chance
  | "community chest" -> ComChest
  | "income tax" -> IncomeTax
  | str -> OtherNonpurchase str

let can_be_purchased card = can_purchase_type card.property_type

let create_t str color own prices cost house =
  {
    name = str;
    property_type = color;
    owner = own;
    stage = (if can_purchase_type color then Zero else CannotBuy);
    mortgaged = false;
    price = cost;
    rent_prices = prices;
    house_price = house;
  }

let create_rent_list (arr : int array) = function
  | Brown | LBlue | Pink | Orange | Red | Yellow | Green | DBlue | OtherColor _
    ->
      if Array.length arr >= 6 then
        [
          (Zero, arr.(0));
          (One, arr.(1));
          (Two, arr.(2));
          (Three, arr.(3));
          (Four, arr.(4));
          (Hotel, arr.(5));
        ]
      else
        [
          (Zero, 6);
          (One, 30);
          (Two, 90);
          (Three, 270);
          (Four, 400);
          (Hotel, 550);
        ]
  | Railroad -> [ (Other, arr.(0)) ]
  | _ -> failwith "Card that cannot be purchased"

let create_buyable_card name card_type prices price house =
  let space = assign_purchaseable card_type in
  create_t name space "" (create_rent_list prices space) price house

let create_unbuyable_card name card_type rent =
  create_t name
    (assign_non_purchaseable card_type)
    "notforsale" [ (CannotBuy, rent) ] (-1) (-1)

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

let upgrade_stage prop =
  match prop.stage with
  | Zero -> prop.stage <- One
  | One -> prop.stage <- Two
  | Two -> prop.stage <- Three
  | Three -> prop.stage <- Four
  | Four -> prop.stage <- Hotel
  | Hotel | CannotBuy | Other -> ()

let downgrade_stage prop =
  match prop.stage with
  | CannotBuy | Other | Zero -> ()
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

let rec upgrade_prop prop = change_stage prop upgrade_stage

let rec downgrade_prop prop = change_stage prop downgrade_stage

let num_for_monopoly prop =
  match get_type prop with
  | Brown | DBlue -> 2
  | LBlue | Pink | Orange | Red | Yellow | Green | OtherColor _ -> 3
  | Jail | GoToJail | Go | FreeParking | Chance | Railroad | ComChest
  | IncomeTax | OtherNonpurchase _ ->
      -1
