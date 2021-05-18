type space_type =
  | Brown
  | LBlue
  | Pink
  | Orange
  | Red
  | Yellow
  | Green
  | DBlue
  | Railroad
  | Utilities
  | OtherColor of string
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
  | Utilities | OtherColor _ ->
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
  per_house_cost : int;
}

let prop_name prop = prop.name

let pp_space_type = function
  | Brown -> "brown"
  | LBlue -> "light blue"
  | Pink -> "pink"
  | Orange -> "orange"
  | Red -> "red"
  | Yellow -> "yellow"
  | Green -> "green"
  | DBlue -> "dark blue"
  | Railroad -> "railroad"
  | Utilities -> "utilities"
  | OtherColor str -> str
  | Jail -> "visiting jail"
  | GoToJail -> "go to jail"
  | Go -> "pass go"
  | FreeParking -> "free parking"
  | Chance -> "chance"
  | ComChest -> "community chest"
  | IncomeTax -> "income tax"
  | OtherNonpurchase str -> str

let prop_space_type prop = pp_space_type prop.property_type

let can_be_purchased prop = can_purchase_type prop.property_type

let can_be_upgraded prop =
  match prop.stage with CannotBuy | Other | Hotel -> false | _ -> true

let what_stage prop =
  match prop.stage with
  | CannotBuy -> "space cannot be purchased"
  | Other -> "non-house property (cannot have houses)"
  | Zero -> "zero"
  | One -> "one"
  | Two -> "two"
  | Three -> "three"
  | Four -> "four"
  | Hotel -> "hotel"

let space_type_has_houses = function
  | Brown | LBlue | Pink | Orange | Red | Yellow | Green | DBlue | OtherColor _
    ->
      true
  | _ -> false

let can_have_houses prop = space_type_has_houses prop.property_type

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
  | "utilities" -> Utilities
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

let is_owned prop = prop.owner <> ""

let is_com_or_chance prop =
  match prop.property_type with Chance | ComChest -> true | _ -> false

let is_utilities prop =
  match prop.property_type with Utilities -> true | _ -> false

let is_free_parking prop =
  match prop.property_type with FreeParking -> true | _ -> false

let is_railroad prop =
  match prop.property_type with Railroad -> true | _ -> false

let is_tax prop = match prop.property_type with IncomeTax -> true | _ -> false

let is_jail prop = match prop.property_type with Jail -> true | _ -> false

let is_go_to_jail prop =
  match prop.property_type with GoToJail -> true | _ -> false

let create_t str color own prices cost house_price =
  {
    name = str;
    property_type = color;
    owner = own;
    stage =
      ( if can_purchase_type color then
        if space_type_has_houses color then Zero else Other
      else CannotBuy );
    mortgaged = false;
    price = cost;
    rent_prices = prices;
    per_house_cost = house_price;
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
  | Railroad | Utilities -> [ (Other, arr.(0)) ]
  | _ -> failwith "Card that cannot be purchased"

let create_buyable_card name card_type prices price house_price =
  let space = assign_purchaseable card_type in
  create_t name space "" (create_rent_list prices space) price house_price

let create_unbuyable_card name card_type rent =
  create_t name
    (assign_non_purchaseable card_type)
    "" [ (CannotBuy, rent) ] (-1) (-1)

let calculate_rent_or_tax prop = List.assoc prop.stage prop.rent_prices

let purchase_price prop = prop.price

let house_cost prop = prop.per_house_cost

let get_owner_name prop = prop.owner

let set_owner prop owner = prop.owner <- owner

let release_property prop = set_owner prop ""

let reset_stage prop =
  match prop.stage with
  | CannotBuy | Other | Zero -> ()
  | One | Two | Three | Four | Hotel -> prop.stage <- Zero

let current_stage prop = prop.stage

let create_mortgage prop = prop.mortgaged <- true

let unmortgage prop = prop.mortgaged <- false

let is_mortgaged prop = prop.mortgaged

let get_type prop = prop.property_type

let upgrade_property prop =
  match prop.stage with
  | Zero -> prop.stage <- One
  | One -> prop.stage <- Two
  | Two -> prop.stage <- Three
  | Three -> prop.stage <- Four
  | Four -> prop.stage <- Hotel
  | Hotel | CannotBuy | Other -> ()

let downgrade_property prop =
  match prop.stage with
  | Zero -> prop.stage <- Zero
  | One -> prop.stage <- Zero
  | Two -> prop.stage <- One
  | Three -> prop.stage <- Two
  | Four -> prop.stage <- Three
  | Hotel -> prop.stage <- Four
  | CannotBuy | Other -> ()

(* maybe move this out later so it changes depending on # of each colors *)
let num_for_monopoly prop =
  match get_type prop with
  | Brown | DBlue -> 2
  | LBlue | Pink | Orange | Red | Yellow | Green | OtherColor _ -> 3
  | Jail | GoToJail | Go | FreeParking | Chance | Railroad | ComChest
  | IncomeTax | OtherNonpurchase _ | Utilities ->
      -1

let num_houses prop =
  match prop.stage with
  | CannotBuy | Other -> 0
  | Zero -> 0
  | One -> 1
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Hotel -> 5

let get_value prop = purchase_price prop / (1 + Bool.to_int (is_mortgaged prop))
