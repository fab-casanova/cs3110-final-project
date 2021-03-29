open Property

type t = {
  name : string;
  mutable properties : Property.t list;
  mutable money : int;
  mutable position : Property.t;
  mutable monopolies : Property.space_type list;
}

let create_player player_name start =
  {
    name = player_name;
    properties = [];
    money = 1000;
    position = start;
    monopolies = [];
  }

let get_name player = player.name

let get_properties player = player.properties

let player_money player = player.money

let get_position player = player.position

let change_pos player new_pos = player.position <- new_pos

let update_player_money player update = player.money <- player.money + update

let add_property player prop = player.properties <- prop :: player.properties

let add_monopoly player property_type =
  player.monopolies <- property_type :: player.monopolies

let rec check_monopoly_helper player owned_properties property_type
    properties_required acc =
  match owned_properties with
  | [] -> ()
  | h :: t ->
      if get_type h = property_type then
        if acc + 1 = properties_required then add_monopoly player property_type
        else
          check_monopoly_helper player owned_properties property_type
            properties_required (acc + 1)
      else
        check_monopoly_helper player owned_properties property_type
          properties_required acc

let check_monopoly player new_property =
  let property_type = get_type new_property in
  let properties_required = num_for_monopoly new_property in
  let owned_properties = get_properties player in
  check_monopoly_helper player owned_properties property_type
    properties_required 0

let buy_property player prop =
  let price = Property.purchase_price prop in
  if player.money >= price then (
    player.money <- player.money - price;
    player.properties <- prop :: player.properties;
    Property.set_owner prop player.name;
    if can_have_houses prop then check_monopoly player prop)

let num_of_prop is_prop player =
  List.length (List.filter (fun x -> is_prop x) (get_properties player))

let num_of_util = num_of_prop is_utilities

let num_of_rail = num_of_prop is_railroad

let roll_dice () = 2 + Random.int 5 + Random.int 5

let util_rent player =
  let utils_owned = num_of_util player in
  (1 + (3 * utils_owned)) * roll_dice ()

let calculate_rent prop owner =
  let rent = calculate_rent_or_tax prop in
  if is_utilities prop then util_rent owner
  else if is_railroad prop then
    int_of_float (2. ** float_of_int (rent * num_of_rail owner))
  else rent

let has_monopoly player prop =
  let color = get_type prop in
  List.length (List.find_all (fun x -> get_type x = color) player.properties)
  = num_for_monopoly prop

let building_evenly player prop add_or_subtract =
  let monopoly =
    List.find_all (fun x -> get_type x = get_type prop) player.properties
  in
  let rec building_evenly_helper monopoly prop =
    match monopoly with
    | [] -> true
    | h :: t ->
        if Int.abs (num_houses h) - add_or_subtract (num_houses prop) 1 <= 1
        then building_evenly_helper t prop
        else false
  in
  building_evenly_helper monopoly prop

let build_houses_hotel player prop =
  if can_have_houses prop then
    if has_monopoly player prop then
      if building_evenly player prop ( + ) then upgrade_property prop
      else print_string "Must build houses evenly"
    else print_string "Cannot build house without monopoly"
  else print_string "Cannot build house on this property type"

let out_of_cash amount_owed player = amount_owed > player.money

let is_bankrupt amount_owed player =
  let rec property_value player properties total_value =
    match properties with
    | [] -> total_value
    | h :: t ->
        property_value player t
          (total_value + get_value h + (num_houses h * house_cost h / 2))
  in
  amount_owed > player.money + property_value player player.properties 0

let rec no_houses_on_monopoly monopoly prop =
  match monopoly with
  | [] -> true
  | h :: t ->
      if num_houses prop > 0 then false else no_houses_on_monopoly t prop

let owns_property player prop =
  List.exists (fun x -> x = prop) player.properties

let rec mortgage_allowed player prop =
  let monopoly =
    List.find_all (fun x -> get_type x = get_type prop) player.properties
  in
  owns_property player prop && is_mortgaged prop
  && no_houses_on_monopoly monopoly prop

let get_prop_of_name player prop_name =
  match player.properties with [] -> None | h :: t -> None

let rec collect_nonmonetary_rent player owner rent_owed = failwith ""

(*
let rec collect_nonmonetary_rent player owner rent_owed =
  print_string
    "\n\
     Would you like to pay with cash, mortgage, sell buildings, or transfer \
     properties?\n";
  print_string "> ";
  let input = read_line() in
  if input = "pay with cash" then
      if not (out_of_cash rent_owed player) then
        update_player_money owner rent_owed
      else
        print_string
          "Invalid choice, not enough money. You can pay with cash, mortgage, \
           sell buildings, transfer properties";
      collect_nonmonetary_rent player owner rent_owed
  else if input = "mortgage" then print_string "What property do you want to mortgage?";
        let prop = read_line() in
        if mortgage_allowed player prop then player.money <- player.money - (purchase_price prop)/2; create_mortgage prop else print_string "Can't mortgage this property"; collect_nonmonetary_rent player owner rent_owed
      
  else if input = "sell buildings" then print_string "What property do you want to sell buildings on?";
        let prop = read_line() in
        if (owns_property player prop) && (num_houses prop) > 0 && buiding_evenly player prop ( - ) then
        player.money <- player.money + (house_cout prop)/2
        downgrade_property prop
        else print_string "Cannot sell buildings on this property"; collect_nonmonetary_rent player owner rent_owed
  else if input = "transfer properties" then print_string "What property do you want to sell?";
         let prop = read_line() in
         if (owns_property player prop) && (no_houses_on_monopoly monopoly prop)
         then set_owner prop owner; let rent_owed = rent_owed - get_value prop
         else print_string "Can't sell this property"; collect_nonmonetary_rent player owner rent_owed
  els
      print_string
        "\n\
         Invalid choice. You can pay with cash, mortgage, sell buildings, \
         transfer properties\n";
      collect_nonmonetary_rent player owner rent_owed
*)
let bankruptcy player = failwith "Unimplemented"

let collect_rent player owner property =
  let rent_owed = calculate_rent property owner in
  if not (out_of_cash rent_owed player) then (
    if is_owned property then update_player_money owner rent_owed;
    update_player_money player (-1 * rent_owed))
  else if is_bankrupt rent_owed player then bankruptcy player
  else collect_nonmonetary_rent player owner rent_owed

let collect_tax player property =
  let tax = calculate_rent_or_tax property in
  if not (out_of_cash tax player) then update_player_money player (-1 * tax)
  else if is_bankrupt tax player then bankruptcy player
