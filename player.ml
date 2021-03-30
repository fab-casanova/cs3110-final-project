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

let print_level prop =
  " (" ^ prop_space_type prop
  ^ (if can_have_houses prop then " " ^ what_stage prop else "")
  ^ ")"

let rec pp_properties_helper acc = function
  | [] -> ""
  | h :: t when List.length t > 0 ->
      if acc <= 0 then
        prop_name h ^ print_level h ^ ",\n" ^ pp_properties_helper 4 t
      else prop_name h ^ print_level h ^ ", " ^ pp_properties_helper (acc - 1) t
  | h :: _ -> prop_name h ^ print_level h

let pp_properties player =
  let properties = get_properties player in
  pp_properties_helper 3 (List.rev properties)

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
  let price = purchase_price prop in
  if player.money >= price then (
    player.money <- player.money - price;
    add_property player prop;
    set_owner prop player.name;
    if can_have_houses prop then check_monopoly player prop )

let num_of_prop is_prop player =
  List.length (List.filter (fun x -> is_prop x) (get_properties player))

let num_of_util = num_of_prop is_utilities

let num_of_rail = num_of_prop is_railroad

let roll_dice () =
  Random.self_init ();
  2 + Random.int 5 + Random.int 5

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

let can_build_houses_hotel player prop =
  can_have_houses prop && has_monopoly player prop
  && player_money player >= house_cost prop
  && building_evenly player prop ( + )

(*
let build_houses_hotel player prop =
  if can_have_houses prop then
    if has_monopoly player prop then
      if building_evenly player prop ( + ) then upgrade_property prop
      else print_string "Must build houses evenly\n"
    else print_string "Cannot build house without monopoly\n"
  else print_string "Cannot build house on this property type\n"
*)

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

let rec no_houses_on_monopoly player prop =
  let monopoly =
    List.find_all (fun x -> get_type x = get_type prop) player.properties
  in
  match monopoly with
  | [] -> true
  | h :: t ->
      if num_houses prop > 0 then false else no_houses_on_monopoly player prop

let owns_property player prop =
  List.exists (fun x -> x = prop) player.properties

let rec mortgage_allowed player prop =
  owns_property player prop && is_mortgaged prop
  && no_houses_on_monopoly player prop

let get_prop_of_name player name =
  List.find (fun x -> prop_name x = name) player.properties
