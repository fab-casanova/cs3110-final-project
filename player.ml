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

let rec clear_properties_helper = function
  | [] -> ()
  | h :: t ->
      return_prop_to_bank h;
      clear_properties_helper t

let clear_properties player =
  clear_properties_helper player.properties;
  player.properties <- []

let print_level prop =
  " ("
  ^ (if is_mortgaged prop then "MORTGAGED " else "")
  ^ prop_space_type prop
  ^ (if can_have_houses prop then " " ^ what_stage prop else "")
  ^ ")"

let rec pp_helper f name acc global_acc = function
  | [] -> ""
  | h :: t when List.length t > 0 ->
      if acc <= 0 then
        name h ^ f h ^ ",\n" ^ pp_helper f name global_acc global_acc t
      else name h ^ f h ^ ", " ^ pp_helper f name (acc - 1) global_acc t
  | h :: _ -> name h ^ f h

let pp_properties player =
  let properties = get_properties player in
  pp_helper print_level prop_name 2 3 (List.rev properties)

let pp_monopolies player =
  let monops = player.monopolies in
  pp_helper (fun _ -> "") pp_space_type 5 6 monops

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
          check_monopoly_helper player t property_type properties_required
            (acc + 1)
      else check_monopoly_helper player t property_type properties_required acc

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
    if can_have_houses prop then check_monopoly player prop)

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

let rec is_building_evenly_helper prop building house_per_prop =
  let min, max = (List.hd house_per_prop, List.hd (List.rev house_per_prop)) in
  max - min <= 1 && num_houses prop = if building then min else max

let is_building_evenly props prop building_props =
  props
  |> List.filter (fun x -> get_type prop = get_type x)
  |> List.map num_houses |> List.sort_uniq compare
  |> is_building_evenly_helper prop building_props

let can_build_houses_hotel player prop =
  can_have_houses prop && has_monopoly player prop
  && player_money player >= house_cost prop
  && can_be_upgraded prop
  && is_building_evenly player.properties prop true

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

let no_houses_on_monopoly player prop =
  let monopoly =
    List.find_all (fun x -> get_type x = get_type prop) player.properties
  in
  let rec no_houses_on_monopoly_helper monopoly player prop =
    match monopoly with
    | [] -> true
    | h :: t ->
        if num_houses prop > 0 then false
        else no_houses_on_monopoly_helper t player prop
  in
  no_houses_on_monopoly_helper monopoly player prop

let owns_property player prop =
  List.exists (fun x -> x = prop) player.properties

let rec mortgage_allowed player prop =
  owns_property player prop
  && (not (is_mortgaged prop))
  && no_houses_on_monopoly player prop

let get_prop_of_name player name =
  List.find (fun x -> prop_name x = name) player.properties

let assets player =
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    (get_name player ^ "'s money: $"
    ^ string_of_int (player_money player)
    ^ "\n");
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (get_name player ^ "'s properties: " ^ pp_properties player ^ "\n"
   ^ get_name player ^ "'s monopolies: " ^ pp_monopolies player)
