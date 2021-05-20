open Property

type t = {
  name : string;
  mutable properties : Property.t list;
  mutable money : int;
  mutable position : Property.t;
  mutable monopolies : Property.space_type list;
  mutable jailed : int;
  mutable jail_free : string list;
  mutable doubles : int;
  real_player : bool;
}

let create_t is_real player_name start =
  {
    name = player_name;
    properties = [];
    money = 1500;
    position = start;
    monopolies = [];
    jailed = 0;
    jail_free = [];
    doubles = 0;
    real_player = is_real;
  }

let create_player = create_t true

let create_ai = create_t false

let is_real_player player = player.real_player

let get_name player = player.name

let get_properties player = player.properties

let num_doubles player = player.doubles

let reset_doubles player = player.doubles <- 0

let add_double player = player.doubles <- player.doubles + 1

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

let pp_property_list = pp_helper print_level prop_name 2 3

let pp_properties player =
  let properties = get_properties player in
  if List.length properties > 0 then
    pp_helper print_level prop_name 2 3 (List.rev properties)
  else "none"

let pp_monopolies player =
  let monops = player.monopolies in
  if List.length monops > 0 then
    pp_helper (fun _ -> "") pp_space_type 5 6 monops
  else "none"

let rec pp_jail_free jail_free_cards =
  match jail_free_cards with [] -> "" | h :: t -> h ^ pp_jail_free t

let player_money player = player.money

let get_position player = player.position

let in_jail player = player.jailed > 0

let put_in_jail player = player.jailed <- 3

let time_left player = player.jailed

let served_a_turn player = player.jailed <- max 0 (player.jailed - 1)

let un_jail player = player.jailed <- 0

let change_pos player new_pos = player.position <- new_pos

let update_player_money player update = player.money <- player.money + update

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

let add_property player prop = player.properties <- prop :: player.properties

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

let sum_dice dice = fst dice + snd dice

let debug_dubs = false

let debug_comm = false

let debug_chance = false

let debug_jail = false

let roll_dice () =
  if debug_dubs then (2, 2)
  else if debug_comm then (0, 2)
  else if debug_chance then (2, 5)
  else if debug_jail then (15, 15)
  else (
    Random.self_init ();
    (1 + Random.int 6, 1 + Random.int 6))

let util_rent player =
  let utils_owned = num_of_util player in
  (1 + (3 * utils_owned)) * sum_dice (roll_dice ())

let has_monopoly player prop =
  let color = get_type prop in
  List.length (List.find_all (fun x -> get_type x = color) player.properties)
  = num_for_monopoly prop

let calculate_owned_rent prop owner =
  let rent = calculate_rent_or_tax prop in
  if is_utilities prop then util_rent owner
  else if is_railroad prop then
    rent * int_of_float (2. ** float_of_int (num_of_rail owner - 1))
  else if has_monopoly owner prop then 2 * rent
  else rent

let rec is_building_evenly_helper prop is_building house_per_prop =
  let min, max = (List.hd house_per_prop, List.hd (List.rev house_per_prop)) in
  max - min <= 1 && num_houses prop = if is_building then min else max

let is_building_evenly props prop is_building =
  props
  |> List.filter (fun x -> get_type prop = get_type x)
  |> List.map num_houses |> List.sort_uniq compare
  |> is_building_evenly_helper prop is_building

let can_build_houses_hotel player prop =
  can_have_houses prop && has_monopoly player prop
  && player_money player >= house_cost prop
  && can_be_upgraded prop
  && is_building_evenly player.properties prop true

let buildable_props player =
  List.filter (can_build_houses_hotel player) player.properties

let out_of_cash amount_owed player = amount_owed > player.money

let rec net_worth_helper properties total_value =
  match properties with
  | [] -> total_value
  | h :: t ->
      net_worth_helper t
        (total_value + get_value h + (num_houses h * house_cost h / 2))

let net_worth player = net_worth_helper player.properties 0

let is_bankrupt amount_owed player = amount_owed > net_worth player

let player_status dues player =
  if not (out_of_cash dues player) then 0
  else if not (is_bankrupt dues player) then 1
  else 2

let landed_heads () =
  Random.self_init ();
  Random.int 2 = 1

let three_sided_die () =
  Random.self_init ();
  Random.int 3

let random_property player =
  let lst = player.properties in
  let rec aux = function
    | [] -> aux lst
    | h :: t -> if landed_heads () then h else aux t
  in
  aux lst

let no_houses_on_monopoly player prop =
  let monopoly =
    List.find_all (fun x -> get_type x = get_type prop) player.properties
  in
  let rec no_houses_on_monopoly_helper monopoly player prop =
    match monopoly with
    | [] -> true
    | h :: t ->
        if num_houses h > 0 then false
        else no_houses_on_monopoly_helper t player prop
  in
  no_houses_on_monopoly_helper monopoly player prop

let owns_property player prop = List.mem prop player.properties

let rec allowed_props (player : t) f acc = function
  | [] -> acc
  | h :: t -> allowed_props player f (if f player h then h :: acc else acc) t

let mortgage_allowed player prop =
  owns_property player prop
  && (not (is_mortgaged prop))
  && no_houses_on_monopoly player prop

let mortgageable_props player =
  List.filter (mortgage_allowed player) (get_properties player)

let unmortgageable_props player =
  List.filter
    (fun x -> owns_property player x && is_mortgaged x)
    (get_properties player)

let can_transfer player prop =
  owns_property player prop && no_houses_on_monopoly player prop

let transferable_props player =
  List.filter (can_transfer player) (get_properties player)

let print_assets player =
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    (get_name player ^ "'s money: $"
    ^ string_of_int (player_money player)
    ^ "\n");
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (get_name player ^ "'s properties: " ^ pp_properties player ^ "\n"
   ^ get_name player ^ "'s monopolies: " ^ pp_monopolies player ^ "\n"
    ^ (if List.length player.jail_free > 0 then
       get_name player ^ "'s get out of jail free cards: "
       ^ pp_jail_free player.jail_free
       ^ "\n"
      else "")
    ^
    if num_doubles player > 0 then
      string_of_int (num_doubles player) ^ " consecutive double(s)\n"
    else "")

let remove_property player prop =
  player.properties <- List.filter (fun x -> x <> prop) player.properties

let return_prop_to_bank player prop =
  remove_property player prop;
  release_property prop;
  reset_stage prop

let rec swap_owner_helper giver receiver = function
  | [] -> ()
  | h :: t ->
      set_owner h (get_name receiver);
      add_property receiver h;
      remove_property giver h;
      swap_owner_helper giver receiver t

let swap_owner giver receiver prop = swap_owner_helper giver receiver [ prop ]

let hand_over_all_properties giver receiver =
  swap_owner_helper giver receiver (get_properties giver)

let rec clear_properties_helper player = function
  | [] -> ()
  | h :: t ->
      return_prop_to_bank player h;
      clear_properties_helper player t

let clear_properties player =
  clear_properties_helper player player.properties;
  player.properties <- []

let num_jail_free_cards player = List.length player.jail_free

let add_jail_free_card deck player =
  player.jail_free <- deck :: player.jail_free

let owns_jail_free_card deck player = List.exists (( = ) deck) player.jail_free

let remove_jail_free_card player =
  match player.jail_free with [] -> () | h :: t -> player.jail_free <- t

let remove_monopoly player prop =
  let prop_type = get_type prop in
  player.monopolies <- List.filter (fun x -> x <> prop_type) player.monopolies

let selling_allowed player prop =
  owns_property player prop
  && num_houses prop > 0
  && is_building_evenly (get_properties player) prop false

let sellable_bldg_props player =
  List.filter (selling_allowed player) player.properties
