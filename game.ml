open Property
open Player

type gameboard = Property.t list

type players = Player.t list

type t = {
  board : gameboard;
  mutable player_list : Player.t list;
  mutable current_player : string;
  mutable money_pot : int;
  mutable chance_deck : int list;
  mutable community_chest_deck : int list;
}

let create_players (lst : Player.t list) : players = lst

let rec pp_players_helper = function
  | [] -> ""
  | [ h ] -> get_name h
  | h :: t -> get_name h ^ ", " ^ pp_players_helper t

let pp_players game = "Players: " ^ pp_players_helper game.player_list

let last_one_standing game = List.length game.player_list <= 1

let rec p_aux = function
  | [] -> ""
  | [ h ] ->
      prop_name h
      ^ (if is_owned h then " (owned by " ^ get_owner_name h ^ ")"
        else " (unowned)")
      ^ "\n"
  | h :: t ->
      prop_name h
      ^ (if is_owned h then " (owned by " ^ get_owner_name h ^ ")"
        else " (unowned)")
      ^ ",\n" ^ p_aux t

let pp_remaining_properties game player prop =
  let color = get_type prop in
  let remaining_props =
    List.filter
      (fun x -> get_type x = color && get_owner_name x <> get_name player)
      game.board
  in
  if List.length remaining_props > 0 then
    "Remaining " ^ prop_space_type prop ^ " properties not owned by "
    ^ get_name player ^ ":\n" ^ p_aux remaining_props
  else ""

let create_gameboard (lst : Property.t list) : gameboard = lst

let pot_amount game = game.money_pot

let add_to_pot game cash = game.money_pot <- game.money_pot + cash

let shuffle_deck () =
  Random.self_init ();
  let num_cards = 16 in
  let rec build_deck num_cards acc =
    if num_cards > 0 then build_deck (num_cards - 1) ((num_cards - 1) :: acc)
    else acc
  in
  let deck = build_deck num_cards [] in
  let cards_with_weights = List.map (fun x -> (Random.bits (), x)) deck in
  let shuffled_with_weights = List.sort compare cards_with_weights in
  List.map fst shuffled_with_weights

let draw_card deck game =
  let check_chance_shuffle cur_deck =
    if cur_deck = [] then game.chance_deck <- shuffle_deck ()
  in
  let check_com_chest_shuffle cur_deck =
    if cur_deck = [] then game.community_chest_deck <- shuffle_deck ()
  in
  match deck with
  | "chance" -> (
      check_chance_shuffle game.chance_deck;
      match game.chance_deck with
      | [] -> -1
      | h :: t ->
          game.chance_deck <- t;
          h)
  | "community chest" -> (
      check_com_chest_shuffle game.community_chest_deck;
      match game.community_chest_deck with
      | [] -> -1
      | h :: t ->
          game.community_chest_deck <- t;
          h)
  | _ -> -1

let cash_out_pot game player =
  update_player_money player game.money_pot;
  game.money_pot <- 0

let owns_property_of_name player name game =
  if List.exists (fun x -> prop_name x = name) game.board then
    let prop = List.find (fun x -> prop_name x = name) game.board in
    get_owner_name prop = get_name player
  else false

let create_game (board : gameboard) (players : players) =
  {
    board;
    player_list = players;
    current_player = (match players with [] -> "" | h :: t -> get_name h);
    money_pot = 0;
    chance_deck = [ 0 ];
    community_chest_deck = [ 0 ];
  }

let get_property_of_name name game =
  List.find (fun prop -> prop_name prop = name) game.board

let get_start_pos game = List.hd game.board

let num_players game = List.length game.player_list

let current_player_name game = game.current_player

let still_in_game player game = List.mem player game.player_list

let get_players game = game.player_list

let find_player name lst = List.find (fun x -> get_name x = name) lst

let get_owner prop game = find_player (get_owner_name prop) (get_players game)

let get_jail game = List.find is_jail game.board

let current_player game =
  List.find (fun x -> get_name x = current_player_name game) (get_players game)

let add_a_player game player =
  if List.length game.player_list > 3 then
    print_endline "Max number of players reached, cannot add more\n"
  else
    let name_lst = List.map get_name game.player_list in
    if not (List.mem (get_name player) name_lst) then
      game.player_list <- game.player_list @ [ player ];
    if game.current_player = "" then game.current_player <- get_name player

let move_front_to_back = function [] -> [] | h :: t -> t @ [ h ]

let move_to_next_player game =
  game.player_list <- move_front_to_back game.player_list;
  game.current_player <- get_name (List.hd game.player_list)

let rec get_index_helper board (prop : Property.t) acc =
  match board with
  | [] -> raise Not_found
  | h :: t -> if prop = h then acc else get_index_helper t prop (acc + 1)

let get_index (game : t) (prop : Property.t) =
  get_index_helper game.board prop 0

let rec get_prop_at_index_helper index (board : gameboard) =
  match board with
  | [] -> raise Not_found
  | h :: t -> if index = 0 then h else get_prop_at_index_helper (index - 1) t

let get_prop_at_index index game = get_prop_at_index_helper index game.board

let get_new_pos_and_double player the_game =
  let moves = roll_dice () in
  print_endline
    ("\n" ^ get_name player ^ " rolled a "
    ^ string_of_int (fst moves)
    ^ " and a "
    ^ string_of_int (snd moves)
    ^ " ("
    ^ string_of_int (sum_dice moves)
    ^ ")");
  let new_pos =
    player |> get_position |> get_index the_game |> ( + ) (sum_dice moves)
  in
  (new_pos, fst moves = snd moves)

let move_player player game given_moves special_move =
  let old_pos = get_position player in
  let roll_outcome =
    if special_move = false then get_new_pos_and_double player game
    else (get_index game old_pos + given_moves, not special_move)
  in
  let new_index = fst roll_outcome mod 40 in
  if snd roll_outcome then add_double player else reset_doubles player;
  if num_doubles player < 3 then (
    let new_position = get_prop_at_index new_index game in
    change_pos player new_position;
    if new_index - get_index game old_pos < 0 && not special_move then (
      print_endline "Passed go, collect $200";
      update_player_money player 200))
  else (
    put_in_jail player;
    change_pos player (get_property_of_name "Jail" game))

let pay_with_cash player rent_owed = update_player_money player (-1 * rent_owed)

(* let mortgage_property player game = ANSITerminal.print_string [
   ANSITerminal.blue ] "\nWhat property would you like to mortgage?\n"; let
   input = read_line () in if owns_property_of_name player input game then let
   prop = get_property_of_name input game in let can_mortgage = mortgage_allowed
   player prop in if can_mortgage then ( update_player_money player
   (purchase_price prop / 2); create_mortgage prop) else
   ANSITerminal.print_string [ ANSITerminal.blue ] "\nCan't mortgage this
   property\n" else ANSITerminal.print_string [ ANSITerminal.red ] "\nInvalid
   property name. Please enter a valid property\n"*)
(* let sell_buildings player prop game = ANSITerminal.print_string [
   ANSITerminal.blue ] "What property do you want to sell buildings on?\n"; let
   input = read_line () in if owns_property_of_name player input game then let
   prop = get_property_of_name input game in let selling_allowed = owns_property
   player prop && num_houses prop > 0 && is_building_evenly (get_properties
   player) prop false in if selling_allowed then ( update_player_money player
   (house_cost prop / 2); downgrade_property prop) else
   ANSITerminal.print_string [ ANSITerminal.blue ] "Cannot sell buildings on
   this property"*)

(*let transfer_properties player owner prop game = ANSITerminal.print_string [
  ANSITerminal.blue ] "What property do you want to sell?\n"; let input =
  read_line () in if owns_property_of_name player input game then ( let prop =
  get_property_of_name input game in let can_transfer = owns_property player
  prop && no_houses_on_monopoly player prop in let owned = is_owned prop in if
  (not owned) && can_transfer then return_prop_to_bank player prop else if
  can_transfer then ( swap_owner player owner prop; update_player_money player
  (get_value prop))) else print_string "Can't sell this property"*)

(*make pay with cash automatic *)
(* let rec collect_nonmonetary_payment player prop rent_owed game = let owner =
   if is_owned prop then get_owner prop game else player in print_assets player;
   ANSITerminal.print_string [ ANSITerminal.blue ] "\n\ Would you like to pay
   with cash, mortgage, sell buildings, or transfer \ properties?\n";
   ANSITerminal.print_string [ ANSITerminal.blue ] "> "; match read_line () with
   | "pay with cash" | "cash" -> if not (out_of_cash rent_owed player) then
   pay_with_cash player rent_owed else ( ANSITerminal.print_string [
   ANSITerminal.blue ] "\nInvalid choice, not enough money.\n";
   collect_nonmonetary_payment player prop rent_owed game) | "mortgage" ->
   mortgage_property player game; collect_nonmonetary_payment player prop
   rent_owed game | "sell buildings" -> sell_buildings player prop game;
   collect_nonmonetary_payment player prop rent_owed game | "transfer
   properties" -> transfer_properties player owner prop game;
   collect_nonmonetary_payment player prop rent_owed game | _ ->
   ANSITerminal.print_string [ ANSITerminal.blue ] "\nInvalid input, please try
   again\n"; collect_nonmonetary_payment player prop rent_owed game *)

(* TODO: bankruptcy should remove the player from player_list and return their
   properties to unowned,*)
let remove_player player game =
  game.player_list <-
    List.filter (fun x -> get_name x <> get_name player) game.player_list;
  if game.current_player = get_name player then move_to_next_player game

let calculate_dues prop game =
  if is_owned prop then
    let owner = get_owner prop game in
    calculate_owned_rent prop owner
  else calculate_rent_or_tax prop

let forfeit player game =
  add_to_pot game (player_money player);
  clear_properties player;
  remove_player player game

let bankruptcy player prop game =
  if is_owned prop then (
    let owner = get_owner prop game in
    update_player_money owner (player_money player);
    hand_over_all_properties player owner)
  else forfeit player game

let collect_dues player prop dues game =
  update_player_money player (-1 * dues);
  if is_owned prop then
    let owner = get_owner prop game in
    update_player_money owner dues
  else add_to_pot game dues
