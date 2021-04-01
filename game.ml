open Property
open Player

type gameboard = Property.t list

type players = Player.t list

type t = {
  board : gameboard;
  mutable player_list : players;
  mutable current_player : string;
}

let create_players (lst : Player.t list) : players = lst

let create_gameboard (lst : Property.t list) : gameboard = lst

let owns_property_of_name player name game =
  if List.exists (fun x -> prop_name x = name) game.board then
    let prop = List.find (fun x -> prop_name x = name) game.board in
    get_owner prop = get_name player
  else false

let create_game (board : gameboard) (players : players) =
  { board; player_list = players; current_player = "" }

let get_start_pos game = List.hd game.board

let num_players game = List.length game.player_list

let current_player_name game = game.current_player

let get_players game = game.player_list

let find_player player_name plyr_lst =
  List.find (fun x -> get_name x = player_name) plyr_lst

let current_player game =
  List.find (fun x -> get_name x = current_player_name game) (get_players game)

let add_a_player game player =
  if List.length game.player_list > 3 then
    print_endline "Max number of players reached, cannot add more\n"
  else
    let name_lst = List.map get_name game.player_list in
    if List.mem (get_name player) name_lst then
      print_endline "This name is already taken\n"
    else game.player_list <- game.player_list @ [ player ];
    if game.current_player = "" then game.current_player <- get_name player;
    print_endline ("player count: " ^ string_of_int (num_players game))

let move_front_to_back = function [] -> [] | h :: t -> t @ [ h ]

let move_to_next_player game =
  game.player_list <- move_front_to_back game.player_list;
  game.current_player <- get_name (List.hd game.player_list)

let rec get_index_helper board (prop : Property.t) acc =
  match board with
  | [] -> raise Not_found
  | h :: t -> if prop = h then acc else get_index_helper t prop (acc + 1)

let get_index (the_game : t) (prop : Property.t) =
  get_index_helper the_game.board prop 0

let rec get_prop_at_index_helper index (board : gameboard) =
  match board with
  | [] -> raise Not_found
  | h :: t -> if index = 0 then h else get_prop_at_index_helper (index - 1) t

let get_prop_at_index index the_game =
  get_prop_at_index_helper index the_game.board

let get_new_position player the_game =
  let moves = roll_dice () in
  print_endline ("\n" ^ get_name player ^ " rolled a " ^ string_of_int moves);
  player |> get_position |> get_index the_game |> ( + ) moves

let move_player player game =
  let old_pos = get_position player in
  let new_index = get_new_position player game mod 40 in
  let new_position = get_prop_at_index new_index game in
  change_pos player new_position;
  if new_index - get_index game old_pos < 0 then (
    print_endline "Passed go, collect $200";
    update_player_money player 200)

(*TODO: Finish auction, should take game in as parameter*)

(*let rec auction highest_bidder prop bid_price player_list game =
  match player_list with
  | [] ->
      auction highest_bidder prop bid_price player_list game
      (*Doesn't matter what happens here*)
  | h :: t ->
      if bid_price > 0 && h == highest_bidder then (
        let price = bid_price in
        update_player_money h (-price);
        add_property h prop;
        set_owner prop (get_name h);
        check_monopoly h prop)


    else 
      print_string "Enter your bid value";
      (*new_bid =  (*Enter bid value here*)*)
      (*Make sure they have enough money, if not add print statement and request again*)
      if (new_bid > bid_price) then
        (*set bid price to new bid*)
        let bid_price = new_bid 
        (*Set highest bidder to current highest bidder*)
        highest_bidder = h
      (*Recursively call auction with new data*)
        auction highest_bidder prop bid_price (t @ [h]) 
      if (new_bid <> 0) then
          (*Print prompt that tells user to select price over $0 or to input $0 if 
              they dont want to bid*)
          print_string "The value you entered must be greater than the current 
              bid price. If you wish to not bid anything, please enter 0";;
          auction highest_bidder prop bid_price ([h] @ t)
              *)

(*TODO: finish collect nonmonetary rent*)

let rec collect_nonmonetary_payment player prop rent_owed game =
  let owner =
    if is_owned prop then find_player (get_owner prop) game.player_list
    else player
  in
  assets player;
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\
     Would you like to pay with cash, mortgage, sell buildings, or transfer \
     properties?\n";
  ANSITerminal.print_string [ ANSITerminal.blue ] "> ";
  match read_line () with
  | "pay with cash" | "cash" ->
      if not (out_of_cash rent_owed player) then
        update_player_money player (-1 * rent_owed)
      else (
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "Invalid choice, not enough money. You can pay with cash, mortgage, \
           sell buildings, transfer properties";
        collect_nonmonetary_payment player prop rent_owed game)
  | "mortgage" ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "\nWhat property would you like to mortgage?\n";
      let input = read_line () in
      print_string "-1";
      if owns_property_of_name player input game then
        let prop = get_prop_of_name player input in
        let can_mortgage = mortgage_allowed player prop in
        if can_mortgage then (
          update_player_money player (purchase_price prop / 2);
          create_mortgage prop)
        else
          ANSITerminal.print_string [ ANSITerminal.blue ]
            "\nCan't mortgage this property\n"
      else
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\nInvalid property name. Please enter a valid property\n";
      collect_nonmonetary_payment player prop rent_owed game
  | "sell buildings" ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "What property do you want to sell buildings on?\n";
      let input = read_line () in
      if owns_property_of_name player input game then (
        let prop = get_prop_of_name player input in
        let selling_allowed =
          owns_property player prop
          && num_houses prop > 0
          && is_building_evenly (get_properties player) prop false
        in
        if selling_allowed then (
          update_player_money player (house_cost prop / 2);
          downgrade_property prop)
        else
          ANSITerminal.print_string [ ANSITerminal.blue ]
            "Cannot sell buildings on this property";
        collect_nonmonetary_payment player prop rent_owed game)
  | "transfer properties" ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "What property do you want to sell?\n";
      let input = read_line () in
      if owns_property_of_name player input game then (
        let prop = get_prop_of_name player input in
        let can_transfer =
          owns_property player prop && no_houses_on_monopoly player prop
        in
        let owned = is_owned prop in
        if owned && can_transfer then set_owner prop (get_name owner)
        else if (not owned) && can_transfer then set_owner prop "";
        if can_transfer then
          collect_nonmonetary_payment player prop
            (rent_owed - get_value prop)
            game;
        if not can_transfer then (
          print_string "Can't sell this property";
          collect_nonmonetary_payment player prop rent_owed game))
      else
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "Cannot sell buildings on this property";
      collect_nonmonetary_payment player prop rent_owed game
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "\nInvalid input, please try again\n";
      collect_nonmonetary_payment player prop rent_owed game

(* TODO: bankruptcy should remove the player from player_list and return
  their properties to unowned,*)
let remove_player player game =
  game.player_list <-
    List.filter (fun x -> get_name x <> get_name player) game.player_list

let bankruptcy player game =
  clear_properties player;
  remove_player player game;
  ANSITerminal.print_string [ ANSITerminal.red ]
    ("\n" ^ get_name player ^ " has been removed from the game.")

let collect_rent player owner property game =
  let rent_owed = calculate_rent property owner in
  if not (out_of_cash rent_owed player) then (
    if is_owned property then update_player_money owner rent_owed;
    update_player_money player (-1 * rent_owed))
  else if is_bankrupt rent_owed player then bankruptcy player game
  else collect_nonmonetary_payment player property rent_owed game

let collect_tax player property game =
  let tax = calculate_rent_or_tax property in
  if not (out_of_cash tax player) then update_player_money player (-1 * tax)
  else if out_of_cash tax player then
    collect_nonmonetary_payment player property tax game
  else if is_bankrupt tax player then bankruptcy player game
