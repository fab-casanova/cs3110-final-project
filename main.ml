open Property
open Player
open Game

(* TODO: add remaining properties comment, money gained from selling comment *)
let rec buy_prompt game player pos =
  ANSITerminal.print_string [ ANSITerminal.blue ] "You currently have ";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    ("$" ^ string_of_int (player_money player));
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (".\nWould you like to purchase " ^ prop_name pos ^ " for ");
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    ("$" ^ string_of_int (purchase_price pos));
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "? Type 'y' if yes, 'n' for no\n";
  match read_line () with
  | "y" ->
      buy_property player pos;
      ANSITerminal.print_string [ ANSITerminal.green ]
        ("\n" ^ get_name player ^ " now owns " ^ prop_name pos ^ "\n")
  | "n" ->
      print_endline ("\n" ^ prop_name pos ^ " was not bought")
      (*TODO: prompt an auction if prop is not bought*)
  | _ ->
      print_endline "Please respond  with 'y' or 'n'";
      buy_prompt game player pos

let rec build_prompt player prop =
  ANSITerminal.print_string [ ANSITerminal.blue ] "You currently have ";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    ("$" ^ string_of_int (player_money player));
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (".\nYou have a monopoly on " ^ prop_space_type prop
   ^ " properties. You can build a house on " ^ prop_name prop ^ " for ");
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    ("$" ^ string_of_int (house_cost prop));
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ".\nType 'y' to build a house here, and 'n' to pass.\n";
  match read_line () with
  | "y" ->
      upgrade_property prop;
      update_player_money player (-1 * house_cost prop);
      ANSITerminal.print_string [ ANSITerminal.green ]
        (prop_name prop ^ " is now at Level " ^ what_stage prop ^ "\n")
  | "n" -> print_endline ("\n" ^ prop_name prop ^ " did not have a house built")
  | _ ->
      print_endline "Please respond with 'y' or 'n'";
      build_prompt player prop

let rec check_build player props =
  match props with
  | [] -> ()
  | h :: t ->
      if can_build_houses_hotel player h then build_prompt player h;
      check_build player t

let rec auction_all_props old_props game =
  match old_props with
  | [] -> ()
  | h :: t ->
      (*auction h game;*)
      auction_all_props t game

let mortgage_property player game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\nWhat property would you like to mortgage?\n";
  let input = read_line () in
  if owns_property_of_name player input game then
    let prop = get_property_of_name input game in
    let can_mortgage = mortgage_allowed player prop in
    if can_mortgage then (
      update_player_money player (purchase_price prop / 2);
      create_mortgage prop)
    else
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "\nCan't mortgage this property\n"
  else
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\nInvalid property name. Please enter a valid property\n"

let sell_buildings player prop game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "What property do you want to sell buildings on?\n";
  let input = read_line () in
  if owns_property_of_name player input game then
    let prop = get_property_of_name input game in
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
        "Cannot sell buildings on this property"

let transfer_properties player owner prop game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "What property do you want to sell?\n";
  let input = read_line () in
  if owns_property_of_name player input game then (
    let prop = get_property_of_name input game in
    let can_transfer =
      owns_property player prop && no_houses_on_monopoly player prop
    in
    let owned = is_owned prop in
    if (not owned) && can_transfer then return_prop_to_bank player prop
    else if can_transfer then (
      swap_owner player owner prop;
      update_player_money player (get_value prop)))
  else print_string "Can't sell this property"

let rec collect_nonmonetary_payment player prop rent_owed game =
  let owner = if is_owned prop then get_owner prop game else player in
  print_assets player;
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
          "\nInvalid choice, not enough money.\n";
        collect_nonmonetary_payment player prop rent_owed game)
  | "mortgage" ->
      mortgage_property player game;
      collect_nonmonetary_payment player prop rent_owed game
  | "sell buildings" ->
      sell_buildings player prop game;
      collect_nonmonetary_payment player prop rent_owed game
  | "transfer properties" ->
      transfer_properties player owner prop game;
      collect_nonmonetary_payment player prop rent_owed game
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "\nInvalid input, please try again\n";
      collect_nonmonetary_payment player prop rent_owed game

let check_status player pos dues game =
  match player_status dues player with
  | 0 -> collect_dues player pos dues game
  | 1 -> collect_nonmonetary_payment player pos dues game
  | _ ->
      let owned = is_owned pos in
      let old_props = get_properties player in
      bankruptcy player pos game;
      if not owned then auction_all_props old_props game

let current_property_effects game player =
  let pos = get_position player in
  print_endline ("Position " ^ string_of_int (get_index game pos) ^ " of 40");
  if not (is_go_to_jail pos) then (
    if is_com_or_chance pos then
      print_endline ("Draw a " ^ prop_space_type pos ^ " card!");
    if is_tax pos then (
      print_endline "Tax will be collected";
      let dues = calculate_dues pos game in
      check_status player pos dues game)
    else if is_owned pos then (
      print_endline (prop_name pos ^ " is owned");
      let owner = get_owner pos game in
      print_endline ("The owner of " ^ prop_name pos ^ " is " ^ get_name owner);
      if owner = player then print_endline "This is your property"
      else
        let dues = calculate_dues pos game in
        check_status player pos dues game)
    else if can_be_purchased pos then (
      print_endline
        (prop_name pos ^ " can be purchased for $"
        ^ string_of_int (purchase_price pos)
        ^ "\n");
      if player_money player >= purchase_price pos then
        buy_prompt game player pos
      else
        ANSITerminal.print_string [ ANSITerminal.red ]
          "Not enough funds to purchase\n\n");
    check_build player (get_properties player))
  else (
    ANSITerminal.print_string [ ANSITerminal.red ] "You're going to jail\n";
    put_in_jail player;
    change_pos player (get_jail game);
    ANSITerminal.print_string [ ANSITerminal.red ]
      (string_of_int (time_left player)
      ^ " turn(s) left in jail for" ^ get_name player ^ "\n"))

let rec play_a_turn game =
  let player = current_player game in
  let old_doubles = num_doubles player in
  move_player player game 0 false;
  if num_doubles player >= 3 then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Rolled 3 doubles. Going to jail. Serve 3 turns";
    change_pos player (get_jail game);
    reset_doubles player)
  else
    let pos = get_position player in
    print_endline
      (get_name player ^ " is at: " ^ prop_name pos ^ " (" ^ prop_space_type pos
     ^ ")");
    current_property_effects game player;
    if still_in_game player game then (
      print_assets player;
      if num_doubles player > old_doubles && not (in_jail player) then (
        (match num_doubles player with
        | 1 ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "Rolled a double. Go again"
        | _ ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "Rolled a second double. Go again\n");
        play_a_turn game))

let attempt_escape player game =
  let roll = roll_dice () in
  if fst roll = snd roll then (
    let sum = sum_dice roll in
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      ("Rolled doubles of "
      ^ string_of_int (fst roll)
      ^ "! Escaped!\nMoving " ^ string_of_int sum ^ " spaces\n");
    un_jail player;
    move_player player game sum true;
    current_property_effects game player)
  else
    ANSITerminal.print_string [ ANSITerminal.yellow ] "Did not roll doubles!\n"

let rec jail_prompt player game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("Would you like to "
    ^ (if player_money player >= 500 && time_left player > 1 then
       "pay $500 (pay) or"
      else "")
    ^ "attempt to roll doubles (roll) to escape jail? Type 'no' to remain in \
       jail\n");
  match read_line () with
  | "pay" ->
      if player_money player < 500 then (
        ANSITerminal.print_string [ ANSITerminal.red ] "\nNot enough money\n";
        jail_prompt player game)
      else if time_left player <= 1 then (
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\nCannot pay to get out in your last turn of jail\n";
        jail_prompt player game)
      else (
        update_player_money player (-500);
        un_jail player)
  | "roll" -> attempt_escape player game
  | "remain" | "no" -> ()
  | _ -> jail_prompt player game

let rec current_turn game =
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("\nCurrent player: " ^ get_name (current_player game));
  let curr = current_player game in
  if not (in_jail curr) then (
    print_endline " NOT IN JAIL";
    play_a_turn game)
  else (
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("\n" ^ get_name curr ^ " is in jail\n");
    (* (let pos = get_position curr in print_endline (get_name curr ^ " is at: "
       ^ prop_name pos ^ " (" ^ prop_space_type pos ^ ")")); *)
    jail_prompt curr game;
    if in_jail curr then (
      served_a_turn curr;
      ANSITerminal.print_string [ ANSITerminal.blue ]
        (string_of_int (time_left curr)
        ^ " turn(s) left in jail for " ^ get_name curr ^ "\n")));

  ANSITerminal.print_string [ ANSITerminal.green ]
    "\nContinue playing? Enter 'q' to quit\n";
  match read_line () with
  | "q" -> ANSITerminal.print_string [ ANSITerminal.green ] "Bye!\n\n"
  | _ ->
      move_to_next_player game;
      current_turn game

let first_player game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\
    \ Please give the first player a name: \n\
    \ Empty names will default to 'playern' where playern is the nth player: \n\n";
  match read_line () with
  | str -> (
      let new_plyr =
        create_player (if str = "" then "player1" else str) (get_start_pos game)
      in
      add_a_player game new_plyr;
      ANSITerminal.print_string [ ANSITerminal.green ]
        ("\nFirst player is: "
        ^ get_name (current_player game)
        ^ "\nPress the 'Enter' key to start");
      (*Prompt adding more players in MS2*)
      match read_line () with _ -> current_turn game)

let rec main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\nWelcome to our 3110 Group Project: Monopoly\n\nUsing standard board\n";

  let board = Standard_board.standard_board in
  let the_game = create_game board (create_players []) in
  first_player the_game

let () = main ()
