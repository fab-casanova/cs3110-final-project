open Property
open Player
open Game
open Cards

let win_message game =
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    ( "Congratulations " ^ current_player_name game
    ^ "!\n\n\
       You won a poorly-coded children's game.\n\n\
       Your parents must be proud :)\n\n\
       Bye!\n\n" )

let rec auction_helper highest_bidder prop bid_price player_list =
  match player_list with
  | [] -> ()
  | h :: t ->
      if bid_price > 0 && h == highest_bidder then (
        update_player_money h (-bid_price);
        add_property h prop;
        set_owner prop (get_name h);
        ANSITerminal.print_string [ ANSITerminal.green ]
          ("\n" ^ get_name h ^ " bought " ^ prop_name prop ^ " for ");
        ANSITerminal.print_string [ ANSITerminal.yellow ]
          ("$" ^ string_of_int bid_price ^ "\n\n");
        if can_have_houses prop then check_monopoly h prop )
      else if not (in_jail h) then (
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ( "\nCurrent highest bidder: " ^ get_name highest_bidder
          ^ "\nBid amount: " ^ string_of_int bid_price );
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ( "\n" ^ get_name h
          ^ ": enter your bid value, enter 0 if you would not like to bid\n" );
        let input = read_line () in
        let new_bid = try int_of_string input with _ -> -1 in
        if new_bid > bid_price && new_bid < player_money h then
          auction_helper h prop new_bid (t @ [ h ])
        else if new_bid <> 0 then (
          ANSITerminal.print_string [ ANSITerminal.red ]
            ( if new_bid <= bid_price && new_bid > 0 then
              "\n\
              \ Bid must be higher than current highest bid if you want to bid \
               on this property, please try again\n"
            else "\nInvalid bid, please try again\n" );
          auction_helper highest_bidder prop bid_price player_list )
        else auction_helper highest_bidder prop bid_price (t @ [ h ]) )
      else auction_helper highest_bidder prop bid_price (t @ [ h ])

let auction prop game =
  auction_helper (current_player game) prop 0 (get_players game)

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
    "? Type 'y' if yes, 'n' if no (note: typing 'n' will start an auction)\n";
  match read_line () with
  | "y" ->
      buy_property player pos;
      ANSITerminal.print_string [ ANSITerminal.green ]
        ("\n" ^ get_name player ^ " now owns " ^ prop_name pos ^ "\n");
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        (pp_remaining_properties game player pos)
  | "n" ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        ("\n" ^ prop_name pos ^ " was not bought\nTime to auction!");
      auction pos game (*TODO: prompt an auction if prop is not bought*)
  | _ ->
      print_endline "Please respond  with 'y' or 'n'";
      buy_prompt game player pos

let rec build_prompt player prop =
  ANSITerminal.print_string [ ANSITerminal.blue ] "You currently have ";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    ("$" ^ string_of_int (player_money player));
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ( ".\nYou have a monopoly on " ^ prop_space_type prop
    ^ " properties. You can build a house on " ^ prop_name prop ^ " for " );
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
  let len = List.length old_props in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ( string_of_int len ^ " propert"
    ^ (if len = 1 then "y" else "ies")
    ^ " left to auction\n" );
  match old_props with
  | [] -> ()
  | h :: t ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        ("Now auctioning: " ^ prop_name h);
      auction h game;
      auction_all_props t game

let mortgage_property player game =
  let props = mortgageable_props player in
  if List.length props > 0 then (
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ( "Mortgageable properties:\n" ^ pp_property_list props
      ^ "\nWhat property would you like to mortgage? Enter 'q' to exit\n" );
    let input = read_line () in
    if input = "q" then ()
    else if owns_property_of_name player input game then
      let prop = get_property_of_name input game in
      (*let can_mortgage = mortgage_allowed player prop in*)
      if mortgage_allowed player prop then (
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ( "Mortgaging " ^ prop_name prop ^ " will earn you $"
          ^ string_of_int (get_value prop / 2)
          ^ ".\n\
            \  Enter 'y' to go through with this action, anything else to \
             forgo this action.\n" );
        match read_line () with
        | "y" ->
            create_mortgage prop;
            update_player_money player (get_value prop)
        | _ -> () )
      else
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "\nCan't mortgage this property\n"
    else
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\nInvalid property name. Please enter a valid property\n" )
  else print_string "No properties to mortgage"

let unmortgage_property player game =
  let props = unmortgageable_props player in
  if List.length props > 0 then (
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ( "Properties that can be unmortgaged:\n" ^ pp_property_list props
      ^ "\nWhat property would you like to mortgage? Enter 'q' to exit\n" );
    let input = read_line () in
    if input = "q" then ()
    else if owns_property_of_name player input game then
      let prop = get_property_of_name input game in
      (*let can_mortgage = mortgage_allowed player prop in*)
      if List.mem prop props then (
        let unmort_cost = int_of_float (float_of_int (get_value prop) *. 1.1) in
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ( "Unmortgaging " ^ prop_name prop ^ " will cost you $"
          ^ string_of_int unmort_cost
          ^ ".\n\
            \  Enter 'y' to go through with this action, anything else to \
             forgo this action.\n" );
        match read_line () with
        | "y" ->
            unmortgage prop;
            update_player_money player (-unmort_cost)
        | _ -> () )
      else
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "\nCan't mortgage this property\n"
    else
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\nInvalid property name. Please enter a valid property\n" )
  else print_string "No properties to mortgage\n"

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
      ANSITerminal.print_string [ ANSITerminal.blue ]
        ( "Selling buildings on " ^ prop_name prop ^ " will earn you $"
        ^ string_of_int (house_cost prop / 2)
        ^ ".\n\
          \  Enter 'y' to go through with this action, anything else to forgo \
           this action.\n" );
      match read_line () with
      | "y" ->
          update_player_money player (house_cost prop / 2);
          downgrade_property prop
      | _ -> () )
    else
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "Cannot sell buildings on this property\n"

let transfer_properties player owner game =
  let props = transferable_props player in
  if List.length props > 0 then (
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ( "Transferrable props: " ^ pp_property_list props
      ^ "\nWhat property do you want to sell?\n" );
    let input = read_line () in
    if owns_property_of_name player input game then
      let prop = get_property_of_name input game in
      if player = owner && List.mem prop props then (
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ( "Returning " ^ prop_name prop ^ " to the bank will earn you $"
          ^ string_of_int (get_value prop)
          ^ ".\n\
            \  Enter 'y' to go through with this action, anything else to \
             forgo this action.\n" );
        match read_line () with
        | "y" ->
            update_player_money player (get_value prop);
            return_prop_to_bank player prop;
            remove_monopoly player prop
        | _ -> () )
      else if List.mem prop props then (
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ( "Transferring " ^ prop_name prop ^ " to " ^ get_name owner
          ^ " will earn you $"
          ^ string_of_int (get_value prop)
          ^ ".\n\
            \  Enter 'y' to go through with this action, anything else to \
             forgo this action.\n" );
        match read_line () with
        | "y" ->
            swap_owner player owner prop;
            update_player_money player (get_value prop);
            check_monopoly owner prop;
            remove_monopoly player prop
        | _ -> () )
      else print_string "Can't transfer this property\n"
    else print_string "Can't transfer this property\n" )
  else print_string "No properties to transfer"

let rec collect_nonmonetary_payment player receiver rent_owed game =
  (*MONEY REMAINING TO PAY: X MONEY ON HAND: Y*)
  (*TODO: FOR EVERY ACTION, ADD PROMPT SHOWING HOW MUCH MONEY WILL BE EARNED,
    ALONG WITH ASKING THE PLAYER IF THEY WANT TO GO THROUGH WITH IT

    DO THIS FOR EVERY ACTION*)
  print_assets player;
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("Player must pay $" ^ string_of_int rent_owed ^ "\n");
  if player_money player >= rent_owed then (
    ANSITerminal.print_string [ ANSITerminal.blue ] "\nEnough money to pay!\n";
    update_player_money player (-1 * rent_owed);
    if receiver <> player then update_player_money receiver rent_owed
    else add_to_pot game rent_owed )
  else (
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\nWould you like to mortgage, sell buildings, or transfer properties?\n";
    ANSITerminal.print_string [ ANSITerminal.blue ] "> ";
    match read_line () with
    (*| "pay with cash" | "cash" -> if not (out_of_cash rent_owed player) then (
      update_player_money player (-1 * rent_owed); if receiver <> player then
      update_player_money receiver rent_owed else add_to_pot game rent_owed)
      else ( ANSITerminal.print_string [ ANSITerminal.blue ] "\nInvalid choice,
      not enough money.\n"); collect_nonmonetary_payment player receiver
      rent_owed game*)
    | "mortgage properties" | "mortgage" ->
        mortgage_property player game;
        collect_nonmonetary_payment player receiver rent_owed game
    | "sell buildings" | "sell" ->
        sell_buildings player receiver game;
        collect_nonmonetary_payment player receiver rent_owed game
    | "transfer properties" | "transfer" ->
        transfer_properties player receiver game;
        collect_nonmonetary_payment player receiver rent_owed game
    | _ ->
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "\nInvalid input, please try again\n";
        collect_nonmonetary_payment player receiver rent_owed game )

let check_status player pos dues game =
  match player_status dues player with
  | 0 -> collect_dues player pos dues game
  | 1 ->
      let owner = if is_owned pos then get_owner pos game else player in
      collect_nonmonetary_payment player owner dues game
  | _ ->
      let owned = is_owned pos in
      let old_props = get_properties player in
      bankruptcy player pos game;
      if not (last_one_standing game) then (
        if not owned then auction_all_props old_props game )
      else win_message game

let rec current_property_effects game player =
  let pos = get_position player in
  print_endline ("Position " ^ string_of_int (get_index game pos) ^ " of 40");
  if not (is_go_to_jail pos) then (
    if is_free_parking pos && pot_amount game > 0 then (
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        ( get_name player ^ " will receive the "
        ^ string_of_int (pot_amount game)
        ^ " in the free parking money pot!\n" );
      cash_out_pot game player );
    if is_com_or_chance pos then (
      print_endline ("Draw a " ^ prop_space_type pos ^ " card!");
      draw_card game );
    if is_tax pos then (
      print_endline "Tax will be collected and added to the pot";
      let dues = calculate_dues pos game in
      check_status player pos dues game )
    else if is_owned pos then (
      print_endline (prop_name pos ^ " is owned");
      let owner = get_owner pos game in
      print_endline ("The owner of " ^ prop_name pos ^ " is " ^ get_name owner);
      if owner = player then print_endline "This is your property"
      else
        let dues = calculate_dues pos game in
        print_endline
          ( get_name player ^ " must pay $" ^ string_of_int dues ^ " to "
          ^ get_name owner );
        check_status player pos dues game )
    else if can_be_purchased pos then (
      print_endline
        ( prop_name pos ^ " can be purchased for $"
        ^ string_of_int (purchase_price pos)
        ^ "\n" );
      if player_money player >= purchase_price pos then
        buy_prompt game player pos
      else
        ANSITerminal.print_string [ ANSITerminal.red ]
          "Not enough funds to purchase\n\n" ) )
  else (
    ANSITerminal.print_string [ ANSITerminal.red ] "You're going to jail\n";
    put_in_jail player;
    change_pos player (get_jail game);
    ANSITerminal.print_string [ ANSITerminal.red ]
      ( string_of_int (time_left player)
      ^ " turn(s) left in jail for " ^ get_name player ^ "\n" ) )

and draw_card game =
  let player = current_player game in
  let before_pos = get_position player in
  let card_type = pp_space_type (get_type (get_position player)) in
  let get_nonempty_deck card_type game =
    let deck = get_deck card_type game in
    if List.length deck > 0 then deck else shuffle_deck ()
  in
  let deck = get_nonempty_deck card_type game in
  let jail_in_play card_type =
    let players = get_players game in
    List.exists (fun x -> owns_jail_free_card card_type x) players
  in
  let rec first_card deck =
    match deck with
    | [] -> first_card (get_nonempty_deck card_type game)
    | h :: t ->
        set_deck t card_type game;
        let temp_card = get_card card_type h in
        if
          card_text temp_card = "Get Out of Jail Free" && jail_in_play card_type
        then first_card t
        else temp_card
  in
  let card = first_card deck in
  let rec print_deck deck acc =
    match deck with
    | [] -> acc
    | h :: t -> print_deck t (acc ^ " " ^ string_of_int h)
  in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("Your card: " ^ card_text card ^ "\n");

  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("Current deck: " ^ print_deck deck "" ^ "\n");

  let transactions = card_effect card game in
  let is_none giver_receiver =
    match giver_receiver with None -> true | giver_receiver -> false
  in
  let perform_payment giver amount receiver =
    match player_status amount player with
    | 0 ->
        update_player_money (Option.get giver) (-1 * amount);
        update_player_money (Option.get receiver) amount
    | 1 -> collect_nonmonetary_payment player (Option.get receiver) amount game
    | _ ->
        let old_props = get_properties (Option.get giver) in
        if not (is_none receiver) then (
          update_player_money (Option.get receiver)
            (player_money (Option.get giver));
          hand_over_all_properties (Option.get giver) (Option.get receiver) )
        else forfeit player game;
        if not (last_one_standing game) then auction_all_props old_props game
        else win_message game
  in

  let rec finish_transactions transactions =
    match transactions with
    | [] -> ()
    | (giver, amount, receiver) :: t ->
        let giver_is_none = is_none giver in
        let receiver_is_none = is_none receiver in
        if (not giver_is_none) && not receiver_is_none then (
          perform_payment giver amount receiver;
          finish_transactions t )
        else if giver_is_none then (
          update_player_money (Option.get receiver) amount;
          finish_transactions t )
        else if receiver_is_none then (
          update_player_money (Option.get giver) (-amount);
          add_to_pot game amount )
  in
  if before_pos <> get_position player then current_property_effects game player;
  finish_transactions transactions

let rec play_a_turn game =
  let player = current_player game in
  let old_doubles = num_doubles player in
  move_player player game 0 false;
  if num_doubles player >= 3 then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Rolled 3 doubles. Going to jail. Serve 3 turns";
    change_pos player (get_jail game);
    reset_doubles player )
  else
    let pos = get_position player in
    print_endline
      ( get_name player ^ " is at: " ^ prop_name pos ^ " ("
      ^ prop_space_type pos ^ ")" );
    current_property_effects game player;
    if still_in_game player game then (
      print_assets player;
      if num_doubles player > old_doubles && not (in_jail player) then (
        ( match num_doubles player with
        | 1 ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "Rolled a double. Go again"
        | _ ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "Rolled a second double. Go again\n" );
        play_a_turn game ) )

let attempt_escape player game =
  let roll = roll_dice () in
  if fst roll = snd roll then (
    let sum = sum_dice roll in
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      ( "Rolled doubles of "
      ^ string_of_int (fst roll)
      ^ "! Escaped!\nMoving " ^ string_of_int sum ^ " spaces\n" );
    un_jail player;
    move_player player game sum true;
    current_property_effects game player )
  else
    ANSITerminal.print_string [ ANSITerminal.yellow ] "Did not roll doubles!\n"

let rec jail_prompt player game =
  if
    num_jail_free_cards player > 0
    || (player_money player >= 50 && time_left player > 1)
  then (
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ( "Would you like to "
      ^ ( if player_money player >= 50 && time_left player > 1 then
          "pay $50 (pay) or "
        else "" )
      ^ ( if num_jail_free_cards player > 0 then
          "use your Get Out of Jail card (use) or "
        else "" )
      ^ "attempt to roll doubles (roll) to escape jail? Type 'no' to remain in \
         jail\n" );
    match read_line () with
    | "pay" ->
        if player_money player < 50 then (
          ANSITerminal.print_string [ ANSITerminal.red ] "\nNot enough money\n";
          jail_prompt player game )
        else if time_left player <= 1 then (
          ANSITerminal.print_string [ ANSITerminal.red ]
            "\nCannot pay to get out in your last turn of jail\n";
          jail_prompt player game )
        else (
          update_player_money player (-50);
          un_jail player )
    | "y" | "roll" -> attempt_escape player game
    | "remain" | "n" | "no" -> ()
    | "use" | "use card" ->
        if num_jail_free_cards player > 0 then (
          remove_jail_free_card player;
          un_jail player )
        else (
          ANSITerminal.print_string [ ANSITerminal.red ]
            "\nDoesn't own get out of jail free card\n";
          jail_prompt player game )
    | _ -> jail_prompt player game )
  else (
    ANSITerminal.print_string [ ANSITerminal.red ] "\n\n";
    attempt_escape player game )

let rec start_deal player asked_player game =
  print_assets player;
  print_endline "";
  print_assets asked_player;
  print_endline ""

let rec deal_prompt player game =
  ();
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (pp_other_players player game ^ "\n");
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Which player would you like to make a deal with?\nEnter '' to go back\n";
  let s = read_line () in
  if String.length s > 0 then (
    try
      let other_player = find_player s game in
      if player <> other_player then (
        ANSITerminal.print_string [ ANSITerminal.yellow ]
          ("Attempting a deal with " ^ get_name other_player ^ "\n");
        start_deal player other_player game;
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "\nMake more deals? 'y' for yes, anything else for no\n";
        match read_line () with
        | "y" | "yes" -> deal_prompt player game
        | _ -> () )
      else (
        ANSITerminal.print_string [ ANSITerminal.red ]
          "Cannot make a deal with yourself, choose another player\n";
        deal_prompt player game )
    with _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Invalid player name, please enter one of the other player names\n";
      deal_prompt player game )

(* responses from other player: yes, no negotiate*)

let rec end_of_turn player game =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Enter\n\
     's' to print the remaining players' current stats,\n\
     'b' to check if houses can be built on any properties,\n\
     'm' to mortgage a property,\n\
     'u' to unmortgage a property,\n\
     \'d' to make deals with other players,\n\
     anything else to move on.\n";
  match read_line () with
  | "s" ->
      print_game_status game;
      end_of_turn player game
  | "b" ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        ( "Checking which of " ^ get_name player
        ^ "'s properties can be built on.\n\
           note: one house can be built on each elligible property per 'b'.\n\
           If you would like to build multiple houses, continue inputting 'b' \n\
           to evenly build houses until no prompts to build show up.\n\
           If no prompts show up, you cannot currently build houses\n\n" );
      check_build player (get_properties player);
      end_of_turn player game
  | "m" ->
      mortgage_property player game;
      end_of_turn player game
  | "u" ->
      unmortgage_property player game;
      end_of_turn player game
  | "d" ->
      deal_prompt player game;
      end_of_turn player game
  | _ -> ()

(*current_turn game*)

let rec current_turn game =
  if not (last_one_standing game) then (
    ANSITerminal.print_string [ ANSITerminal.green ]
      ("\nCurrent player: " ^ get_name (current_player game));
    let curr = current_player game in
    if not (in_jail curr) then
      (* print_endline " NOT IN JAIL";*)
      play_a_turn game
    else (
      ANSITerminal.print_string [ ANSITerminal.red ]
        ("\n" ^ get_name curr ^ " is in jail\n");
      (* (let pos = get_position curr in print_endline (get_name curr ^ " is at:
         " ^ prop_name pos ^ " (" ^ prop_space_type pos ^ ")")); *)
      jail_prompt curr game;
      if in_jail curr then (
        served_a_turn curr;
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ( string_of_int (time_left curr)
          ^ " turn(s) left in jail for " ^ get_name curr ^ "\n" ) ) );
    end_of_turn curr game;
    ANSITerminal.print_string [ ANSITerminal.green ]
      ( "\nContinue playing? Enter 'f' to forfeit, 'q' to quit the entire game\n"
      ^ "anything else to start the next turn.\n" );
    match read_line () with
    | "f" ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          ( get_name curr ^ " has forfeited. \n Removing " ^ get_name curr
          ^ "\n\n" );
        let old_props = get_properties curr in
        forfeit curr game;
        if not (last_one_standing game) then (
          auction_all_props old_props game;
          ANSITerminal.print_string [ ANSITerminal.blue ]
            (pp_players game ^ "\n\n");
          current_turn game )
        else win_message game
    | "q" -> ANSITerminal.print_string [ ANSITerminal.green ] "Bye!\n\n"
    | _ ->
        move_to_next_player game;
        current_turn game )
  else win_message game

let rec addl_player game =
  let old_num = num_players game in
  let default_name = "player" ^ string_of_int (old_num + 1) in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ( "\n Please give " ^ default_name
    ^ " a name \n Entering nothing will default to " ^ default_name ^ "\n" );
  let str = read_line () in
  let new_plyr =
    create_player (if str = "" then default_name else str) (get_start_pos game)
  in
  add_a_player game new_plyr;
  if old_num = num_players game then
    ANSITerminal.print_string [ ANSITerminal.red ] "\nName was already taken\n"
  else
    ANSITerminal.print_string [ ANSITerminal.green ]
      ("\nAdded " ^ get_name new_plyr ^ " to game!\n");

  if num_players game = 1 then (
    ANSITerminal.print_string [ ANSITerminal.green ] "Please add another player";
    addl_player game )
  else if num_players game < 4 then (
    ANSITerminal.print_string [ ANSITerminal.green ]
      ( pp_players game
      ^ "\n\
         Enter 'add' to add another player, enter anything else to start the \
         game\n" );
    match read_line () with "add" -> addl_player game | _ -> current_turn game )
  else (
    ANSITerminal.print_string [ ANSITerminal.green ]
      "Max number of players reached, starting game...\n";
    current_turn game )

let first_player game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\
    \ Please give the first player a name \n\
    \ Entering nothing will default to player1 \n\n";
  let str = read_line () in
  let new_plyr =
    create_player (if str = "" then "player1" else str) (get_start_pos game)
  in
  add_a_player game new_plyr;
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("\nFirst player is: " ^ get_name (current_player game));
  addl_player game

let rec main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\nWelcome to our 3110 Group Project: Monopoly\n\nUsing standard board\n";

  let board = Standard_board.standard_board in
  let the_game = create_game board in
  first_player the_game

let () = main ()
