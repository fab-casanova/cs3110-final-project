open Property
open Player
open Game
open Cards

let win_message game =
  let curr = current_player game in
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    ( if is_real_player curr then
      "Congratulations " ^ get_name curr
      ^ "!\n\n\
         You won a poorly-coded children's game.\n\n\
         Your parents must be proud :)\n\n\
         Bye!\n\n"
    else
      "The winner of the game is AI " ^ get_name curr
      ^ ".\nYou lost to a computer, sad.\n" )

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
          "\nCurrent highest bidder: ";
        ANSITerminal.print_string [ ANSITerminal.cyan ]
          (get_name highest_bidder ^ "\n");
        ANSITerminal.print_string [ ANSITerminal.blue ] "Bid amount: ";
        ANSITerminal.print_string [ ANSITerminal.yellow ]
          ("$" ^ string_of_int bid_price ^ "\n");
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ( get_name h
          ^ ": enter your bid value, enter 0 if you would not like to bid\n" );

        let new_bid =
          if is_real_player h then
            let input = read_line () in
            try int_of_string input with _ -> -1
          else if player_money h >= bid_price + 50 && landed_heads () then
            bid_price + 50
          else 0
        in
        if new_bid > bid_price && new_bid <= player_money h then (
          if not (is_real_player h) then (
            ANSITerminal.print_string [ ANSITerminal.white ]
              ("AI " ^ get_name h ^ " placed a bid of ");
            ANSITerminal.print_string [ ANSITerminal.yellow ]
              ("$" ^ string_of_int new_bid ^ "\n") );
          auction_helper h prop new_bid (t @ [ h ]) )
        else if new_bid <> 0 then (
          ANSITerminal.print_string [ ANSITerminal.red ]
            ( if new_bid <= bid_price && new_bid > 0 then
              "\n\
              \ Bid must be higher than current highest bid if you want to bid \
               on this property, please try again\n"
            else "\nInvalid bid, please try again\n" );
          auction_helper highest_bidder prop bid_price player_list )
        else (
          if not (is_real_player h) then
            ANSITerminal.print_string [ ANSITerminal.red ]
              ("AI " ^ get_name h ^ " did not place a bid\n");
          auction_helper highest_bidder prop bid_price (t @ [ h ]) ) )
      else auction_helper highest_bidder prop bid_price (t @ [ h ])

let auction prop game =
  auction_helper (current_player game) prop 0 (get_players game)

let rec buy_prompt game player pos =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (get_name player ^ ": you currently have ");
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    ("$" ^ string_of_int (player_money player));
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (".\nWould you like to purchase " ^ prop_name pos ^ " for ");
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    ("$" ^ string_of_int (purchase_price pos));
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "? Type 'y' if yes, 'n' if no (note: typing 'n' will start an auction)\n";
  let input =
    if is_real_player player then read_line ()
    else if landed_heads () then "y"
    else "n"
  in
  match input with
  | "y" ->
      buy_property player pos;
      ANSITerminal.print_string [ ANSITerminal.green ]
        ("\n" ^ get_name player ^ " now owns " ^ prop_name pos ^ "\n");
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        (pp_remaining_properties game player pos)
  | "n" -> (
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        ( "\n" ^ prop_name pos
        ^ " was not bought\nTime to auction! Press Enter to begin\n" );
      match read_line () with _ -> auction pos game )
  | _ ->
      print_endline "Please respond  with 'y' or 'n'";
      buy_prompt game player pos

let rec build_prompt player prop =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("You can build a house on " ^ prop_name prop ^ " for ");
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    ("$" ^ string_of_int (house_cost prop));
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ".\nType 'y' to build a house here, and 'n' to pass.\n";
  let input =
    if is_real_player player then read_line ()
    else if landed_heads () then "y"
    else "n"
  in
  match input with
  | "y" ->
      upgrade_property prop;
      update_player_money player (-1 * house_cost prop);
      ANSITerminal.print_string [ ANSITerminal.green ]
        (prop_name prop ^ " is now at Level " ^ what_stage prop ^ "\n")
  | "n" -> print_endline ("\n" ^ prop_name prop ^ " did not have a house built")
  | _ ->
      print_endline "Please respond with 'y' or 'n'";
      build_prompt player prop

let rec check_build player game props =
  if List.length props = 0 then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      "No more houses can be built at this time\n";
    match read_line () with _ -> () )
  else (
    ANSITerminal.print_string [ ANSITerminal.blue ]
      (get_name player ^ ": you currently have ");
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      ("$" ^ string_of_int (player_money player));
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ".\nProperties where houses can be built: ";
    ANSITerminal.print_string [ ANSITerminal.cyan ]
      (pp_property_list props ^ "\n");
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ( get_name player
      ^ ": what property would you like to build houses on? Enter '' to exit\n"
      );
    let input = read_line () in
    match input with
    | "" -> print_newline ()
    | _ ->
        if owns_property_of_name player input game then
          let prop = get_property_of_name input game in
          if List.mem prop props then (
            build_prompt player prop;
            check_build player game (buildable_props player) )
          else (
            ANSITerminal.print_string [ ANSITerminal.red ]
              ("You cannot build a property on " ^ input ^ " at this time.\n");
            check_build player game props )
        else (
          ANSITerminal.print_string [ ANSITerminal.red ]
            ("Player owns no property named " ^ input ^ "\n");
          check_build player game props ) )

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

let end_career player pos game =
  let owned = is_owned pos in
  let old_props = get_properties player in
  bankruptcy player pos game;
  if not (last_one_standing game) then (
    if not owned then auction_all_props old_props game )
  else win_message game

let rec mortgage_property player game is_end_of_turn =
  let props = mortgageable_props player in
  if List.length props > 0 then (
    ANSITerminal.print_string [ ANSITerminal.blue ] "Mortgageable properties: ";
    ANSITerminal.print_string [ ANSITerminal.cyan ]
      (pp_property_list props ^ "\n");
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ( get_name player
      ^ ": what property would you like to mortgage? Enter '' to exit\n" );
    let input =
      if is_real_player player then read_line ()
      else if landed_heads () then prop_name (List.hd props)
      else ""
    in
    if input = "" then ()
    else if owns_property_of_name player input game then
      let prop = get_property_of_name input game in
      if mortgage_allowed player prop then (
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ( get_name player ^ ": mortgaging " ^ prop_name prop
          ^ " will earn you " );
        ANSITerminal.print_string [ ANSITerminal.yellow ]
          ("$" ^ string_of_int (get_value prop / 2));
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ".\n\
          \  Enter 'y' to go through with this action, anything else to forgo \
           this action.\n";
        match read_line () with
        | "y" ->
            ANSITerminal.print_string [ ANSITerminal.blue ]
              (prop_name prop ^ " has been mortgaged\n");
            create_mortgage prop;
            update_player_money player (get_value prop);
            if is_end_of_turn then mortgage_property player game true
        | _ -> mortgage_property player game is_end_of_turn )
      else (
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "\nCan't mortgage this property\n";
        mortgage_property player game is_end_of_turn )
    else (
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\nInvalid property name. Please enter a valid property\n";
      mortgage_property player game is_end_of_turn ) )
  else
    ANSITerminal.print_string [ ANSITerminal.red ] "No properties to mortgage\n"

let rec unmortgage_property player game is_end_of_turn =
  let props = unmortgageable_props player in
  if List.length props > 0 then (
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "Properties that can be unmortgaged: ";
    ANSITerminal.print_string [ ANSITerminal.cyan ]
      (pp_property_list props ^ "\n");
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ( get_name player
      ^ ": what property would you like to unmortgage? Enter 'q' to exit\n" );

    let input =
      if is_real_player player then read_line ()
      else if landed_heads () then prop_name (List.hd props)
      else "q"
    in
    if input = "q" then ()
    else if owns_property_of_name player input game then
      let prop = get_property_of_name input game in
      (*let can_mortgage = mortgage_allowed player prop in*)
      if List.mem prop props then (
        let unmort_cost = int_of_float (float_of_int (get_value prop) *. 1.1) in
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ( get_name player ^ ": unmortgaging " ^ prop_name prop
          ^ " will cost you " );
        ANSITerminal.print_string [ ANSITerminal.yellow ]
          ("$" ^ string_of_int unmort_cost);
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ".\n\
          \  Enter 'y' to go through with this action, anything else to forgo \
           this action.\n";
        match read_line () with
        | "y" ->
            unmortgage prop;
            update_player_money player (-unmort_cost);
            if is_end_of_turn then unmortgage_property player game true
        | _ -> unmortgage_property player game is_end_of_turn )
      else
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "\nCan't unmortgage this property\n"
    else
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\nInvalid property name. Please enter a valid property\n" )
  else print_string "No properties to unmortgage\n"

let rec sell_buildings player game is_end_of_turn =
  (*TELEPORT*)
  let props = sellable_bldg_props player in
  if List.length props > 0 then (
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "Properties with houses to sell: ";
    ANSITerminal.print_string [ ANSITerminal.cyan ] (pp_property_list props);
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ( "\n" ^ get_name player
      ^ ": what property do you want to sell buildings on? Enter 'q' to quit\n"
      );
    let input =
      if is_real_player player then read_line ()
      else if landed_heads () then prop_name (List.hd props)
      else "q"
    in
    if input = "q" then ()
    else if owns_property_of_name player input game then (
      let prop = get_property_of_name input game in
      if List.mem prop props then (
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ("Selling a building on " ^ prop_name prop ^ " will earn you ");
        ANSITerminal.print_string [ ANSITerminal.yellow ]
          ("$" ^ string_of_int (house_cost prop / 2));
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ".\n\
           Enter 'y' to go through with this action, anything else to forgo \
           this action.\n";
        match read_line () with
        | "y" ->
            update_player_money player (house_cost prop / 2);
            downgrade_property prop;
            if is_end_of_turn then sell_buildings player game true
        | _ -> sell_buildings player game is_end_of_turn )
      else
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "Cannot sell buildings from given input\n";
      if is_end_of_turn then sell_buildings player game true ) )
  else
    ANSITerminal.print_string [ ANSITerminal.red ]
      "No properties to sell buildings from\n"

let transfer_properties player owner game =
  let props = transferable_props player in
  if List.length props > 0 then (
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ( "Transferrable props: " ^ pp_property_list props ^ "\n"
      ^ get_name player ^ ": what property do you want to sell?\n" );
    let input =
      if is_real_player player then read_line ()
      else if landed_heads () then prop_name (List.hd props)
      else "q"
    in
    if owns_property_of_name player input game then
      let prop = get_property_of_name input game in
      if player = owner && List.mem prop props then (
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ( "Returning " ^ prop_name prop ^ " to the bank will earn you $"
          ^ string_of_int (get_value prop)
          ^ ".\n\
            \  Enter 'y' to go through with this action, anything else to \
             forgo this action.\n" );
        match if is_real_player player then read_line () else "y" with
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
        match if is_real_player player then read_line () else "y" with
        | "y" ->
            swap_owner player owner prop;
            update_player_money player (get_value prop);
            check_monopoly owner prop;
            remove_monopoly player prop
        | _ -> () )
      else print_string "Can't transfer this property\n"
    else print_string "Can't transfer this property\n" )
  else print_string "No properties to transfer"

let ai_collect_action () =
  match three_sided_die () with
  | 0 -> "mortgage"
  | 1 -> "sell"
  | _ -> "transfer"

let rec collect_nonmonetary_payment player receiver rent_owed game limit =
  print_assets player;
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("Player must pay $" ^ string_of_int rent_owed ^ "\n");
  if player_money player >= rent_owed then (
    ANSITerminal.print_string [ ANSITerminal.blue ] "\nEnough money to pay!\n";
    update_player_money player (-1 * rent_owed);
    if receiver <> player then update_player_money receiver rent_owed
    else add_to_pot game rent_owed )
  else if limit >= 50 && not (is_real_player player) then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      ( "AI " ^ get_name player
      ^ " has run out of turns and has automatically gone bankrupt \n" );
    hand_over_all_properties player receiver;
    bankruptcy player (get_position player) game;
    if last_one_standing game then win_message game )
  else (
    if not (is_real_player player) then
      ANSITerminal.print_string [ ANSITerminal.blue ]
        (string_of_int (50 - limit) ^ " turn(s) left for the AI\n");
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\nWould you like to mortgage, sell buildings, or transfer properties?\n";
    ANSITerminal.print_string [ ANSITerminal.blue ] "> ";
    match
      if is_real_player player then read_line () else ai_collect_action ()
    with
    | "mortgage properties" | "mortgage" ->
        mortgage_property player game false;
        collect_nonmonetary_payment player receiver rent_owed game (limit + 1)
    | "sell buildings" | "sell" ->
        sell_buildings player game false;
        collect_nonmonetary_payment player receiver rent_owed game (limit + 1)
    | "transfer properties" | "transfer" ->
        transfer_properties player receiver game;
        collect_nonmonetary_payment player receiver rent_owed game (limit + 1)
    | _ ->
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "\nInvalid input, please try again\n";
        collect_nonmonetary_payment player receiver rent_owed game (limit + 1) )

let end_career player pos dues game =
  let owned =
    is_owned pos
    && dues = calculate_dues pos game
    && get_owner pos game <> player
  in
  let old_props = get_properties player in
  bankruptcy player pos game;
  if not (last_one_standing game) then (
    if not owned then auction_all_props old_props game )
  else win_message game

let check_status player pos dues game =
  match player_status dues player with
  | 0 -> collect_dues player pos dues game
  | 1 ->
      let owner = if is_owned pos then get_owner pos game else player in
      collect_nonmonetary_payment player owner dues game 0
  | _ -> end_career player pos dues game

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
    | 1 ->
        collect_nonmonetary_payment player (Option.get receiver) amount game 0
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
      "Rolled 3 doubles. Going to jail. Serve 3 turns\n";
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
              "Rolled a double. Press Enter to go again\n"
        | _ ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "Rolled a second double. Press Enter to go again\n" );
        (*if is_real_player player then*)
        match read_line () with _ -> play_a_turn game ) )

let attempt_escape player game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Rolling dice to attempt an escape...\n";
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

let ai_jail player =
  if num_jail_free_cards player > 0 then "use"
  else if landed_heads () && player_money player >= 50 && time_left player > 1
  then "pay"
  else "roll"

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
    match if is_real_player player then read_line () else ai_jail player with
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
    | "use" | "use card" ->
        if num_jail_free_cards player > 0 then (
          remove_jail_free_card player;
          un_jail player )
        else (
          ANSITerminal.print_string [ ANSITerminal.red ]
            "\nDoesn't own get out of jail free card\n";
          jail_prompt player game )
    | "remain" | "n" | "no" -> ()
    | _ -> jail_prompt player game )
  else (
    ANSITerminal.print_string [ ANSITerminal.red ] "\n\n";
    attempt_escape player game )

let ai_barter ai buyer prop =
  if ai = buyer then
    string_of_int (min (purchase_price prop - 100) (player_money buyer))
  else string_of_int (min (purchase_price prop + 100) (player_money buyer))

let ai_respond () =
  match three_sided_die () with 0 -> "y" | 1 -> "b" | _ -> "n"

let rec propose_price player asked_player buyer seller prop =
  let buyer_max = player_money buyer in
  if is_real_player player then
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ( get_name player ^ ": how much would you like to "
      ^ (if player = buyer then "buy " else "sell ")
      ^ prop_name prop ^ " for?\nPlease enter a nonnegative number (at most $"
      ^ string_of_int buyer_max ^ "), enter 'n' if you want to cancel\n" );
  let input =
    if is_real_player player then read_line () else ai_barter player buyer prop
  in

  match input with
  | "n" -> ()
  | _ -> (
      try
        let desired = int_of_string input in
        if desired < 0 then (
          ANSITerminal.print_string [ ANSITerminal.red ]
            "Please only enter a nonnegative number or 'n'\n";
          propose_price player asked_player buyer seller prop )
        else if desired > buyer_max then (
          ANSITerminal.print_string [ ANSITerminal.red ]
            ( get_name buyer ^ " cannot pay anything greater than $"
            ^ string_of_int buyer_max ^ "\n" );
          propose_price player asked_player buyer seller prop )
        else (
          if is_real_player player then (
            ANSITerminal.print_string [ ANSITerminal.blue ]
              ( get_name player ^ ": would you like to propose "
              ^ (if player = buyer then "buying " else "selling ")
              ^ prop_name prop ^ " for " );
            ANSITerminal.print_string [ ANSITerminal.yellow ] ("$" ^ input);
            ANSITerminal.print_string [ ANSITerminal.blue ]
              "?\nEnter 'y' to confirm, anything else to choose another price\n"
            );
          match if is_real_player player then read_line () else "y" with
          | "y" -> barter_respond player asked_player buyer seller prop desired
          | _ -> propose_price player asked_player buyer seller prop )
      with _ ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          "Please only enter a nonnegative number or 'n'\n";
        propose_price player asked_player buyer seller prop )

and barter_respond player asked_player buyer seller prop price =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ( get_name asked_player ^ ": would you like to "
    ^ (if player = buyer then "sell " else "buy ")
    ^ prop_name prop
    ^ (if player = buyer then " to " else " from ")
    ^ get_name player ^ " for " );
  ANSITerminal.print_string [ ANSITerminal.yellow ] ("$" ^ string_of_int price);
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "?\n\
     Enter 'y' to accept, 'b' to barter and propose a new price, or 'n' to \
     reject the deal\n";
  match if is_real_player asked_player then read_line () else ai_respond () with
  | "y" ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        ( get_name player ^ " has "
        ^ (if player = buyer then "bought " else "sold ")
        ^ prop_name prop
        ^ (if player = buyer then " from " else " to ")
        ^ get_name asked_player ^ " for " );
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        ("$" ^ string_of_int price);
      swap_owner seller buyer prop;
      update_player_money buyer (-1 * price);
      update_player_money seller price;
      check_monopoly buyer prop;
      remove_monopoly seller prop (* TELEPORT HERE*)
  | "b" ->
      if not (is_real_player asked_player) then
        ANSITerminal.print_string [ ANSITerminal.magenta ]
          ("AI " ^ get_name asked_player ^ " has decided to barter\n");
      propose_price asked_player player buyer seller prop
  | "n" ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        ( get_name asked_player ^ " has rejected the deal on " ^ prop_name prop
        ^ "\n" )
  | _ -> barter_respond player asked_player buyer seller prop price

let rec propose_deal player asked_player game buyer seller props =
  if List.length props > 0 then (
    let action = if player = buyer then "buy" else "sell" in
    ANSITerminal.print_string [ ANSITerminal.blue ]
      (get_name seller ^ "'s transferable properties: ");
    ANSITerminal.print_string [ ANSITerminal.cyan ] (pp_property_list props);
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ( "\n" ^ get_name player
      ^ ": which property from this list would you like to " ^ action
      ^ "?\nEnter '' to go back\n" );
    let input =
      if is_real_player player then read_line ()
      else prop_name (random_property seller)
    in
    match input with
    | "" -> ()
    | _ ->
        if owns_property_of_name seller input game then
          let prop = get_property_of_name input game in
          if can_transfer seller prop then (
            if not (is_real_player player) then
              ANSITerminal.print_string [ ANSITerminal.cyan ]
                ( "AI " ^ get_name player ^ " would like to " ^ action ^ " "
                ^ input ^ "\n" );
            propose_price player asked_player buyer seller prop )
          else (
            ANSITerminal.print_string [ ANSITerminal.red ]
              (input ^ " cannot be transfered\n");
            propose_deal player asked_player game buyer seller props )
        else (
          ANSITerminal.print_string [ ANSITerminal.red ]
            (get_name seller ^ " does not own a property named " ^ input ^ "\n");
          propose_deal player asked_player game buyer seller props ) )
  else
    ANSITerminal.print_string [ ANSITerminal.red ]
      (get_name asked_player ^ " does not have any transferable properties")

let ai_deal cond =
  match cond with
  | true, true -> (
      match three_sided_die () with 0 -> "b" | 1 -> "s" | _ -> "" )
  | true, _ -> if landed_heads () then "b" else ""
  | _ -> if landed_heads () then "s" else ""

let rec start_deal player asked_player game =
  let tprops1 = transferable_props player in
  let tprops2 = transferable_props asked_player in
  let props1exist = List.length tprops1 > 0 in
  let props2exist = List.length tprops2 > 0 in
  let cond = (props2exist, props1exist) in
  if props1exist || props2exist then (
    let msg =
      match cond with
      | true, true -> "buy ('b') a property from or sell ('s') a property to "
      | true, _ -> "buy ('b') a property from "
      | _ -> "sell ('s') a property to "
    in
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ( get_name player ^ ": would you like to " ^ msg ^ get_name asked_player
      ^ "?\nEnter '' to go back\n" );
    let input = if is_real_player player then read_line () else ai_deal cond in
    match input with
    | "b" -> propose_deal player asked_player game player asked_player tprops2
    | "s" -> propose_deal player asked_player game asked_player player tprops1
    | "" -> ()
    | _ ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          "Please enter one of the given inputs depending on what you want to do\n";
        start_deal player asked_player game )
  else
    ANSITerminal.print_string [ ANSITerminal.red ]
      "A deal cannot be struck because neither side has any transferable \
       properties\n"

let rec deal_prompt player game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (pp_other_players player game ^ "\n");
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ( get_name player
    ^ ": which player would you like to make a deal with?\n\
       Enter '' to go back\n" );
  let s = if is_real_player player then read_line () else "random_player" in
  if String.length s > 0 then (
    try
      let other_player =
        if is_real_player player then find_player s game
        else random_other_player player game
      in
      if player <> other_player then (
        ANSITerminal.print_string [ ANSITerminal.yellow ]
          ("Attempting a deal with " ^ get_name other_player ^ "\n");
        print_assets player;
        print_endline "";
        print_assets other_player;
        print_endline "";
        start_deal player other_player game;
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ( "\n" ^ get_name player
          ^ ": make more deals? 'y' for yes, anything else for no\n" );
        match
          if is_real_player player then read_line ()
          else if three_sided_die () = 2 then "y"
          else ""
        with
        | "y" | "yes" ->
            if not (is_real_player player) then
              ANSITerminal.print_string [ ANSITerminal.blue ]
                "AI is attempting another deal\n";
            deal_prompt player game
        | _ ->
            if not (is_real_player player) then
              ANSITerminal.print_string [ ANSITerminal.red ]
                "AI is not attempting another deal\n" )
      else (
        ANSITerminal.print_string [ ANSITerminal.red ]
          "Cannot make a deal with yourself, choose another player\n";
        deal_prompt player game )
    with _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Invalid player name, please enter one of the other player names\n";
      deal_prompt player game )

let ai_end_of_turn lim =
  if lim >= 10 then "end"
  else (
    Random.self_init ();
    match Random.int 12 with
    | 0 | 1 -> "build"
    | 2 -> "sell"
    | 3 | 4 -> "mortgage"
    | 5 -> "unmortgage"
    | 6 | 7 | 8 -> "deal"
    | _ -> "end" )

(* MODIFY END OF TURN FOR AI *)
let rec end_of_turn player game lim =
  if is_real_player player then
    ANSITerminal.print_string [ ANSITerminal.green ]
      ( get_name player
      ^ ": enter\n\
         'stats' to print the remaining players' current stats,\n\
         'build' to check if houses can be built on any properties,\n\
         'sell' to sell houses from your properties,\n\
         'mortgage' to mortgage a property,\n\
         'unmortgage' to unmortgage a property,\n\
         'deal' to make deals with other players,\n\
         'end' to move on.\n" )
  else (
    ANSITerminal.print_string [ ANSITerminal.green ]
      ( "AI " ^ get_name player ^ " has "
      ^ string_of_int (10 - lim)
      ^ " actions left\nPress Enter to let AI perform action\n" );
    match read_line () with _ -> () );
  match if is_real_player player then read_line () else ai_end_of_turn lim with
  | "stats" ->
      if not (is_real_player player) then
        ANSITerminal.print_string [ ANSITerminal.green ] "AI is checking stats";
      print_game_status game;
      end_of_turn player game (lim + 1)
  | "build" ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        ( "Checking which of " ^ get_name player
        ^ "'s properties can be built on.\n" );
      let props = buildable_props player in
      if List.length props > 0 then check_build player game props
      else
        ANSITerminal.print_string [ ANSITerminal.red ]
          "No houses can be built at this time\n";
      end_of_turn player game (lim + 1)
  | "sell" ->
      sell_buildings player game true;
      (*TELEPORT*)
      end_of_turn player game (lim + 1)
  | "mortgage" ->
      mortgage_property player game true;
      end_of_turn player game (lim + 1)
  | "unmortgage" ->
      unmortgage_property player game true;
      end_of_turn player game (lim + 1)
  | "deal" ->
      deal_prompt player game;
      end_of_turn player game (lim + 1)
  | "end" ->
      if not (is_real_player player) then
        ANSITerminal.print_string [ ANSITerminal.red ]
          ("AI " ^ get_name player ^ " has ended their turn")
  | _ -> end_of_turn player game lim

let ai_forfeit () =
  Random.self_init ();
  if Random.int 100 = 50 then "forfeit" else ""

let rec current_turn game =
  if not (last_one_standing game) then (
    let curr = current_player game in
    ANSITerminal.print_string [ ANSITerminal.green ]
      ( "\nCurrent player: " ^ get_name curr
      ^ if is_real_player curr then "" else " (AI PLAYER)" );
    if not (in_jail curr) then
      (* print_endline " NOT IN JAIL";*)
      play_a_turn game
    else (
      ANSITerminal.print_string [ ANSITerminal.red ]
        ("\n" ^ get_name curr ^ " is in jail\n");
      jail_prompt curr game;
      if in_jail curr then (
        served_a_turn curr;
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ( string_of_int (time_left curr)
          ^ " turn(s) left in jail for " ^ get_name curr ^ "\n" ) ) );
    end_of_turn curr game 0;
    ANSITerminal.print_string [ ANSITerminal.green ]
      ( "\nContinue playing? Enter 'f' to forfeit, 'q' to quit the entire game\n"
      ^ "anything else to start the next turn.\n" );
    match if is_real_player curr then read_line () else ai_forfeit () with
    | "forfeit" ->
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
    | "quit" -> ANSITerminal.print_string [ ANSITerminal.green ] "Bye!\n\n"
    | _ ->
        move_to_next_player game;
        current_turn game )
  else win_message game

let rec addl_player game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\
     Add a real player or an AI player?\n\
     Enter 'ai' for an AI player, anything else for a real player\n";
  let is_ai = read_line () = "ai" in
  let old_num = num_players game in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ( "\n Please give the next "
    ^ (if is_ai then "AI" else "player")
    ^ " a name \n Entering nothing will yield the default name\n" );
  let str = read_line () in
  let new_plyr =
    if is_ai then
      create_ai
        (if str = "" then acceptable_default_name game true else str)
        (get_start_pos game)
    else
      create_player
        (if str = "" then acceptable_default_name game false else str)
        (get_start_pos game)
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
    match read_line () with
    | "add" -> addl_player game
    | _ -> (
        ANSITerminal.print_string [ ANSITerminal.green ]
          "Press Enter key to start game\n";
        match read_line () with _ -> current_turn game ) )
  else (
    ANSITerminal.print_string [ ANSITerminal.green ]
      "Max number of players reached, press Enter key to start game\n";
    match read_line () with _ -> current_turn game )

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
    "\nWelcome to our 3110 Group Project: Monopoly\n";

  let board = Json_reader.create_board "standard_board.json" in
  let the_game = create_game board in
  first_player the_game

let () = main ()
