open Game
open Player
open Property

type giver = Player.t option

type receiver = Player.t option

type transaction = giver * int * receiver

type t = { text : string; effect : Game.t -> transaction list }

let get_money money game = [ (None, money, Some (current_player game)) ]

let pay_money money game = [ (Some (current_player game), money, None) ]

let advance_to_property property_name game =
  let player = current_player game in
  let old_pos = get_position player in
  change_pos player (get_property_of_name property_name game);
  let new_pos = get_position player in
  if get_index game new_pos - get_index game old_pos < 0 then get_money 200 game
  else if property_name = "Pass Go" then get_money 200 game
  else []

let advance_to_nearest_utility game =
  let pos = get_index game (get_position (current_player game)) in
  if pos > 12 && pos < 19 then
    change_pos (current_player game) (get_property_of_name "Water Works" game)
  else
    change_pos (current_player game)
      (get_property_of_name "Electric Company" game);
  []

let advance_to_nearest_railroad game =
  let pos = get_index game (get_position (current_player game)) in
  let name =
    if pos > 35 || pos < 6 then "Reading Railroad"
    else if pos > 5 && pos < 16 then "Pennsylvania Railroad"
    else if pos > 15 && pos < 26 then "B & O Railroad"
    else if pos > 25 && pos < 36 then "Short Line Railroad"
    else ""
  in
  let rail = get_property_of_name name game in
  change_pos (current_player game) rail;
  if is_owned rail then
    [
      ( Some (current_player game),
        calculate_dues rail game,
        Some (get_owner rail game) );
    ]
  else []

let jail_free deck game =
  add_jail_free_card deck (current_player game);
  []

let back_3 game =
  move_player (current_player game) game (-3) true;
  []

let go_to_jail game =
  let player = current_player game in
  change_pos player (get_property_of_name "Jail" game);
  put_in_jail player;
  []

let house_hotel_payment house_amt hotel_amt game : transaction list =
  let player = current_player game in
  let properties = get_properties player in
  let rec repair_cost properties acc =
    match properties with
    | [] -> acc
    | h :: t ->
        let houses = num_houses h in
        if houses = 5 then repair_cost t (acc + hotel_amt)
        else repair_cost t (acc + (house_amt * houses))
  in
  let cost = repair_cost properties 0 in
  [ (Some player, cost, None) ]

let pay_all_players payment_per_player game : transaction list =
  let current = current_player game in
  let rec trans_list players acc =
    match players with
    | [] -> acc
    | h :: t ->
        if h <> current then
          if payment_per_player > 0 then
            trans_list t ((Some current, payment_per_player, Some h) :: acc)
          else trans_list t ((Some h, -payment_per_player, Some current) :: acc)
        else trans_list t acc
  in
  trans_list (get_players game) []

let chance_cards =
  [
    {
      text = "Advance to Go (Collect $200)";
      effect = advance_to_property "Pass Go";
    };
    {
      text = "Advance to Illinois Ave—If you pass Go, collect $200";
      effect = advance_to_property "Illinois Avenue";
    };
    {
      text = "Advance to St. Charles Place – If you pass Go, collect $200";
      effect = advance_to_property "St. Charles Place";
    };
    {
      text =
        "Advance token to nearest Utility. If unowned, you may buy it from the \
         Bank. If owned, throw dice and pay owner a total ten times the amount \
         thrown.";
      effect = advance_to_nearest_utility;
    };
    {
      text =
        "Advance token to the nearest Railroad and pay owner twice the rental \
         to which they are otherwise entitled. If Railroad is unowned, you may \
         buy it from the Bank.";
      effect = advance_to_nearest_railroad;
    };
    { text = "Bank pays you dividend of $50"; effect = get_money 50 };
    { text = "Get Out of Jail Free"; effect = jail_free "chance" };
    { text = "Go Back 3 Spaces"; effect = back_3 };
    {
      text =
        "Go to Jail–Go directly to Jail–Do not pass Go, do not collect $200";
      effect = go_to_jail;
    };
    {
      text =
        "Make general repairs on all your property–For each house pay \
         $25–For each hotel $100";
      effect = house_hotel_payment 25 100;
    };
    { text = "Pay poor tax of $15"; effect = pay_money 15 };
    {
      text = "Take a trip to Reading Railroad–If you pass Go, collect $200";
      effect = advance_to_property "Reading Railroad";
    };
    {
      text = "Take a walk on the Boardwalk–Advance token to Boardwalk";
      effect = advance_to_property "Boardwalk";
    };
    {
      text = "You have been elected Chairman of the Board–Pay each player $50";
      effect = pay_all_players 50;
    };
    {
      text = "Your building and loan matures—Collect $150";
      effect = get_money 150;
    };
    {
      text = "You have won a crossword competition—Collect $100";
      effect = get_money 100;
    };
  ]

let com_chest =
  [
    {
      text = "Advance to Go (Collect $200)";
      effect = advance_to_property "Pass Go";
    };
    { text = "Bank error in your favor—Collect $200"; effect = get_money 200 };
    { text = "Doctor's fee—Pay $50"; effect = pay_money 50 };
    { text = "From sale of stock you get $50"; effect = get_money 50 };
    { text = "Get Out of Jail Free"; effect = jail_free "community chest" };
    {
      text =
        "Go to Jail–Go directly to jail–Do not pass Go–Do not collect \
         $200";
      effect = go_to_jail;
    };
    {
      text =
        "Grand Opera Night—Collect $50 from every player for opening night \
         seats";
      effect = pay_all_players (-50);
    };
    { text = "Holiday Fund matures—Receive $100"; effect = get_money 100 };
    { text = "Income tax refund–Collect $20"; effect = get_money 20 };
    { text = "It is your birthday—Collect $10"; effect = get_money 10 };
    { text = "Life insurance matures–Collect $100"; effect = get_money 100 };
    { text = "Pay hospital fees of $100"; effect = pay_money 100 };
    { text = "Pay school fees of $150"; effect = pay_money 150 };
    { text = "Receive $25 consultancy fee"; effect = get_money 25 };
    {
      text =
        "You are assessed for street repairs–$40 per house–$115 per hotel";
      effect = house_hotel_payment 40 115;
    };
    {
      text = "You have won second prize in a beauty contest–Collect $10";
      effect = get_money 10;
    };
    { text = "You inherit $100"; effect = get_money 100 };
  ]

let get_card card_type card_id =
  match card_type with
  | "chance" -> List.nth chance_cards card_id
  | "community chest" -> List.nth com_chest card_id
  | _ -> List.nth chance_cards card_id

let card_text card = card.text

let card_effect card game = card.effect game
