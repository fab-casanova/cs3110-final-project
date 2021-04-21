open Game
open Player
open Property

type t = { text : string; effect : Game.t -> unit }

(*TODO: MAKE PASSED_GO (...->bool) in game.ml*)
let advance_to_go game =
  change_pos (current_player game) (get_property_of_name "Pass Go" game)

let advance_to_illinois game =
  change_pos (current_player game) (get_property_of_name "Illinois Avenue" game)

let advance_to_st_charles game =
  change_pos (current_player game)
    (get_property_of_name "St. Charles Place" game)

let advance_to_nearest_utility game =
  let pos = get_index game (get_position (current_player game)) in
  if pos > 12 && pos < 19 then
    change_pos (current_player game) (get_property_of_name "Water Works" game)
  else
    change_pos (current_player game)
      (get_property_of_name "Electric Company" game)

let advance_to_nearest_railroad game =
  let pos = get_index game (get_position (current_player game)) in
  if pos > 35 || pos < 6 then
    change_pos (current_player game)
      (get_property_of_name "Reading Railroad" game)
  else if pos > 5 && pos < 16 then
    change_pos (current_player game)
      (get_property_of_name "Pennsylvania Railroad" game)
  else if pos > 15 && pos < 26 then
    change_pos (current_player game)
      (get_property_of_name "B & O Railroad" game)
  else if pos > 25 && pos < 36 then
    change_pos (current_player game)
      (get_property_of_name "Short Line Railroad" game)

let bank_50_dividend game = update_player_money (current_player game) 50

(*TODO: get out of jail free*)
let jail_free game = ()

let back_3 game = move_player (current_player game) game (-3) true

let go_to_jail game =
  let player = current_player game in
  change_pos player (get_property_of_name "Jail" game);
  put_in_jail player

let general_repairs game =
  let player = current_player game in
  let properties = get_properties player in
  let rec repair_cost properties acc =
    match properties with
    | [] -> acc
    | h :: t ->
        let houses = num_houses h in
        if houses = 5 then repair_cost t (acc + 100)
        else repair_cost t (acc + (25 * houses))
  in
  let cost = repair_cost properties 0 in
  add_to_pot game cost;
  update_player_money (current_player game) (-cost)

let poor_tax game =
  add_to_pot game 15;
  update_player_money (current_player game) (-15)

let to_reading_railroad game =
  change_pos (current_player game)
    (get_property_of_name "Reading Railroad" game)

let go_to_boardwalk game =
  change_pos (current_player game) (get_property_of_name "Boardwalk" game)

let elected_chairman game =
  let total_payment = (num_players game - 1) * 50 in
  let players = get_players game in
  let current = current_player game in
  add_to_pot game total_payment;
  update_player_money current (-total_payment);
  match players with
  | [] -> ()
  | h :: t -> if h <> current then update_player_money h 50

let loan_matures game = update_player_money (current_player game) 150

let crossword_competition game = update_player_money (current_player game) 100

let chance_cards =
  [
    { text = "Advance to Go (Collect $200)"; effect = advance_to_go };
    {
      text = "Advance to Illinois Ave—If you pass Go, collect $200";
      effect = advance_to_illinois;
    };
    {
      text = "Advance to St. Charles Place – If you pass Go, collect $200";
      effect = advance_to_st_charles;
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
    { text = "Bank pays you dividend of $50"; effect = bank_50_dividend };
    { text = "Get Out of Jail Free"; effect = jail_free };
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
      effect = general_repairs;
    };
    { text = "Pay poor tax of $15"; effect = poor_tax };
    {
      text = "Take a trip to Reading Railroad–If you pass Go, collect $200";
      effect = to_reading_railroad;
    };
    {
      text = "Take a walk on the Boardwalk–Advance token to Boardwalk";
      effect = go_to_boardwalk;
    };
    {
      text = "You have been elected Chairman of the Board–Pay each player $50";
      effect = elected_chairman;
    };
    {
      text = "Your building and loan matures—Collect $150";
      effect = loan_matures;
    };
    {
      text = "You have won a crossword competition—Collect $100";
      effect = crossword_competition;
    };
  ]
