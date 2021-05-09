open OUnit2
open Property
open Player
open Game
open Cards

(*Player Done*)
(*Property Done*)
(*Game Done*)
(*Cards Done*)
(*Main*)

(*Template*)
(*let funct_test name inp1 inp2 inp3 out = name >:: fun _ -> assert_equal out
  (funct inp1 inp2 inp3)*)

(* Code for testing *)
let property_test =
  create_buyable_card "Test" "Green" [| 10; 20; 30; 40; 50; 60 |] 500 100

let player_test = create_player "player" property_test

let dice_test = (5, 4)

(*Player functions*)

let get_name_test name player out =
  name >:: fun _ -> assert_equal out (get_name player)

let get_properties_test name player out =
  name >:: fun _ -> assert_equal out (get_properties player)

(* Skipped *)
let num_doubles_test name player out =
  name >:: fun _ -> assert_equal out (num_doubles player)

let player_money_test name player out =
  name >:: fun _ -> assert_equal out (player_money player)

let get_position_test name player out =
  name >:: fun _ -> assert_equal out (get_position player)

let in_jail_test name player out =
  name >:: fun _ -> assert_equal out (in_jail player)

let time_left_test name player out =
  name >:: fun _ -> assert_equal out (time_left player)

let sum_dice_test name dice out =
  name >:: fun _ -> assert_equal out (sum_dice dice)

(*Skipped*)
let roll_dice_test name out = name >:: fun _ -> assert_equal out (roll_dice ())

let has_monopoly_test name player prop out =
  name >:: fun _ -> assert_equal out (has_monopoly player prop)

(* Skipped *)
let calculate_owned_rent_test name prop owner out =
  name >:: fun _ -> assert_equal out (calculate_owned_rent prop owner)

let is_building_evenly_test name props prop is_building out =
  name >:: fun _ -> assert_equal out (is_building_evenly props prop is_building)

let can_build_houses_hotel_test name player prop out =
  name >:: fun _ -> assert_equal out (can_build_houses_hotel player prop)

let out_of_cash_test name amount_owed player out =
  name >:: fun _ -> assert_equal out (out_of_cash amount_owed player)

let net_worth_test name player out =
  name >:: fun _ -> assert_equal out (net_worth player)

let is_bankrupt_test name amount_owed player out =
  name >:: fun _ -> assert_equal out (is_bankrupt amount_owed player)

let player_status_test name dues player out =
  name >:: fun _ -> assert_equal out (player_status dues player)

let no_houses_on_monopoly_test name player prop out =
  name >:: fun _ -> assert_equal out (no_houses_on_monopoly player prop)

(* Pending *)
let owns_property_test name player prop out =
  name >:: fun _ -> assert_equal out (owns_property player prop)

let player_tests =
  [
    get_name_test "basic get name test" player_test "player";
    get_properties_test "basic get properties test" player_test [];
    player_money_test "basic player money test" player_test 1500;
    get_position_test "basic get position test" player_test property_test;
    in_jail_test "basic in jail test" player_test false;
    time_left_test "basic time left test" player_test 0;
    sum_dice_test "basic dice test" dice_test 9;
    has_monopoly_test "basic has monopoly test" player_test property_test false;
    is_building_evenly_test "basic building evenly test" [ property_test ]
      property_test true true;
    can_build_houses_hotel_test "basic can biuld houses test" player_test
      property_test true;
    out_of_cash_test "basic out of cash test" 0 player_test false;
    net_worth_test "basic net worth test" player_test 1500;
    is_bankrupt_test "basic is bankrupt test" 0 player_test false;
    player_status_test "basic player status test" 0 player_test 0;
    no_houses_on_monopoly_test "basic no house on monopoly test" player_test
      property_test true;
  ]

let mortgage_allowed_test name player prop out =
  name >:: fun _ -> assert_equal out (mortgage_allowed player prop)

let num_jail_free_cards_test name player out =
  name >:: fun _ -> assert_equal out (num_jail_free_cards player)

let owns_jail_free_card_test name deck player out =
  name >:: fun _ -> assert_equal out (owns_jail_free_card deck player)

(* player_tests goes here (might be somewhere else) *)

(*Property functions*)

let prop_name_test name prop out =
  name >:: fun _ -> assert_equal out (prop_name prop)

let prop_space_type_test name prop out =
  name >:: fun _ -> assert_equal out (prop_space_type prop)

let can_be_purchased_test name prop out =
  name >:: fun _ -> assert_equal out (can_be_purchased prop)

let can_be_upgraded_test name prop out =
  name >:: fun _ -> assert_equal out (can_be_upgraded prop)

let what_stage_test name prop out =
  name >:: fun _ -> assert_equal out (what_stage prop)

let can_have_houses_test name prop out =
  name >:: fun _ -> assert_equal out (can_have_houses prop)

let is_owned_test name prop out =
  name >:: fun _ -> assert_equal out (is_owned prop)

let is_com_or_chance_test name prop out =
  name >:: fun _ -> assert_equal out (is_com_or_chance prop)

let is_utilities_test name prop out =
  name >:: fun _ -> assert_equal out (is_utilities prop)

let is_free_parking_test name prop out =
  name >:: fun _ -> assert_equal out (is_free_parking prop)

let is_railroad_test name prop out =
  name >:: fun _ -> assert_equal out (is_railroad prop)

let is_tax_test name prop out = name >:: fun _ -> assert_equal out (is_tax prop)

let is_jail_test name prop out =
  name >:: fun _ -> assert_equal out (is_jail prop)

let is_go_to_jail_test name prop out =
  name >:: fun _ -> assert_equal out (is_go_to_jail prop)

let create_buyable_card_test name name_input card_type prices price house_price
    out =
  name >:: fun _ ->
  assert_equal out
    (create_buyable_card name_input card_type prices price house_price)

let create_unbuyable_card_test name name_input card_type rent out =
  name >:: fun _ ->
  assert_equal out (create_unbuyable_card name_input card_type rent)

let calculate_rent_or_tax_test name prop out =
  name >:: fun _ -> assert_equal out (calculate_rent_or_tax prop)

let purchase_price_test name prop out =
  name >:: fun _ -> assert_equal out (purchase_price prop)

let house_cost_test name prop out =
  name >:: fun _ -> assert_equal out (house_cost prop)

let get_owner_name_test name prop out =
  name >:: fun _ -> assert_equal out (get_owner_name prop)

let current_stage_test name prop out =
  name >:: fun _ -> assert_equal out (current_stage prop)

let is_mortgaged_test name prop out =
  name >:: fun _ -> assert_equal out (is_mortgaged prop)

let get_type_test name prop out =
  name >:: fun _ -> assert_equal out (get_type prop)

let num_for_monopoly_test name prop out =
  name >:: fun _ -> assert_equal out (num_for_monopoly prop)

let num_houses_test name prop out =
  name >:: fun _ -> assert_equal out (num_houses prop)

let get_value_test name prop out =
  name >:: fun _ -> assert_equal out (get_value prop)

let property_tests = [ get_name_test "Player name test" player_test "player" ]

(*Game functions*)

let last_one_standing_test name game out =
  name >:: fun _ -> assert_equal out (last_one_standing game)

let create_gameboard_test name lst out =
  name >:: fun _ -> assert_equal out (create_gameboard lst)

let pot_amount_test name game out =
  name >:: fun _ -> assert_equal out (pot_amount game)

let owns_property_of_name_test name player name game out =
  name >:: fun _ -> assert_equal out (owns_property_of_name player name game)

let create_game_test name board out =
  name >:: fun _ -> assert_equal out (create_game board)

let get_property_of_name_test name name_input game out =
  name >:: fun _ -> assert_equal out (get_property_of_name name_input game)

let get_start_pos_test name game out =
  name >:: fun _ -> assert_equal out (get_start_pos game)

let num_players_test name game out =
  name >:: fun _ -> assert_equal out (num_players game)

let current_player_name_test name game out =
  name >:: fun _ -> assert_equal out (current_player_name game)

let still_in_game_test name player game out =
  name >:: fun _ -> assert_equal out (still_in_game player game)

let get_players_test name game out =
  name >:: fun _ -> assert_equal out (get_players game)

let find_player_test name name_input lst out =
  name >:: fun _ -> assert_equal out (find_player name_input lst)

let get_owner_test name prop game out =
  name >:: fun _ -> assert_equal out (get_owner prop game)

let get_jail_test name game out =
  name >:: fun _ -> assert_equal out (get_jail game)

let current_player_test name game out =
  name >:: fun _ -> assert_equal out (current_player game)

let get_index_test name game prop out =
  name >:: fun _ -> assert_equal out (get_index game prop)

let get_prop_at_index_test name index game out =
  name >:: fun _ -> assert_equal out (get_prop_at_index index game)

let calculate_dues_test name prop game out =
  name >:: fun _ -> assert_equal out (calculate_dues prop game)

let get_deck_test name card_type game out =
  name >:: fun _ -> assert_equal out (get_deck card_type game)

let game_tests = []

(*Card functions*)

let get_card_test name card_type card_id out =
  name >:: fun _ -> assert_equal out (get_card card_type card_id)

let card_text_test name card out =
  name >:: fun _ -> assert_equal out (card_text card)

let card_effect_test name card game out =
  name >:: fun _ -> assert_equal out (card_effect card game)

let card_tests = []

let suite = "test suite" >::: List.flatten []

let _ = run_test_tt_main suite
