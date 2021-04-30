open OUnit2
open Property
open Player
open Game

(*Player Done*)
(*Property Done*)
(*Game*)
(*Cards*)
(*Main*)

(*Template*)
(*let funct_test name inp1 inp2 inp3 out = name >:: fun _ -> assert_equal out
  (inp1 inp2 inp3)*)

(*Player functions*)

let create_player_test name player_name start out =
  name >:: fun _ -> assert_equal out (create_player player_name start)

let get_name_test name player out =
  name >:: fun _ -> assert_equal out (get_name player)

let get_properties_test name player out =
  name >:: fun _ -> assert_equal out (get_properties player)

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

let change_pos_test name player new_pos out =
  name >:: fun _ -> assert_equal out (change_pos player new_pos)

let sum_dice_test name dice out =
  name >:: fun _ -> assert_equal out (sum_dice dice)

let roll_dice_test name out = name >:: fun _ -> assert_equal out (roll_dice ())

let has_monopoly_test name player prop out =
  name >:: fun _ -> assert_equal out (has_monopoly player prop)

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

let owns_property_test name player prop out =
  name >:: fun _ -> assert_equal out (owns_property player prop)

let mortgage_allowed_test name player prop out =
  name >:: fun _ -> assert_equal out (mortgage_allowed player prop)

let remove_property_test name player prop out =
  name >:: fun _ -> assert_equal out (remove_property player prop)

let return_prop_to_bank_test name player prop out =
  name >:: fun _ -> assert_equal out (return_prop_to_bank player prop)

let swap_owner_test name giver receiver prop out =
  name >:: fun _ -> assert_equal out (swap_owner giver receiver prop)

let hand_over_all_properties_test name giver receiver out =
  name >:: fun _ -> assert_equal out (hand_over_all_properties giver receiver)

let clear_properties_test name player out =
  name >:: fun _ -> assert_equal out (clear_properties player)

let num_jail_free_cards_test name player out =
  name >:: fun _ -> assert_equal out (num_jail_free_cards player)

let add_jail_free_card_test name deck player out =
  name >:: fun _ -> assert_equal out (add_jail_free_card deck player)

let owns_jail_free_card_test name deck player out =
  name >:: fun _ -> assert_equal out (owns_jail_free_card deck player)

let player_tests = []

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

let set_owner_test name prop owner out =
  name >:: fun _ -> assert_equal out (set_owner prop owner)

let release_property_test name prop out =
  name >:: fun _ -> assert_equal out (release_property prop)

let reset_stage_test name prop out =
  name >:: fun _ -> assert_equal out (reset_stage prop)

let current_stage_test name prop out =
  name >:: fun _ -> assert_equal out (current_stage prop)

let create_mortgage_test name prop out =
  name >:: fun _ -> assert_equal out (create_mortgage prop)

let unmortgage_test name prop out =
  name >:: fun _ -> assert_equal out (unmortgage prop)

let is_mortgaged_test name prop out =
  name >:: fun _ -> assert_equal out (is_mortgaged prop)

let get_type_test name prop out =
  name >:: fun _ -> assert_equal out (get_type prop)

let upgrade_property_test name prop out =
  name >:: fun _ -> assert_equal out (upgrade_property prop)

let downgrade_property_test name prop out =
  name >:: fun _ -> assert_equal out (downgrade_property prop)

let num_for_monopoly_test name prop out =
  name >:: fun _ -> assert_equal out (num_for_monopoly prop)

let num_houses_test name prop out =
  name >:: fun _ -> assert_equal out (num_houses prop)

let get_value_test name prop out =
  name >:: fun _ -> assert_equal out (get_value prop)

let property_tests = []

(*Game functions*)

let create_players_test name lst out =
  name >:: fun _ -> assert_equal out (create_players lst)

let last_one_standing_test name game out =
  name >:: fun _ -> assert_equal out (last_one_standing game)

let create_gameboard_test name lst out =
  name >:: fun _ -> assert_equal out (create_gameboard lst)

let pot_amount_test name game out =
  name >:: fun _ -> assert_equal out (pot_amount game)

let add_to_pot_test name game cash out =
  name >:: fun _ -> assert_equal out (add_to_pot game cash)

(*let draw_card_test name deck game out = name >:: fun _ -> assert_equal out
  (draw_card deck game)*)

let owns_property_of_name_test name player name game out =
  name >:: fun _ -> assert_equal out (owns_property_of_name player name game)

let create_game_test name board players out =
  name >:: fun _ -> assert_equal out (create_game board players)

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

(* let move_front_to_back_test name lst out = name >:: fun _ -> assert_equal out
   (move_front_to_back lst)

   let funct_test name inp1 inp2 inp3 out = name >:: fun _ -> assert_equal out
   (funct inp1 inp2 inp3)

   let funct_test name inp1 inp2 inp3 out = name >:: fun _ -> assert_equal out
   (funct inp1 inp2 inp3)

   let funct_test name inp1 inp2 inp3 out = name >:: fun _ -> assert_equal out
   (funct inp1 inp2 inp3)

   let funct_test name inp1 inp2 inp3 out = name >:: fun _ -> assert_equal out
   (funct inp1 inp2 inp3)

   let funct_test name inp1 inp2 inp3 out = name >:: fun _ -> assert_equal out
   (funct inp1 inp2 inp3)

   let funct_test name inp1 inp2 inp3 out = name >:: fun _ -> assert_equal out
   (funct inp1 inp2 inp3)

   let funct_test name inp1 inp2 inp3 out = name >:: fun _ -> assert_equal out
   (funct inp1 inp2 inp3)

   let funct_test name inp1 inp2 inp3 out = name >:: fun _ -> assert_equal out
   (funct inp1 inp2 inp3)

   let funct_test name inp1 inp2 inp3 out = name >:: fun _ -> assert_equal out
   (funct inp1 inp2 inp3) *)
let suite = "test suite" >::: List.flatten []

let _ = run_test_tt_main suite
