open Property
open Player
open Game

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
      print_endline "Please respond with 'yes' or no";
      buy_prompt game player pos

let current_property_effects game player =
  let pos = get_position player in
  print_endline ("Position " ^ string_of_int (get_index game pos) ^ " of 40");
  if is_com_or_chance pos then
    print_endline ("Draw a " ^ prop_space_type pos ^ " card!")
  else if is_tax pos then (
    print_endline "Tax will be collected";
    collect_tax player pos game)
  else if is_owned pos then (
    print_endline (prop_name pos ^ " is owned");
    let owner = find_player (get_owner pos) (get_players game) in
    print_endline ("The owner of " ^ prop_name pos ^ " is " ^ get_name owner);
    if owner = player then print_endline "This is your property"
    else collect_rent player owner pos game)
  else if can_be_purchased pos then (
    print_endline
      (prop_name pos ^ " can be purchased for $"
      ^ string_of_int (purchase_price pos)
      ^ "\n");
    if player_money player >= purchase_price pos then buy_prompt game player pos
    else
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Not enough funds to purchase\n\n")

let rec build_prompt player prop =
  ANSITerminal.print_string [ ANSITerminal.blue ] "You currently have ";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    ("$" ^ string_of_int (player_money player));
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (".\nYou can build a house on " ^ prop_name prop ^ " for ");
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    ("$" ^ string_of_int (house_cost prop));
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ". Type 'y' if yes, and anything else for no\n";
  match read_line () with
  | "y" ->
      upgrade_property prop;
      update_player_money player (-1 * house_cost prop);
      ANSITerminal.print_string [ ANSITerminal.green ]
        (prop_name prop ^ " is now at Level " ^ what_stage prop ^ "\n")
  | "n" -> print_endline ("\n" ^ prop_name prop ^ " did not have a house built")
  | _ ->
      print_endline "Please respond with 'yes' or no";
      build_prompt player prop

let rec check_build (game : Game.t) (player : Player.t) = function
  | [] -> ()
  | h :: t ->
      if can_build_houses_hotel player h then build_prompt player h;
      check_build game player t

let play_a_turn game =
  let player = current_player game in
  move_player player game;
  let pos = get_position player in
  print_endline
    (get_name player ^ " is at: " ^ prop_name pos ^ " (" ^ prop_space_type pos
   ^ ")");
  current_property_effects game player;
  check_build game player (get_properties player);
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    (get_name player ^ "'s money: $"
    ^ string_of_int (player_money player)
    ^ "\n");
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (get_name player ^ "'s properties: " ^ pp_properties player ^ "\n ")

let rec current_turn game =
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("\nCurrent player: " ^ get_name (current_player game));
  play_a_turn game;
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\nContinue playing? Type 'y' if yes, and anything else for no\n";
  match read_line () with
  | "y" ->
      move_to_next_player game;
      current_turn game
  | _ -> ANSITerminal.print_string [ ANSITerminal.green ] "Bye!\n\n"

let first_player game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\
    \ Please give the first player a name: \n\
    \ Empty names will default to 'playern' where playern is the nth player: \n\n";
  match read_line () with
  | str -> (
      add_a_player game
        (create_player
           (if str = "" then "player1" else str)
           (get_start_pos game));
      ANSITerminal.print_string [ ANSITerminal.green ]
        ("\nFirst player is: "
        ^ get_name (current_player game)
        ^ "\nPress the 'Enter' key to start");
      (*Prompt adding players in MS2*)
      match read_line () with _ -> current_turn game)

let rec main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\nWelcome to our 3110 Group Project: Monopoly\n\nUsing standard board\n";

  let board = Standard_board.standard_board in
  let the_game = Game.create_game board (create_players []) in
  first_player the_game

let () = main ()
