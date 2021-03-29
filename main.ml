open Property
open Player
open Game

let current_property_effects player game =
  let pos = get_position player in
  print_endline ("Position " ^ string_of_int (get_index game pos) ^ " of 40");
  if is_tax pos then (
    print_endline "Tax will be collected";
    Player.collect_tax player pos)
  else if is_owned pos then (
    print_endline (prop_name pos ^ " is owned");
    let owner = find_player (get_owner pos) (get_players game) in
    print_endline ("The owner of " ^ prop_name pos ^ " is " ^ get_name owner);
    if owner = player then print_endline "This is your property"
    else collect_rent player owner pos)
  else if can_be_purchased pos then
    print_endline (prop_name pos ^ " can be purchased")

let play_a_turn game player =
  move_player player game;
  print_endline (get_name player ^ " is at: " ^ prop_name (get_position player));
  current_property_effects player game;
  print_endline
    (get_name player ^ "'s money: $" ^ string_of_int (player_money player))

let rec current_turn game =
  play_a_turn game (find_player (current_player_name game) (get_players game));
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\nContinue rolling? Type 'y' if yes\n";
  match read_line () with
  | "y" -> current_turn game
  | _ -> ANSITerminal.print_string [ ANSITerminal.green ] "Bye!\n"

let first_player game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\
    \ Please give the first player a name: \n\
    \ Empty names will default to 'playern' where playern is the nth player: \n\n";
  match read_line () with
  | "" ->
      let n = num_players game in
      add_a_player game
        (create_player ("player" ^ string_of_int (n + 1)) (get_start_pos game));
      ANSITerminal.print_string [ ANSITerminal.green ]
        ("\nFirst player is : " ^ current_player_name game ^ "\n");
      current_turn game
  | str ->
      add_a_player game (create_player str (get_start_pos game))
      (* Add additional players in MS2*);
      ANSITerminal.print_string [ ANSITerminal.green ]
        ("\nFirst player is : " ^ current_player_name game ^ "\n");
      current_turn game

let rec main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\nWelcome to our 3110 Group Project: Monopoly\n\nUsing standard board\n";

  let board = Standard_board.standard_board in
  let the_game = Game.create_game board (create_list_of_players []) in
  first_player the_game

let () = main ()
