open Player
open Game

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
