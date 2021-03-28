open Player
open Game

let first_player game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\
    \ Please give the first player a name\n\
    \ Empty names will default to 'playern' where playern is the nth player";
  match read_line () with
  | "" ->
      let n = num_players game in
      add_a_player game
        (create_player ("player" ^ string_of_int n) (get_start_pos game))
  | str -> add_a_player game (create_player str (get_start_pos game))

let rec main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\nWelcome to our 3110 Group Project: Monopoly\n\nUsing standard board\n";

  let board = Standard_board.standard_board in
  let the_game = Game.create_game board (create_list_of_players []) in
  first_player the_game

let () = main ()
