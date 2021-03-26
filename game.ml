open Property
open Player
open Gameboard

type players = Player.t list

type t = { board : Gameboard.t; player_list : players }

let roll_dice () = 2 + Random.int 5 + Random.int 5

let get_new_position player the_game =
  let moves = roll_dice () in
  player |> get_position |> get_index the_game.board |> ( + ) moves

let move_player player game =
  let new_index = get_new_position player game mod 40 in
  let new_position = get_prop_at_index new_index game.board in
  change_pos player new_position

(* 1: check position of player
      a: get_position player (gets us property)
      b: get index of current_pos
   2: add dice roll mod 40
   3: Find position where index is located
   4: Move player here
 *)
let get_players game = game.player_list

let find_player player_name (lst : players) =
  List.find (fun x -> get_name x = player_name) lst

let mortgage player property game = failwith "Unimplemented"

let collect_rent player property game =
  let rent_owed = calculate_rent property in
  if player_money player >= rent_owed then (
    let owner = find_player (get_owner property) (get_players game) in
    update_player_money owner rent_owed;
    update_player_money player (-1 * rent_owed))
  else mortgage player property game
