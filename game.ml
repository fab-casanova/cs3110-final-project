open Property
open Player
open Gameboard

type game = { board : gameboard; players : player list }

let roll_dice () = 2 + Random.int 5 + Random.int 5

let next_pos board moves pos = 0

let move_player_helper pos = pos

let move_player player board = move_player_helper (get_position player)

let find_player player_name lst =
  List.find (fun x -> get_name x = player_name) lst

let mortgage plyr property game = failwith "Unimplemented"

let collect_rent plyr property game =
  let rent_owed = calculate_rent property in
  if player_money plyr >= rent_owed then (
    let owner = find_player (get_owner property) game.players in
    update_player_money owner rent_owed;
    update_player_money plyr (-1 * rent_owed))
  else mortgage plyr property game
