open Property
open Player
open Gameboard

type game = { board : gameboard; players : player list }

let roll_dice () = 2 + Random.int 5 + Random.int 5

let next_pos board moves pos = 0

let move_player_helper pos = pos

let move_player player board = move_player_helper (get_position player)


let find_player player_name lst = 
    List.find (fun x -> get_name x = )

let collect_rent renter property =
  update_player_money (get_owner property) (calculate_rent property)
