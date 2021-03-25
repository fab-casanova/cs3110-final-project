open Property
open Player
open Gameboard

type t = { board : Gameboard.t; players : Player.t list }

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

let find_player player_name lst =
  List.find (fun x -> get_name x = player_name) lst

let mortgage player property game = failwith "Unimplemented"

let collect_rent player property game =
  let rent_owed = calculate_rent property in
  if player_money player >= rent_owed then (
    let owner = find_player (get_owner property) game.players in
    update_player_money owner rent_owed;
    update_player_money player (-1 * rent_owed))
  else mortgage player property game

let has_monopoly player new_property =
  (* TODO: IMPLEMENT USING PATTERN MATCHING*)
  (*let property_type = get_type new_property in let properties_required = if property_type = (Brown or DBlue) then 2 else if property_type =    Pink
        or Orange
        or Red
        or Yellow
        or Green then 3 else -1 in let owned_properties = get_properties player in let rec monopoly_helper player owned_properties property_type properties_required acc =
        match owned_properties with |[] -> () |h :: t -> if get_type h = property_type then if acc = properties_required - 1 then add_monopoly player property_type
        else monopoly_helper player owned_properties property_type properties_required (acc + 1)
      ()
    let buy_property player property =
      update_player_money player (get_price property);
      set_owner property (get_name player);
      has_monopoly player property;
      ()
  *)
  ()
