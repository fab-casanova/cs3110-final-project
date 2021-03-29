open Property
open Player

type gameboard = Property.t list

type players = Player.t list

type t = {
  board : gameboard;
  mutable player_list : players;
  mutable current_player : string;
}

let create_list_of_players (lst : Player.t list) : players = lst

let create_gameboard (lst : Property.t list) : gameboard = lst

let create_game (board : gameboard) (players : players) =
  { board; player_list = players; current_player = "" }

let get_start_pos game = List.hd game.board

let num_players game = List.length game.player_list

let current_player_name game = game.current_player

let add_a_player game player =
  if List.length game.player_list > 3 then
    print_endline "Max number of players reached, cannot add more\n"
  else
    let name_lst = List.map get_name game.player_list in
    if List.mem (get_name player) name_lst then
      print_endline "This name is already taken\n"
    else game.player_list <- game.player_list @ [ player ];
    if game.current_player = "" then game.current_player <- get_name player;
    print_endline ("player count: " ^ string_of_int (num_players game))

let rec get_index_helper board (prop : Property.t) acc =
  match board with
  | [] -> raise Not_found
  | h :: t -> if prop = h then acc else get_index_helper t prop (acc + 1)

let get_index (the_game : t) (prop : Property.t) =
  get_index_helper the_game.board prop 0

let rec get_prop_at_index_helper index (board : gameboard) =
  match board with
  | [] -> raise Not_found
  | h :: t -> if index = 0 then h else get_prop_at_index_helper (index - 1) t

let get_prop_at_index index the_game =
  get_prop_at_index_helper index the_game.board

let roll_dice () =
  Random.self_init ();
  2 + Random.int 5 + Random.int 5

let get_new_position player the_game =
  let moves = roll_dice () in
  print_endline ("\n" ^ get_name player ^ " rolled a " ^ string_of_int moves);
  player |> get_position |> get_index the_game |> ( + ) moves

let move_player player game =
  let old_pos = get_position player in
  let new_index = get_new_position player game mod 40 in
  let new_position = get_prop_at_index new_index game in
  change_pos player new_position;
  if new_index - get_index game old_pos < 0 then (
    print_endline "Passed go, collect $200";
    update_player_money player 200)

let get_players game = game.player_list

let find_player player_name plyr_lst =
  List.find (fun x -> get_name x = player_name) plyr_lst

(*TODO: Finish auction*)
(*
let rec auction highest_bidder prop bid_price player_list=
  let curr_player = match player_list with 
    | [] ->  auction highest_bidder prop bid_price (*Doesn't matter what happens here*)
    | h :: t ->  in
    if bid_price > 0 && h == highest_bidder
      let price = bid_price in
      if h.money >= price then (
        h.money <- h.money - price;
        h.properties <- prop :: h.properties;
        Property.set_owner prop h.name;
        check_monopoly h prop)
    else
      (*
      new_bid = ??? (*ask for bid*)
      if (new_bid > bid_price)
        let bid_price = new_bid (*set bid price to new bid*)
        highest_bidder = h (*Set highest bidder to current highest bidder*)
        auction highest_bidder prop bid_price (t @ [h]) (*Recursively call
        auction with new data*)
*) *)
