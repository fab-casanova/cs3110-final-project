open Yojson.Basic.Util
open Property
open Game

type space = {
  name : string;
  card_type : string;
  rents : int list;
  price : int;
  house_cost : int;
  buyable : bool;
}

type board = { board : space list }

let space_from_json json =
  {
    name = json |> member "name" |> to_string;
    card_type = json |> member "card_type" |> to_string;
    rents = json |> member "rents" |> to_list |> List.map to_int;
    price = json |> member "price" |> to_int;
    house_cost = json |> member "house_cost" |> to_int;
    buyable = json |> member "buyable" |> to_bool;
  }

let board_from_json json =
  { board = json |> member "board" |> to_list |> List.map space_from_json }

let create_card space =
  if space.buyable then
    create_buyable_card space.name space.card_type
      (Array.of_list space.rents)
      space.price space.house_cost
  else
    create_unbuyable_card space.name space.card_type
      (match space.rents with [] -> 0 | h :: t -> h)

let create_board f =
  let board = f |> Yojson.Basic.from_file |> board_from_json in
  create_gameboard (List.map (fun space -> create_card space) board.board)
