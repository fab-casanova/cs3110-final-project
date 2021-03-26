type t = {
  name : string;
  mutable properties : Property.t list;
  mutable money : int;
  mutable position : Property.t;
  mutable monopolies : Property.space_type list;
}

let get_name player = player.name

let get_properties player = player.properties

let player_money player = player.money

let get_position player = player.position

let change_pos player new_pos = player.position <- new_pos

let update_player_money player update = player.money <- player.money + update

let add_monopoly player property_type =
  player.monopolies <- property_type :: player.monopolies
