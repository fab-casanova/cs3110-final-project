open Property

type t = {
  name : string;
  mutable properties : Property.t list;
  mutable money : int;
  mutable position : Property.t;
  mutable monopolies : Property.space_type list;
}

let create_player player_name start =
  {
    name = player_name;
    properties = [];
    money = 0;
    position = start;
    monopolies = [];
  }

let get_name player = player.name

let get_properties player = player.properties

let player_money player = player.money

let get_position player = player.position

let change_pos player new_pos = player.position <- new_pos

let update_player_money player update = player.money <- player.money + update

let add_property player prop = player.properties <- prop :: player.properties

let add_monopoly player property_type =
  player.monopolies <- property_type :: player.monopolies

let rec check_monopoly_helper player owned_properties property_type
    properties_required acc =
  match owned_properties with
  | [] -> ()
  | h :: t ->
      if get_type h = property_type then
        if acc + 1 = properties_required then add_monopoly player property_type
        else
          check_monopoly_helper player owned_properties property_type
            properties_required (acc + 1)
      else
        check_monopoly_helper player owned_properties property_type
          properties_required acc

let check_monopoly player new_property =
  let property_type = get_type new_property in
  let properties_required = num_for_monopoly new_property in
  let owned_properties = get_properties player in
  check_monopoly_helper player owned_properties property_type
    properties_required 0

let buy_property player prop =
  let price = Property.purchase_price prop in
  if player.money >= price then (
    player.money <- player.money - price;
    player.properties <- prop :: player.properties;
    Property.set_owner prop player.name;
    check_monopoly player prop)
