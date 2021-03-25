open Property

type player = {
  name : string;
  mutable properties : property list;
  mutable money : int;
  mutable position : property;
  mutable monopolies : property_type list;
}

let get_name plyr = plyr.name

let get_properties plyr = plyr.properties

let player_money plyr = plyr.money

let get_position plyr = plyr.position

let change_pos plyr new_pos = plyr.position <- new_pos

let update_player_money plyr update = plyr.money <- plyr.money + update
