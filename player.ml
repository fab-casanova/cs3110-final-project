open Property

type player = {
  name : string;
  properties : property list;
  money : int;
  position : string;
  monopolies : property_type list;
}
