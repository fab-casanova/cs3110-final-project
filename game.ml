open Property

type gameboard = { board : property list }

let roll_dice = 2 + Random.int 5 + Random.int 5
