open Core

type ship_type = Carrier | Battleship | Destroyer | Submarine | Patrol
[@@deriving compare]

type ship = { ship_type : ship_type; length : int } [@@deriving compare]

type board_cell = int * char

type cell_state = Empty | Miss | Occupied of ship | Hit of ship | Sunk of ship
[@@deriving compare]

type ship_orientation = Horizontal | Vertical

type board = (board_cell, cell_state) List.Assoc.t

let carrier : ship = { ship_type = Carrier; length = 5 }

let battleship : ship = { ship_type = Battleship; length = 4 }

let destroyer : ship = { ship_type = Destroyer; length = 3 }

let submarine : ship = { ship_type = Submarine; length = 3 }

let patrol : ship = { ship_type = Patrol; length = 2 }

let rows = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]

let columns = [ 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J' ]

let create_board : board =
  let create_row acc cur_row =
    List.map columns ~f:(fun cur_column -> ((cur_row, cur_column), Empty))
    :: acc
  in
  List.fold rows ~init:[] ~f:create_row |> List.join

let is_game_over (b : board) : bool = failwith "unimplemented"

let attack_cell (b : board) (cell : board_cell) : board option =
  failwith "unimplemented"

let place_ship (s : ship) (b : board)
    (placement : board_cell * ship_orientation) : board option =
  failwith "unimplemented"

let board_to_string (b : board) : string = failwith "unimplemented"

let print_board (b : board) : unit = b |> board_to_string |> print_endline
