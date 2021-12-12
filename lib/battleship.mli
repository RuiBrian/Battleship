(** Five types of ships corresponding to the 2002 Hasbro version of the game. *)
type ship_type = Carrier | Battleship | Destroyer | Submarine | Patrol
[@@deriving equal, compare, yojson]

type board_cell = int * char [@@deriving equal, compare, yojson]
(** An [(int, char)] pair that corresponds to a position/cell on the board. *)

(** A ship can be oriented either horizontally or vertically. *)
type ship_orientation = Horizontal | Vertical
[@@deriving equal, compare, yojson]

type ship = {
  ship_type : ship_type;
  length : int;
  position : board_cell;
  orientation : ship_orientation;
}
[@@deriving equal, compare, yojson]
(** A ship consists of a [ship_type], its length, its position, and its orientation. *)

(** Describes the current state of a given cell. *)
type cell_state = Empty | Miss | Occupied of ship | Hit of ship
[@@deriving compare, yojson]

(** Summarizes the result of an attempted attack by a player. *)
type attack_result = Missed | Success | Repeat | Invalid [@@deriving equal]

type grid = (board_cell * cell_state) list [@@deriving yojson]
(** A grid is represented by an association list*)

type board = grid * ship list [@@deriving yojson]
(** A game board is represented by a grid and a list of sunk ships. *)

val create_board : board
(** [create_board] creates and returns a 10 x 10 board. *)

val is_game_over : board -> bool
(** [is_game_over b] checks if all ships on [b] have been destroyed. *)

val attack_cell : board -> board_cell -> board * attack_result
(** [attack_cell b c] attempts to attack [c] on [b]. Returns [Some(board)] on
success and [None] on failure. *)

val get_ship_cells : board_cell -> int -> ship_orientation -> board_cell list
(** [get_ship_cells c l o] gets the cells of a ship at [c] with length [l]
oriented according to [o]. *)

val place_ship :
  ship_type -> int -> board -> board_cell * ship_orientation -> board option
(** [place_ship s b (c, o)] attempts to place [s] on [b] at [c] oriented
according to [o]. Returns [Some(board)] on success and [None] on failure. *)

val ship_type_to_string : ship_type -> string
(** [ship_type_to_string s] converts [s] to a string representation. *)

val board_to_string : board -> string
(** [board_to_string b] converts [b] to a string representation for testing. *)

val to_primary_board_string : board -> string
(** [to_primary_board_string b] converts [b] to a string representation of a
primary board. *)

val to_tracking_board_string : board -> string
(** [to_tracking_board_string b] converts [b] to a string representation of a
tracking board. *)

val print_board : board -> unit
(** [print_board b] prints [b] to standard output. *)
