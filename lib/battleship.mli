(** Five types of ships corresponding to the 2002 Hasbro version of the game. *)
type ship_type = Carrier | Battleship | Destroyer | Submarine | Patrol
[@@deriving compare, yojson]

type board_cell = int * char [@@deriving equal, compare, yojson]
(** An [(int, char)] pair that corresponds to a position/cell on the board. *)

(** A ship can be oriented either horizontally or vertically. *)
type ship_orientation = Horizontal | Vertical [@@deriving compare, yojson]

type ship = {
  ship_type : ship_type;
  length : int;
  position : board_cell;
  orientation : ship_orientation;
}
[@@deriving compare, yojson]
(** A ship consists of a [ship_type], its length, its position, and its orientation. *)

(** Describes the current state of a given cell. *)
type cell_state = Empty | Miss | Occupied of ship | Hit of ship | Sunk of ship
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

val attack_cell : board -> board_cell -> board option * attack_result
(** [attack_cell b c] attempts to attack [c] on [b]. Returns [Some(board)] on
success and [None] on failure. *)

val place_ship :
  ship_type -> int -> board -> board_cell * ship_orientation -> board option
(** [place_ship s b (c, o)] attempts to place [s] on [b] at [c] oriented
according to [o]. Returns [Some(board)] on success and [None] on failure. *)

val board_to_string : board -> string
(** [board_to_string b] converts [b] to a string representation for testing. *)

val print_board : board -> unit
(** [print_board b] prints [b] to standard output. *)
