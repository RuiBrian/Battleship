open Core

(** Five types of ships corresponding to the 2002 Hasbro version of the game. *)
type ship_type = Carrier | Battleship | Destroyer | Submarine | Patrol
[@@deriving compare]

type ship = { ship_type : ship_type; length : int }
(** A ship consists of a [ship_type] and its length.  *)

type board_cell = int * char
(** An [(int, char)] pair that corresponds to a position/cell on the board. *)

(** A cell can be empty or occupied by a [ship]. A boolean value indicates
whether or not the position has been struck. *)
type cell_state = Empty | Filled of (ship_type * bool) [@@deriving compare]

(** A ship can be oriented either horizontally or vertically. *)
type ship_orientation = Horizontal | Vertical

type board = (board_cell, cell_state) List.Assoc.t
(** A game board is represented by an association list. *)

val carrier : ship
(** Create a carrier ship. *)

val battleship : ship
(** Create a battleship. *)

val destroyer : ship
(** Create a destroyer. *)

val submarine : ship
(** Create a submarine. *)

val patrol : ship
(** Create a patrol boat. *)

val create_board : board
(** [create_board] creates and returns a 10 x 10 board. *)

val is_game_over : board -> bool
(** [is_game_over b] checks if all ships on [b] have been destroyed. *)

val attack_cell : board -> board_cell -> board option
(** [attack_cell b c] attempts to attack [c] on [b]. Returns [Some(board)] on
success and [None] on failure. *)

val place_ship : ship -> board -> board_cell * ship_orientation -> board option
(** [place_ship s b (c, o)] attempts to place [s] on [b] at [c] oriented
according to [o]. Returns [Some(board)] on success and [None] on failure. *)

val board_to_string : board -> string
(** [board_to_string b] converts [b] to a string representation for testing. *)

val print_board : board -> unit
(** [print_board b] prints [b] to standard output. *)
