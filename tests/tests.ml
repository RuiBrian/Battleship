open Core
open OUnit2
module B = Battleship

let test_create_board _ =
  assert_equal
    (" E(0,A)  E(0,B)  E(0,C)  E(0,D)  E(0,E) "
   ^ " E(0,F)  E(0,G)  E(0,H)  E(0,I)  E(0,J) \n"
   ^ " E(1,A)  E(1,B)  E(1,C)  E(1,D)  E(1,E) "
   ^ " E(1,F)  E(1,G)  E(1,H)  E(1,I)  E(1,J) \n"
   ^ " E(2,A)  E(2,B)  E(2,C)  E(2,D)  E(2,E) "
   ^ " E(2,F)  E(2,G)  E(2,H)  E(2,I)  E(2,J) \n"
   ^ " E(3,A)  E(3,B)  E(3,C)  E(3,D)  E(3,E) "
   ^ " E(3,F)  E(3,G)  E(3,H)  E(3,I)  E(3,J) \n"
   ^ " E(4,A)  E(4,B)  E(4,C)  E(4,D)  E(4,E) "
   ^ " E(4,F)  E(4,G)  E(4,H)  E(4,I)  E(4,J) \n"
   ^ " E(5,A)  E(5,B)  E(5,C)  E(5,D)  E(5,E) "
   ^ " E(5,F)  E(5,G)  E(5,H)  E(5,I)  E(5,J) \n"
   ^ " E(6,A)  E(6,B)  E(6,C)  E(6,D)  E(6,E) "
   ^ " E(6,F)  E(6,G)  E(6,H)  E(6,I)  E(6,J) \n"
   ^ " E(7,A)  E(7,B)  E(7,C)  E(7,D)  E(7,E) "
   ^ " E(7,F)  E(7,G)  E(7,H)  E(7,I)  E(7,J) \n"
   ^ " E(8,A)  E(8,B)  E(8,C)  E(8,D)  E(8,E) "
   ^ " E(8,F)  E(8,G)  E(8,H)  E(8,I)  E(8,J) \n"
   ^ " E(9,A)  E(9,B)  E(9,C)  E(9,D)  E(9,E) "
   ^ " E(9,F)  E(9,G)  E(9,H)  E(9,I)  E(9,J) \n")
  @@ B.board_to_string B.create_board

let test_place_ship _ =
  let test_board =
    match B.place_ship Carrier 5 B.create_board ((0, 'A'), B.Horizontal) with
    | Some board -> board
    | None -> B.create_board
  in
  assert_equal
    (" O(0,A)  O(0,B)  O(0,C)  O(0,D)  O(0,E) "
   ^ " E(0,F)  E(0,G)  E(0,H)  E(0,I)  E(0,J) \n"
   ^ " E(1,A)  E(1,B)  E(1,C)  E(1,D)  E(1,E) "
   ^ " E(1,F)  E(1,G)  E(1,H)  E(1,I)  E(1,J) \n"
   ^ " E(2,A)  E(2,B)  E(2,C)  E(2,D)  E(2,E) "
   ^ " E(2,F)  E(2,G)  E(2,H)  E(2,I)  E(2,J) \n"
   ^ " E(3,A)  E(3,B)  E(3,C)  E(3,D)  E(3,E) "
   ^ " E(3,F)  E(3,G)  E(3,H)  E(3,I)  E(3,J) \n"
   ^ " E(4,A)  E(4,B)  E(4,C)  E(4,D)  E(4,E) "
   ^ " E(4,F)  E(4,G)  E(4,H)  E(4,I)  E(4,J) \n"
   ^ " E(5,A)  E(5,B)  E(5,C)  E(5,D)  E(5,E) "
   ^ " E(5,F)  E(5,G)  E(5,H)  E(5,I)  E(5,J) \n"
   ^ " E(6,A)  E(6,B)  E(6,C)  E(6,D)  E(6,E) "
   ^ " E(6,F)  E(6,G)  E(6,H)  E(6,I)  E(6,J) \n"
   ^ " E(7,A)  E(7,B)  E(7,C)  E(7,D)  E(7,E) "
   ^ " E(7,F)  E(7,G)  E(7,H)  E(7,I)  E(7,J) \n"
   ^ " E(8,A)  E(8,B)  E(8,C)  E(8,D)  E(8,E) "
   ^ " E(8,F)  E(8,G)  E(8,H)  E(8,I)  E(8,J) \n"
   ^ " E(9,A)  E(9,B)  E(9,C)  E(9,D)  E(9,E) "
   ^ " E(9,F)  E(9,G)  E(9,H)  E(9,I)  E(9,J) \n")
  @@ B.board_to_string test_board;
  assert_equal None
  @@ B.place_ship Battleship 4 B.create_board ((0, 'M'), B.Horizontal);
  let test_board =
    match B.place_ship Patrol 2 B.create_board ((0, 'A'), B.Vertical) with
    | Some board -> board
    | None -> B.create_board
  in
  assert_equal
    (" O(0,A)  E(0,B)  E(0,C)  E(0,D)  E(0,E) "
   ^ " E(0,F)  E(0,G)  E(0,H)  E(0,I)  E(0,J) \n"
   ^ " O(1,A)  E(1,B)  E(1,C)  E(1,D)  E(1,E) "
   ^ " E(1,F)  E(1,G)  E(1,H)  E(1,I)  E(1,J) \n"
   ^ " E(2,A)  E(2,B)  E(2,C)  E(2,D)  E(2,E) "
   ^ " E(2,F)  E(2,G)  E(2,H)  E(2,I)  E(2,J) \n"
   ^ " E(3,A)  E(3,B)  E(3,C)  E(3,D)  E(3,E) "
   ^ " E(3,F)  E(3,G)  E(3,H)  E(3,I)  E(3,J) \n"
   ^ " E(4,A)  E(4,B)  E(4,C)  E(4,D)  E(4,E) "
   ^ " E(4,F)  E(4,G)  E(4,H)  E(4,I)  E(4,J) \n"
   ^ " E(5,A)  E(5,B)  E(5,C)  E(5,D)  E(5,E) "
   ^ " E(5,F)  E(5,G)  E(5,H)  E(5,I)  E(5,J) \n"
   ^ " E(6,A)  E(6,B)  E(6,C)  E(6,D)  E(6,E) "
   ^ " E(6,F)  E(6,G)  E(6,H)  E(6,I)  E(6,J) \n"
   ^ " E(7,A)  E(7,B)  E(7,C)  E(7,D)  E(7,E) "
   ^ " E(7,F)  E(7,G)  E(7,H)  E(7,I)  E(7,J) \n"
   ^ " E(8,A)  E(8,B)  E(8,C)  E(8,D)  E(8,E) "
   ^ " E(8,F)  E(8,G)  E(8,H)  E(8,I)  E(8,J) \n"
   ^ " E(9,A)  E(9,B)  E(9,C)  E(9,D)  E(9,E) "
   ^ " E(9,F)  E(9,G)  E(9,H)  E(9,I)  E(9,J) \n")
  @@ B.board_to_string test_board;
  assert_equal None
  @@ B.place_ship Battleship 4 test_board ((0, 'A'), B.Vertical)

let test_attack_cell _ =
  let test_board =
    match B.place_ship Patrol 2 B.create_board ((0, 'A'), B.Horizontal) with
    | Some board -> board
    | None -> B.create_board
  in
  let result_board, attack_result = B.attack_cell test_board (0, 'A') in
  assert_equal
    (" H(0,A)  O(0,B)  E(0,C)  E(0,D)  E(0,E) "
   ^ " E(0,F)  E(0,G)  E(0,H)  E(0,I)  E(0,J) \n"
   ^ " E(1,A)  E(1,B)  E(1,C)  E(1,D)  E(1,E) "
   ^ " E(1,F)  E(1,G)  E(1,H)  E(1,I)  E(1,J) \n"
   ^ " E(2,A)  E(2,B)  E(2,C)  E(2,D)  E(2,E) "
   ^ " E(2,F)  E(2,G)  E(2,H)  E(2,I)  E(2,J) \n"
   ^ " E(3,A)  E(3,B)  E(3,C)  E(3,D)  E(3,E) "
   ^ " E(3,F)  E(3,G)  E(3,H)  E(3,I)  E(3,J) \n"
   ^ " E(4,A)  E(4,B)  E(4,C)  E(4,D)  E(4,E) "
   ^ " E(4,F)  E(4,G)  E(4,H)  E(4,I)  E(4,J) \n"
   ^ " E(5,A)  E(5,B)  E(5,C)  E(5,D)  E(5,E) "
   ^ " E(5,F)  E(5,G)  E(5,H)  E(5,I)  E(5,J) \n"
   ^ " E(6,A)  E(6,B)  E(6,C)  E(6,D)  E(6,E) "
   ^ " E(6,F)  E(6,G)  E(6,H)  E(6,I)  E(6,J) \n"
   ^ " E(7,A)  E(7,B)  E(7,C)  E(7,D)  E(7,E) "
   ^ " E(7,F)  E(7,G)  E(7,H)  E(7,I)  E(7,J) \n"
   ^ " E(8,A)  E(8,B)  E(8,C)  E(8,D)  E(8,E) "
   ^ " E(8,F)  E(8,G)  E(8,H)  E(8,I)  E(8,J) \n"
   ^ " E(9,A)  E(9,B)  E(9,C)  E(9,D)  E(9,E) "
   ^ " E(9,F)  E(9,G)  E(9,H)  E(9,I)  E(9,J) \n")
  @@ B.board_to_string result_board;
  assert_equal B.Success attack_result;
  let _, sunk_ships = result_board in
  assert_equal 0 @@ List.length sunk_ships;
  let result_board, attack_result = B.attack_cell result_board (0, 'B') in
  assert_equal
    (" H(0,A)  H(0,B)  E(0,C)  E(0,D)  E(0,E) "
   ^ " E(0,F)  E(0,G)  E(0,H)  E(0,I)  E(0,J) \n"
   ^ " E(1,A)  E(1,B)  E(1,C)  E(1,D)  E(1,E) "
   ^ " E(1,F)  E(1,G)  E(1,H)  E(1,I)  E(1,J) \n"
   ^ " E(2,A)  E(2,B)  E(2,C)  E(2,D)  E(2,E) "
   ^ " E(2,F)  E(2,G)  E(2,H)  E(2,I)  E(2,J) \n"
   ^ " E(3,A)  E(3,B)  E(3,C)  E(3,D)  E(3,E) "
   ^ " E(3,F)  E(3,G)  E(3,H)  E(3,I)  E(3,J) \n"
   ^ " E(4,A)  E(4,B)  E(4,C)  E(4,D)  E(4,E) "
   ^ " E(4,F)  E(4,G)  E(4,H)  E(4,I)  E(4,J) \n"
   ^ " E(5,A)  E(5,B)  E(5,C)  E(5,D)  E(5,E) "
   ^ " E(5,F)  E(5,G)  E(5,H)  E(5,I)  E(5,J) \n"
   ^ " E(6,A)  E(6,B)  E(6,C)  E(6,D)  E(6,E) "
   ^ " E(6,F)  E(6,G)  E(6,H)  E(6,I)  E(6,J) \n"
   ^ " E(7,A)  E(7,B)  E(7,C)  E(7,D)  E(7,E) "
   ^ " E(7,F)  E(7,G)  E(7,H)  E(7,I)  E(7,J) \n"
   ^ " E(8,A)  E(8,B)  E(8,C)  E(8,D)  E(8,E) "
   ^ " E(8,F)  E(8,G)  E(8,H)  E(8,I)  E(8,J) \n"
   ^ " E(9,A)  E(9,B)  E(9,C)  E(9,D)  E(9,E) "
   ^ " E(9,F)  E(9,G)  E(9,H)  E(9,I)  E(9,J) \n")
  @@ B.board_to_string result_board;
  assert_equal B.Success attack_result;
  let _, sunk_ships = result_board in
  assert_equal
    [
      {
        B.ship_type = Patrol;
        length = 2;
        position = (0, 'A');
        orientation = Horizontal;
      };
    ]
    sunk_ships;
  let result_board, attack_result = B.attack_cell result_board (0, 'C') in
  assert_equal
    (" H(0,A)  H(0,B)  M(0,C)  E(0,D)  E(0,E) "
   ^ " E(0,F)  E(0,G)  E(0,H)  E(0,I)  E(0,J) \n"
   ^ " E(1,A)  E(1,B)  E(1,C)  E(1,D)  E(1,E) "
   ^ " E(1,F)  E(1,G)  E(1,H)  E(1,I)  E(1,J) \n"
   ^ " E(2,A)  E(2,B)  E(2,C)  E(2,D)  E(2,E) "
   ^ " E(2,F)  E(2,G)  E(2,H)  E(2,I)  E(2,J) \n"
   ^ " E(3,A)  E(3,B)  E(3,C)  E(3,D)  E(3,E) "
   ^ " E(3,F)  E(3,G)  E(3,H)  E(3,I)  E(3,J) \n"
   ^ " E(4,A)  E(4,B)  E(4,C)  E(4,D)  E(4,E) "
   ^ " E(4,F)  E(4,G)  E(4,H)  E(4,I)  E(4,J) \n"
   ^ " E(5,A)  E(5,B)  E(5,C)  E(5,D)  E(5,E) "
   ^ " E(5,F)  E(5,G)  E(5,H)  E(5,I)  E(5,J) \n"
   ^ " E(6,A)  E(6,B)  E(6,C)  E(6,D)  E(6,E) "
   ^ " E(6,F)  E(6,G)  E(6,H)  E(6,I)  E(6,J) \n"
   ^ " E(7,A)  E(7,B)  E(7,C)  E(7,D)  E(7,E) "
   ^ " E(7,F)  E(7,G)  E(7,H)  E(7,I)  E(7,J) \n"
   ^ " E(8,A)  E(8,B)  E(8,C)  E(8,D)  E(8,E) "
   ^ " E(8,F)  E(8,G)  E(8,H)  E(8,I)  E(8,J) \n"
   ^ " E(9,A)  E(9,B)  E(9,C)  E(9,D)  E(9,E) "
   ^ " E(9,F)  E(9,G)  E(9,H)  E(9,I)  E(9,J) \n")
  @@ B.board_to_string result_board;
  assert_equal B.Missed attack_result;
  let _, attack_result = B.attack_cell result_board (11, 'B') in
  assert_equal B.Invalid attack_result;
  let _, attack_result = B.attack_cell result_board (0, 'A') in
  assert_equal B.Repeat attack_result;
  let _, attack_result = B.attack_cell result_board (0, 'C') in
  assert_equal B.Repeat attack_result

let test_is_game_over _ = assert_equal false @@ B.is_game_over B.create_board

let test_ship_type_to_string _ =
  assert_equal "Carrier" @@ B.ship_type_to_string B.Carrier;
  assert_equal "Battleship" @@ B.ship_type_to_string B.Battleship;
  assert_equal "Destroyer" @@ B.ship_type_to_string B.Destroyer;
  assert_equal "Submarine" @@ B.ship_type_to_string B.Submarine;
  assert_equal "Patrol" @@ B.ship_type_to_string B.Patrol

let test_get_remaining_ships _ =
  assert_equal [ B.Carrier; B.Battleship; B.Destroyer; B.Submarine; B.Patrol ]
  @@ B.get_remaining_ships B.create_board;
  let sunk_ships =
    [
      {
        B.ship_type = B.Carrier;
        length = 5;
        position = (1, 'A');
        orientation = Horizontal;
      };
      {
        B.ship_type = B.Patrol;
        length = 2;
        position = (2, 'D');
        orientation = Vertical;
      };
    ]
  in
  let grid, _ = B.create_board in
  let test_board = (grid, sunk_ships) in
  assert_equal [ B.Battleship; B.Destroyer; B.Submarine ]
  @@ B.get_remaining_ships test_board

let test_get_primary_board _ =
  let test_board =
    match B.place_ship Patrol 2 B.create_board ((0, 'A'), B.Horizontal) with
    | Some board -> board
    | None -> B.create_board
  in
  assert_equal
    "  | A | B | C | D | E | F | G | H | I | J \n\
     ------------------------------------------\n\
     0 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     1 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     2 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     3 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     4 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     5 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     6 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     7 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     8 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     9 |   |   |   |   |   |   |   |   |   |   "
  @@ B.to_primary_board_string test_board;
  let result_board, _ = B.attack_cell test_board (0, 'A') in
  assert_equal
    "  | A | B | C | D | E | F | G | H | I | J \n\
     ------------------------------------------\n\
     0 | H |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     1 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     2 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     3 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     4 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     5 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     6 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     7 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     8 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     9 |   |   |   |   |   |   |   |   |   |   "
  @@ B.to_primary_board_string result_board;
  let result_board, _ = B.attack_cell result_board (1, 'B') in
  assert_equal
    "  | A | B | C | D | E | F | G | H | I | J \n\
     ------------------------------------------\n\
     0 | H |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     1 |   | M |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     2 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     3 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     4 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     5 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     6 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     7 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     8 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     9 |   |   |   |   |   |   |   |   |   |   "
  @@ B.to_primary_board_string result_board

let test_get_tracking_board _ =
  let test_board =
    match B.place_ship Patrol 2 B.create_board ((0, 'A'), B.Horizontal) with
    | Some board -> board
    | None -> B.create_board
  in
  assert_equal
    "  | A | B | C | D | E | F | G | H | I | J \n\
     ------------------------------------------\n\
     0 | O | O |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     1 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     2 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     3 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     4 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     5 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     6 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     7 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     8 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     9 |   |   |   |   |   |   |   |   |   |   "
  @@ B.to_tracking_board_string test_board;
  let result_board, _ = B.attack_cell test_board (0, 'A') in
  assert_equal
    "  | A | B | C | D | E | F | G | H | I | J \n\
     ------------------------------------------\n\
     0 | H | O |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     1 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     2 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     3 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     4 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     5 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     6 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     7 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     8 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     9 |   |   |   |   |   |   |   |   |   |   "
  @@ B.to_tracking_board_string result_board;
  let result_board, _ = B.attack_cell result_board (1, 'B') in
  assert_equal
    "  | A | B | C | D | E | F | G | H | I | J \n\
     ------------------------------------------\n\
     0 | H | O |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     1 |   | M |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     2 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     3 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     4 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     5 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     6 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     7 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     8 |   |   |   |   |   |   |   |   |   |   \n\
     ------------------------------------------\n\
     9 |   |   |   |   |   |   |   |   |   |   "
  @@ B.to_tracking_board_string result_board

let test_valid_request _ =
  assert_equal true @@ B.valid_request "Carrier" "Horizontal" [ "1"; "A" ];
  assert_equal true @@ B.valid_request "carrier" "Horizontal" [ "1"; "A" ];
  assert_equal true @@ B.valid_request "Carrier" "horizontal" [ "1"; "A" ];
  assert_equal true @@ B.valid_request "Carrier" "Vertical" [ "1"; "A" ];
  assert_equal true @@ B.valid_request "Carrier" "vertical" [ "1"; "A" ];
  assert_equal true @@ B.valid_request "Battleship" "horizontal" [ "1"; "A" ];
  assert_equal true @@ B.valid_request "battleship" "horizontal" [ "1"; "A" ];
  assert_equal true @@ B.valid_request "Destroyer" "horizontal" [ "1"; "A" ];
  assert_equal true @@ B.valid_request "destroyer" "horizontal" [ "1"; "A" ];
  assert_equal true @@ B.valid_request "Submarine" "horizontal" [ "1"; "A" ];
  assert_equal true @@ B.valid_request "submarine" "horizontal" [ "1"; "A" ];
  assert_equal true @@ B.valid_request "Patrol" "horizontal" [ "1"; "A" ];
  assert_equal true @@ B.valid_request "patrol" "horizontal" [ "1"; "A" ];
  assert_equal false @@ B.valid_request "Carrier" "Horizontal" [ "1"; "asdf" ];
  assert_equal false @@ B.valid_request "Carrier" "Horizontal" [ "1"; "Z" ];
  assert_equal false @@ B.valid_request "asdf" "Horizontal" [ "1" ];
  assert_equal false @@ B.valid_request "Carrier" "asdf" [ "1" ]

let battleship_tests =
  "Battleship Tests"
  >: test_list
       [
         "Create a new game board" >:: test_create_board;
         "Place ships on board" >:: test_place_ship;
         "Attack a cell on the board" >:: test_attack_cell;
         "Check game status" >:: test_is_game_over;
         "Ship type to string" >:: test_ship_type_to_string;
         "Get unsunk ships" >:: test_get_remaining_ships;
         "Get primary board" >:: test_get_primary_board;
         "Get tracking board" >:: test_get_tracking_board;
         "Check valid request" >:: test_valid_request;
       ]

let series = "Tests" >::: [ battleship_tests ]
let () = run_test_tt_main series
