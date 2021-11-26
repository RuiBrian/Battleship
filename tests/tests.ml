open Core
open OUnit2
module B = Battleship

let test_create_board _ = failwith ""

let test_place_ship _ = failwith ""

let test_attack_cell _ = failwith ""

let test_is_game_over _ = failwith ""

let battleship_tests =
  "Battleship Tests"
  >: test_list
       [
         "Create a new game board" >:: test_create_board;
         "Place ships on board" >:: test_place_ship;
         "Attack a cell on the board" >:: test_attack_cell;
         "Check game status" >:: test_is_game_over;
       ]

let series = "Tests" >::: [ battleship_tests ]

let () = run_test_tt_main series
