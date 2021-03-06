open Core

[@@@coverage off]

type ship_type = Carrier | Battleship | Destroyer | Submarine | Patrol
[@@deriving equal, compare, yojson]

type board_cell = int * char [@@deriving equal, compare, yojson]

type ship_orientation = Horizontal | Vertical
[@@deriving equal, compare, yojson]

type ship = {
  ship_type : ship_type;
  length : int;
  position : board_cell;
  orientation : ship_orientation;
}
[@@deriving equal, compare, yojson]

type cell_state = Empty | Miss | Occupied of ship | Hit of ship
[@@deriving compare, yojson]

type attack_result = Missed | Success | Repeat | Invalid [@@deriving equal]
type grid = (board_cell * cell_state) list [@@deriving yojson]
type board = grid * ship list [@@deriving yojson]

[@@@coverage on]

let rows = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
let columns = [ 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J' ]

let create_board : board =
  let create_row acc cur_row =
    List.map columns ~f:(fun cur_column -> ((cur_row, cur_column), Empty))
    :: acc
  in
  let grid = List.fold (List.rev rows) ~init:[] ~f:create_row |> List.join in
  (grid, [])

let is_game_over (b : board) : bool =
  match b with _, sunk_ships -> List.length sunk_ships = 5

let valid_request (ship_type : string) (orientation : string)
    (board_cell_str_list : string list) : bool =
  let validate_type =
    match ship_type with
    | "Carrier" | "carrier" | "Battleship" | "battleship" | "Destroyer"
    | "destroyer" | "Submarine" | "submarine" | "Patrol" | "patrol" ->
        true
    | _ -> false
  in
  let validate_orientation =
    match orientation with
    | "Horizontal" | "horizontal" | "Vertical" | "vertical" -> true
    | _ -> false
  in
  let validate_cell =
    if
      List.length board_cell_str_list = 2
      && Str.string_match (Str.regexp "[0-9]+$")
           (List.nth_exn board_cell_str_list 0)
           0
      && Str.string_match (Str.regexp "[A-J]+$")
           (List.nth_exn board_cell_str_list 1)
           0
    then true
    else false
  in
  validate_type && validate_orientation && validate_cell

let get_ship_cells (start_cell : board_cell) (l : int)
    (orientation : ship_orientation) : board_cell list =
  let x, y = start_cell in
  match orientation with
  | Vertical -> List.map (List.range x (x + l)) ~f:(fun cur -> (cur, y))
  | Horizontal ->
      List.map
        (List.range (Char.to_int y) (Char.to_int y + l))
        ~f:(fun cur -> (x, Char.of_int_exn cur))

let attack_cell (b : board) (cell : board_cell) : board * attack_result =
  let grid, sunk_ships = b in
  match List.Assoc.find grid ~equal:equal_board_cell cell with
  | Some Empty ->
      let temp = List.Assoc.remove grid ~equal:equal_board_cell cell in
      let new_grid = List.Assoc.add temp ~equal:equal_board_cell cell Miss in
      let new_board = (new_grid, sunk_ships) in
      (new_board, Missed)
  | Some Miss | Some (Hit _) -> (b, Repeat)
  | Some (Occupied ship) ->
      let temp = List.Assoc.remove grid ~equal:equal_board_cell cell in
      let new_grid =
        List.Assoc.add temp ~equal:equal_board_cell cell (Hit ship)
      in
      let ship_cells =
        get_ship_cells ship.position ship.length ship.orientation
      in
      let ship_sunk =
        List.fold ship_cells ~init:true ~f:(fun acc cur_cell ->
            match List.Assoc.find new_grid ~equal:equal_board_cell cur_cell with
            | Some (Hit _) -> acc && true
            | Some _ | None -> false)
      in
      let new_board =
        if ship_sunk then (new_grid, ship :: sunk_ships)
        else (new_grid, sunk_ships)
      in
      (new_board, Success)
  | None -> (b, Invalid)

let place_ship (s : ship_type) (l : int) (b : board)
    (placement : board_cell * ship_orientation) : board option =
  let valid_position (x : int) (y : char) (length : int)
      (orientation : ship_orientation) : bool =
    match orientation with
    | Vertical ->
        Int.between x ~low:0 ~high:9
        && Int.between (x + length - 1) ~low:0 ~high:9
        && Char.between y ~low:'A' ~high:'J'
    | Horizontal ->
        Int.between x ~low:0 ~high:9
        && Char.between y ~low:'A' ~high:'J'
        && Char.between
             (Char.of_int_exn (Char.to_int y + length - 1))
             ~low:'A' ~high:'J'
  in

  let cells_are_occupied (cells : board_cell list) (g : grid) : bool =
    let aux acc cur =
      match List.Assoc.find g ~equal:equal_board_cell cur with
      | Some Empty | Some Miss | None -> acc
      | Some _ -> acc || true
    in
    List.fold cells ~init:false ~f:aux
  in

  let place_ship_at_cells (cells : board_cell list) (g : grid)
      (orientation : ship_orientation) : grid =
    List.fold (List.rev cells) ~init:g ~f:(fun acc cur ->
        let temp = List.Assoc.remove acc ~equal:equal_board_cell cur in
        List.Assoc.add temp ~equal:equal_board_cell cur
          (Occupied
             {
               ship_type = s;
               length = l;
               position = List.hd_exn cells;
               orientation;
             }))
  in

  let grid, sunk_ships = b in
  match placement with
  | (x, y), orientation ->
      if valid_position x y l orientation then
        let ship_cells = get_ship_cells (x, y) l orientation in
        match cells_are_occupied ship_cells grid with
        | false ->
            Some (place_ship_at_cells ship_cells grid orientation, sunk_ships)
        | true -> None
      else None

let get_remaining_ships (b : board) : ship_type list =
  let all_ships = [ Carrier; Battleship; Destroyer; Submarine; Patrol ] in
  let _, sunk_ships = b in
  List.fold sunk_ships ~init:all_ships ~f:(fun acc cur ->
      match List.find acc ~f:(fun s -> equal_ship_type s cur.ship_type) with
      | Some _ ->
          List.filter acc ~f:(fun s -> not @@ equal_ship_type s cur.ship_type)
      | None -> failwith "Unknown ship type" [@coverage off])

(******** Testing/IO/Util functions ********)

let ship_type_to_string (s : ship_type) : string =
  match s with
  | Carrier -> "Carrier"
  | Battleship -> "Battleship"
  | Destroyer -> "Destroyer"
  | Submarine -> "Submarine"
  | Patrol -> "Patrol"

let board_to_string (b : board) : string =
  let sort_board b =
    let grid, sunk_ships = b in
    let lex_cmp ((x, y), _) ((x', y'), _) =
      let compare_row = compare x x' in
      if compare_row <> 0 then compare_row else Char.compare y y'
    in
    (List.sort ~compare:lex_cmp grid, sunk_ships)
  in

  let sorted_board = sort_board b in

  let grid, _ = sorted_board in

  let check_for_new_line col = match col with 'J' -> "\n" | _ -> "" in

  let aux acc cur_cell =
    match cur_cell with
    | (x, y), Empty -> sprintf "%s E(%i,%c) %s" acc x y (check_for_new_line y)
    | (x, y), Miss -> sprintf "%s M(%i,%c) %s" acc x y (check_for_new_line y)
    | (x, y), Occupied _ ->
        sprintf "%s O(%i,%c) %s" acc x y (check_for_new_line y)
    | (x, y), Hit _ -> sprintf "%s H(%i,%c) %s" acc x y (check_for_new_line y)
  in

  grid |> List.fold ~init:"" ~f:aux

let to_primary_board_string (b : board) : string =
  let sort_board b =
    let grid, sunk_ships = b in
    let lex_cmp ((x, y), _) ((x', y'), _) =
      let compare_row = compare x x' in
      if compare_row <> 0 then compare_row else Char.compare y y'
    in
    (List.sort ~compare:lex_cmp grid, sunk_ships)
  in

  let top_row = "  | A | B | C | D | E | F | G | H | I | J \n" in
  let row_separator = "------------------------------------------" in

  let sorted_board = sort_board b in

  let grid, _ = sorted_board in

  let check_for_new_line row col =
    if row = 9 && Char.(col = 'J') then ""
    else
      match col with
      | 'J' -> "\n" ^ row_separator ^ "\n" ^ string_of_int (row + 1) ^ " |"
      | _ -> "|"
  in

  let aux acc cur_cell =
    match cur_cell with
    | (x, y), Empty -> sprintf "%s   %s" acc (check_for_new_line x y)
    | (x, y), Miss -> sprintf "%s M %s" acc (check_for_new_line x y)
    | (x, y), Occupied _ -> sprintf "%s   %s" acc (check_for_new_line x y)
    | (x, y), Hit _ -> sprintf "%s H %s" acc (check_for_new_line x y)
  in

  grid |> List.fold ~init:(top_row ^ row_separator ^ "\n0 |") ~f:aux

let to_tracking_board_string (b : board) : string =
  let sort_board b =
    let grid, sunk_ships = b in
    let lex_cmp ((x, y), _) ((x', y'), _) =
      let compare_row = compare x x' in
      if compare_row <> 0 then compare_row else Char.compare y y'
    in
    (List.sort ~compare:lex_cmp grid, sunk_ships)
  in

  let top_row = "  | A | B | C | D | E | F | G | H | I | J \n" in
  let row_separator = "------------------------------------------" in

  let sorted_board = sort_board b in

  let grid, _ = sorted_board in

  let check_for_new_line row col =
    if row = 9 && Char.(col = 'J') then ""
    else
      match col with
      | 'J' -> "\n" ^ row_separator ^ "\n" ^ string_of_int (row + 1) ^ " |"
      | _ -> "|"
  in

  let aux acc cur_cell =
    match cur_cell with
    | (x, y), Empty -> sprintf "%s   %s" acc (check_for_new_line x y)
    | (x, y), Miss -> sprintf "%s M %s" acc (check_for_new_line x y)
    | (x, y), Occupied _ -> sprintf "%s O %s" acc (check_for_new_line x y)
    | (x, y), Hit _ -> sprintf "%s H %s" acc (check_for_new_line x y)
  in

  grid |> List.fold ~init:(top_row ^ row_separator ^ "\n0 |") ~f:aux

let print_board (b : board) : unit = b |> board_to_string |> print_endline
  [@@coverage off]
