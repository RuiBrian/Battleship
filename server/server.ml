open Core
open Battleship

exception Invalid_cell
exception Invalid_ship
exception Invalid_orientation

let num_connections = ref 0

type place_ship_request_obj = {
  cell : string;
  ship_type : string;
  orientation : string;
}
[@@deriving yojson]

type attack_cell_request_obj = { cell : string } [@@deriving yojson]

type state = {
  mutable player_one_board : board;
  mutable player_two_board : board;
  mutable player_one_ships_to_place : ship_type list;
  mutable player_two_ships_to_place : ship_type list;
  mutable player_one_turn : bool;
  mutable winner : int option;
}

let game_state =
  {
    player_one_board = create_board;
    player_two_board = create_board;
    player_one_ships_to_place =
      [ Carrier; Battleship; Destroyer; Submarine; Patrol ];
    player_two_ships_to_place =
      [ Carrier; Battleship; Destroyer; Submarine; Patrol ];
    player_one_turn = true;
    winner = None;
  }

let homepage_handler (_ : Dream.request) : Dream.response Lwt.t =
  let header = "<html>\n<body>\n<h1>Battleship.</h1>\n</body>\n</html>" in
  Dream.html header

let place_ship_handler (request : Dream.request) : Dream.response Lwt.t =
  let player = int_of_string @@ Dream.param "player" request in
  let%lwt body = Dream.body request in

  let place_ship_request_obj =
    body |> Yojson.Safe.from_string |> place_ship_request_obj_of_yojson
  in

  let place_ship_request_json =
    match place_ship_request_obj with
    | Ok res -> res
    | Error msg -> failwith msg
  in
  let board_cell_string_list =
    String.split ~on:' ' place_ship_request_json.cell
  in

  if
    not
    @@ valid_request place_ship_request_json.ship_type
         place_ship_request_json.orientation board_cell_string_list
  then Dream.respond ~status:`Bad_Request "Invalid placement"
  else
    let ship_type =
      match place_ship_request_json.ship_type with
      | "Carrier" | "carrier" -> Carrier
      | "Battleship" | "battleship" -> Battleship
      | "Destroyer" | "destroyer" -> Destroyer
      | "Submarine" | "submarine" -> Submarine
      | "Patrol" | "patrol" -> Patrol
      | _ -> raise Invalid_ship
    in

    let orientation =
      match place_ship_request_json.orientation with
      | "Horizontal" | "horizontal" -> Horizontal
      | "Vertical" | "vertical" -> Vertical
      | _ -> raise Invalid_orientation
    in

    let board_cell =
      match board_cell_string_list with
      | [ row; col ] -> (int_of_string row, Char.of_string col)
      | _ -> raise Invalid_cell
    in

    let length =
      match ship_type with
      | Carrier -> 5
      | Battleship -> 4
      | Destroyer -> 3
      | Submarine -> 3
      | Patrol -> 2
    in

    match player with
    | 1 -> (
        match
          List.find game_state.player_one_ships_to_place ~f:(fun x ->
              compare_ship_type x ship_type = 0)
        with
        | Some _ -> (
            match
              place_ship ship_type length game_state.player_one_board
                (board_cell, orientation)
            with
            | Some new_board ->
                game_state.player_one_board <- new_board;
                game_state.player_one_ships_to_place <-
                  List.filter game_state.player_one_ships_to_place ~f:(fun x ->
                      compare_ship_type x ship_type <> 0);
                Dream.respond @@ board_to_string game_state.player_one_board
            | None -> Dream.respond ~status:`Bad_Request "Invalid placement")
        | None -> Dream.respond ~status:`Bad_Request "Ship already placed")
    | 2 -> (
        match
          List.find game_state.player_two_ships_to_place ~f:(fun x ->
              compare_ship_type x ship_type = 0)
        with
        | Some _ -> (
            match
              place_ship ship_type length game_state.player_two_board
                (board_cell, orientation)
            with
            | Some new_board ->
                game_state.player_two_board <- new_board;
                game_state.player_two_ships_to_place <-
                  List.filter game_state.player_two_ships_to_place ~f:(fun x ->
                      compare_ship_type x ship_type <> 0);
                Dream.respond @@ board_to_string game_state.player_two_board
            | None -> Dream.respond ~status:`Bad_Request "Invalid placement")
        | None -> Dream.respond ~status:`Bad_Request "Ship already placed")
    | _ -> failwith "Invalid player"

let attack_cell_handler (request : Dream.request) : Dream.response Lwt.t =
  let player = int_of_string @@ Dream.param "player" request in
  let%lwt body = Dream.body request in

  let attack_cell_request_obj =
    body |> Yojson.Safe.from_string |> attack_cell_request_obj_of_yojson
  in

  let attack_cell_request_json =
    match attack_cell_request_obj with
    | Ok res -> res
    | Error msg -> failwith msg
  in

  let board_cell_str_list =
    String.split ~on:' ' attack_cell_request_json.cell
  in

  if
    List.length board_cell_str_list <> 2
    || not
       @@ Str.string_match (Str.regexp "[0-9]+$")
            (List.nth_exn board_cell_str_list 0)
            0
    || not
       @@ Str.string_match (Str.regexp "[A-J]+$")
            (List.nth_exn board_cell_str_list 1)
            0
  then Dream.respond ~status:`Bad_Request "Invalid cell"
  else
    let target_cell =
      match board_cell_str_list with
      | [ row; col ] -> (int_of_string row, Char.of_string col)
      | _ -> failwith "Invalid cell"
    in

    let current_turn = if game_state.player_one_turn then 1 else 2 in

    if player <> current_turn then
      Dream.respond ~status:`Bad_Request "Not your turn!"
    else
      match player with
      | 1 -> (
          match attack_cell game_state.player_two_board target_cell with
          | new_board, Missed ->
              game_state.player_two_board <- new_board;
              game_state.player_one_turn <- not game_state.player_one_turn;
              Dream.respond "Miss"
          | new_board, Success ->
              game_state.player_two_board <- new_board;
              game_state.player_one_turn <- not game_state.player_one_turn;
              if is_game_over new_board then game_state.winner <- Some player;
              Dream.respond "Hit!"
          | _, Repeat ->
              Dream.respond ~status:`Bad_Request "Cell already attacked"
          | _, Invalid -> Dream.respond ~status:`Bad_Request "Invalid cell")
      | 2 -> (
          match attack_cell game_state.player_one_board target_cell with
          | new_board, Missed ->
              game_state.player_one_board <- new_board;
              game_state.player_one_turn <- not game_state.player_one_turn;
              Dream.respond "Miss"
          | new_board, Success ->
              game_state.player_one_board <- new_board;
              game_state.player_one_turn <- not game_state.player_one_turn;
              if is_game_over new_board then game_state.winner <- Some player;
              Dream.respond "Hit!"
          | _, Repeat ->
              Dream.respond ~status:`Bad_Request "Cell already attacked"
          | _, Invalid -> Dream.respond ~status:`Bad_Request "Invalid cell")
      | _ -> failwith "Invalid player"

let connection_handler (_ : Dream.request) : Dream.response Lwt.t =
  num_connections := !num_connections + 1;
  if !num_connections > 2 then
    Dream.respond ~status:`Too_Many_Requests "Too many players!"
  else
    Dream.respond
      ~headers:[ ("Connection", "keep-alive") ]
      (string_of_int !num_connections)

let get_turn_handler (_ : Dream.request) : Dream.response Lwt.t =
  if game_state.player_one_turn then Dream.respond "1" else Dream.respond "2"

let get_game_winner_handler (_ : Dream.request) : Dream.response Lwt.t =
  match game_state.winner with
  | None -> Dream.respond "None"
  | Some 1 -> Dream.respond "Player 1"
  | Some 2 -> Dream.respond "Player 2"
  | Some _ -> Dream.respond ~status:`Bad_Request "Invalid player"

let get_ready_status_handler (_ : Dream.request) : Dream.response Lwt.t =
  if
    List.length game_state.player_one_ships_to_place = 0
    && List.length game_state.player_two_ships_to_place = 0
  then Dream.respond "Ready"
  else Dream.respond "Not Ready"

let ships_to_place_handler (request : Dream.request) : Dream.response Lwt.t =
  let player = int_of_string @@ Dream.param "player" request in

  match player with
  | 1 ->
      if List.length game_state.player_one_ships_to_place = 0 then
        Dream.respond ~status:`No_Content ""
      else
        Dream.respond
        @@ List.to_string ~f:ship_type_to_string
             game_state.player_one_ships_to_place
  | 2 ->
      if List.length game_state.player_two_ships_to_place = 0 then
        Dream.respond ~status:`No_Content ""
      else
        Dream.respond
        @@ List.to_string ~f:ship_type_to_string
             game_state.player_two_ships_to_place
  | _ -> Dream.respond ~status:`Bad_Request "Invalid player"

let primary_board_handler (request : Dream.request) : Dream.response Lwt.t =
  let player = int_of_string @@ Dream.param "player" request in
  match player with
  | 1 -> Dream.respond @@ to_primary_board_string game_state.player_two_board
  | 2 -> Dream.respond @@ to_primary_board_string game_state.player_one_board
  | _ -> Dream.respond ~status:`Bad_Request "Invalid player"

let tracking_board_handler (request : Dream.request) : Dream.response Lwt.t =
  let player = int_of_string @@ Dream.param "player" request in
  match player with
  | 1 -> Dream.respond @@ to_tracking_board_string game_state.player_one_board
  | 2 -> Dream.respond @@ to_tracking_board_string game_state.player_two_board
  | _ -> Dream.respond ~status:`Bad_Request "Invalid player: "

let remaining_ships_handler (request : Dream.request) : Dream.response Lwt.t =
  let player = int_of_string @@ Dream.param "player" request in
  match player with
  | 1 ->
      get_remaining_ships game_state.player_two_board
      |> List.to_string ~f:ship_type_to_string
      |> Dream.respond
  | 2 ->
      get_remaining_ships game_state.player_one_board
      |> List.to_string ~f:ship_type_to_string
      |> Dream.respond
  | _ -> Dream.respond ~status:`Bad_Request "Invalid player: "

let () =
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" homepage_handler;
         Dream.post "/connect" connection_handler;
         Dream.scope "/info" []
           [
             Dream.get "/players-ready" get_ready_status_handler;
             Dream.get "/player-turn" get_turn_handler;
             Dream.get "/game-status" get_game_winner_handler;
           ];
         Dream.scope "/battleship" []
           [
             Dream.get "/ships-to-place/:player" ships_to_place_handler;
             Dream.get "/get-primary-board/:player" primary_board_handler;
             Dream.get "/get-tracking-board/:player" tracking_board_handler;
             Dream.get "/get-remaining-ships/:player" remaining_ships_handler;
             Dream.post "/place-ship/:player" place_ship_handler;
             Dream.post "/attack-cell/:player" attack_cell_handler;
           ];
       ]
  @@ Dream.not_found
