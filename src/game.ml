open Core
open Cohttp
open Cohttp_lwt_unix

type place_ship_request_obj = {
  cell : string;
  ship_type : string;
  orientation : string;
}
[@@deriving yojson]

type attack_cell_request_obj = { cell : string } [@@deriving yojson]

(* Base URL for the Battleship API; localhost for now *)
let api_url = "http://localhost:8080"

(* Either Player 1 or Player 2*)
let player_id = ref 0

(* Primary board for the player, represented as a string *)
let primary_board = ref ""

(* Tracking board for the player, represented as a string*)
let tracking_board = ref ""

let get_body body = body |> Cohttp_lwt.Body.to_string

let get_res_code res = res |> Response.status |> Code.code_of_status

let rec echo_line () =
  let%lwt line = Lwt_io.read_line Lwt_io.stdin in
  if String.( = ) line "exit" then Lwt_io.printl "Goodbye!"
  else
    let%lwt () = Lwt_io.printl line in
    echo_line ()

let rec place_ships () =
  let%lwt _ = Lwt_io.printl @@ "Your board:\n" ^ !tracking_board in
  let%lwt res, data =
    Client.get
      (Uri.of_string @@ api_url ^ "/battleship/ships-to-place/"
     ^ string_of_int !player_id)
  in
  let%lwt body = get_body data in
  let res_code = get_res_code res in

  match res_code with
  | 204 -> Lwt_io.printl @@ "All ships placed!"
  | 200 -> (
      let%lwt _ = Lwt_io.printl @@ "Remaining ships to place\n" ^ body in
      let%lwt line = Lwt_io.read_line Lwt_io.stdin in
      match String.split ~on:' ' line with
      | [ ship_type; row; col; orientation ] -> (
          let request =
            { cell = row ^ " " ^ col; ship_type; orientation }
            |> place_ship_request_obj_to_yojson |> Yojson.Safe.to_string
          in

          let%lwt res, data =
            Client.post
              ~body:(Cohttp_lwt.Body.of_string request)
              (Uri.of_string @@ api_url ^ "/battleship/place-ship/"
             ^ string_of_int !player_id)
          in

          let%lwt body = get_body data in
          let res_code = get_res_code res in
          match res_code with
          | 400 ->
              let%lwt _ = Lwt_io.printl @@ body ^ ". Try again" in
              place_ships ()
          | 200 ->
              (* TODO: Update board *)
              tracking_board := body;
              place_ships ()
          | _ -> failwith "")
      | _ ->
          let%lwt _ = Lwt_io.printl @@ "Invalid input. Try again" in
          place_ships ())
  | code -> failwith @@ "API responded with code " ^ string_of_int code

let connect () =
  let%lwt res, data = Client.post (Uri.of_string @@ api_url ^ "/connect") in
  let%lwt body = get_body data in
  let res_code = get_res_code res in
  if res_code <> 200 then failwith body
  else (
    player_id := int_of_string body;
    Lwt_io.printl @@ "Connected as Player " ^ body ^ ". Place your ships!")

let main () =
  let%lwt _ = connect () in
  place_ships ()
(* TODO: implement attack loop *)

let () = Lwt_main.run @@ main ()
