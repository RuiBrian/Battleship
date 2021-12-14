# Battleship

A **very** simple implementation of  the two-player board game [Battleship](https://en.wikipedia.org/wiki/Battleship_(game)) using OCaml.

This project uses the [Dream](https://aantron.github.io/dream/) library to implement a basic web server for two players to connect to. Each player will first place their ships onto the game board. Once both players have placed their ships, each player takes turns firing at cells on the enemy board. The goal is for a player to hit and sink all enemy ships.

The game uses the following ship types:

| Ship Type  | Length |
|------------|:------:|
| Carrier    |    5   |
| Battleship |    4   |
| Destroyer  |    3   |
| Submarine  |    3   |
| Patrol     |    2   |

Each ship can be oriented horizontally or vertically according to the user input.

Each player has their own primary board and tracking board. The primary board allows a player to see their own shots and whether they were hits or misses. The tracking board allows a player to see where their own ships are placed and also where the enemy has taken shots. The game uses the following to provide information about each cell:

| Cell Letter |    Meaning   |
|:-----------:|:------------:|
|      O      |   Occupied   |
|      H      |      Hit     |
|      M      |     Miss     |
|   \<empty>   | Undiscovered |

## Installation

To install all project dependencies:

```
$ opam install .
```

## Usage

To build the entire project:

```
$ dune build
```

To build and start the web server:

```
$ dune exec --root . ./server/server.exe
```

To build and start a game instance:

```
$ dune exec --root . ./src/game.exe
```

To place a ship:

```
$ <ship type> <row> <col> <orientation>

Example: Carrier 1 A Horizontal
```

To attack a cell:

```
$ <row> <col>

Example: 1 A
```

Currently, the web server must be restarted after each game (since only two connections are allowed at once). To do this, simply kill the web server with `Ctrl + C` and start the server again.

## Tests

All tests for the Battleship library functions are located in the `/tests` directory. Run the test suite with

```
$ dune test
```
