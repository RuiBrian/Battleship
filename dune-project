(lang dune 2.9)

(name battleship)

(generate_opam_files true)

(authors "Brian Rui")

(maintainers "brui1@jhu.edu")

(package
 (name battleship)
 (synopsis "A simple two-player implementation of Battleship")
 (description
  "A simple two-player implementation of the board game Battleship using a webserver. Each player first places all of their ships. Then, they take turns firing at cells in an attempt to sink the enemy ships.")
 (homepage "https://github.com/RuiBrian/Battleship")
 (bug_reports "https://github.com/RuiBrian/Battleship/issues")
 (depends
  (ounit2
   (>= 2.2.4))
  (core
   (>= 0.14.1))
  (dream
   (>= 1.0.0~alpha2))
  (lwt
   (>= 5.4.2))
  (ppx_jane
   (>= 0.14.0))
  (bisect_ppx
   (>= 2.7.0))
  (ppx_deriving_yojson
   (>= 3.6.1))
  (lwt_ppx
   (>= 2.0.3))
  (cohttp-lwt-unix
   (>= 4.0.0))))
