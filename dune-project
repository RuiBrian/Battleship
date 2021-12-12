(lang dune 2.9)

(generate_opam_files true)

(authors "Brian Rui")

(maintainers "brui1@jhu.edu")

(package
 (name battleship)
 (synopsis "A simple two-player implementation of Battleship")
 (description
  "A simple two-player implementation of the board game Battleship using a webserver. Each player first places all of their ships. Then, they take turns firing at cells in an attempt to sink the enemy ships.")
 (homepage "")
 (bug_reports "")
 (depends
  (ounit2
   (>= 2.2.4))
  (core
   (>= 0.14.1))
  (yojson
   (>= 1.7.0))
  (dream
   (>= 1.0.0~alpha2))
  (lwt
   (>= 5.4.2))
  (ppx-jane
   (>= 0.14.0))
  (bisect-ppx
   (>= 2.7.0))
  (ppx-deriving-yojson
   (>= 3.6.1))
  (lwt-ppx
   (>= 2.0.3))
  (cohttp-lwt-unix
   (>= 4.0.0))))
