(lang dune 3.0)
(name mnd)
(version 1.1.0)
(source (github maurobringolf/mnd))
(authors "Mauro Bringolf <mauro@bringolf.com>")
(maintainers "Mauro Bringolf <mauro@bringolf.com>")
(license MIT)
(generate_opam_files true)
(homepage https://maurobringolf.ch/mnd)
(documentation https://maurobringolf.ch/mnd)
(formatting (enabled_for ocaml))
(package
 (name mnd)
 (synopsis "A small monads library")
 (description "A small monads library")
 (depends
  (ocaml (>= 4.08.0))
  (odoc :with-doc)
  (alcotest :with-test)
  (bisect_ppx :with-test)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
