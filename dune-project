(lang dune 3.0)
(name mnd)
(version 1.0.0-alpha)
(source (github maurobringolf/mnd))
(authors "Mauro Bringolf")
(maintainers "Mauro Bringolf")
(generate_opam_files true)
(homepage https://maurobringolf.ch/mnd)
(documentation https://maurobringolf.ch/mnd)
(formatting (enabled_for ocaml))
(package
 (name mnd)
 (synopsis "A small monads library")
 (description "A small monads library")
 (depends
  (odoc :with-doc)
  (alcotest :with-test)
  (bisect_ppx :with-test)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
