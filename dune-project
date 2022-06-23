(lang dune 3.0)
(generate_opam_files true)
(version 1.0.0)
(name mnd)
(source (github maurobringolf/mnd))
(authors "Mauro Bringolf")
(maintainers "Mauro Bringolf")
(license GPL-3.0-or-later)
(package
 (name mnd)
 (synopsis "mnd")
 (description "Minimal monads library")
 (depends
  (odoc :with-doc)
  (alcotest :with-test)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
