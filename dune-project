(lang dune 3.7)

(name DataStructures)

(generate_opam_files true)

(source
 (github bjrtx/data_structures))

(authors "bjrtx")

(maintainers "bjrtx")

(license MIT)

(documentation https://bjrtx.github.io/data_structures)

(package
 (name DataStructures)
 (synopsis "Various functional data structures")
 (description "Mostly along the lines of Chris Okasaki's textbook.")
 (depends ocaml dune ppx_inline_test))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
