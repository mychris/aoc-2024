(lang dune 3.1)

(name aoc-2024)

(generate_opam_files false)

(source (github mychris/aoc-2024))

(authors "Christoph Göttschkes")
(maintainers "Christoph Göttschkes")

(license "MIT")

(documentation https://github.com/mychris/aoc-2024/README.adoc)

(package
 (name aoc-2024)
 (synopsis "aoc-2024")
 (description "Advent of Code 2024")
 (depends
   ocaml
   dune
   re
   ppx_blob))
