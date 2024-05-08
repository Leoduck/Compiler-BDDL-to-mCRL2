(lang dune 3.14)

(name bddl2mcrl2)

(generate_opam_files true)

(source
 (gitlab gruppe-25/bachelor-projekt))

(authors "Sofie Haugaard Olsen" "Leonard Hastrup")

(license LICENSE)

(using menhir 2.1)

(package
 (name bddl2mcrl2)
 (synopsis "A compiler from BDDL to mCRL2")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
