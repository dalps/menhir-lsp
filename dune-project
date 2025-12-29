(lang dune 3.20)

(name menhir-lsp)

(generate_opam_files true)

(source
 (github dalps/menhir-lsp))

(authors "Federico Dal Pio Luogo")

(maintainers "Federico Dal Pio Luogo")

(license GPL-2.0-only)

(package
 (name menhir-lsp)
 (synopsis "Language Server for Menhir and ocamllex")
 (description
  "Implementation of the Language Server Protocol for Menhir and ocamllex, providing editors with navigation hints and completions in .mly and .mll files.")
 (depends
  ("ocaml"
   (>= "4.14"))
  ("linol"
   (>= "0.10"))
  linol-lwt
  lwt_ppx
  ("containers"
   (>= "3.6"))
  ("ordering"
   (>= "3.20.2"))
  ("menhir"
   (>= "20250912"))
  menhirSdk
  menhirLib
  ("re"
   (>= "1.14.0")))
 (tags
  ("lsp" "menhir" "ocamllex")))

(using menhir 3.0)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
