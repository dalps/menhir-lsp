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
 (synopsis "Language Server for Menhir and Ocamllex")
 (description
  "Implementation of the Language Server Protocol for Menhir and Ocamllex, providing editors with navigation hints and completions in .mly and .mll files.")
 (depends
  ("ocaml"
   (>= "4.14"))
  ("linol"
   (>= "0.10"))
  linol-lwt
  lwt_ppx
  ("containers"
   (>= "3.14"))
  ("ordering"
   (>= "3.20.2"))
  ("menhir"
   (>= "20250912"))
  menhirSdk
  menhirLib
  ("re"
   (>= "1.14.0"))
  ("merlin-lib"
   (>= "4.19-414"))
  ("stdune"
   (>= "3.20.2"))
  visitors
  dune-build-info
  menhirformat
  cmarkit
  (odoc-parser
   (>= "3.2.1"))
  (interval-map
   (>= "0.2.1"))
  ("ppx_deriving"
   (>= "6.1.1"))
  (alcotest :with-test)
  (ppx_expect :with-test)
  (ppx_inline_test :with-test))
 (tags
  ("lsp" "menhir" "ocamllex")))

(package
 (name menhirformat)
 (synopsis "Formatter for Menhir and ocamllex")
 (description
  "A formatter for Menhir and ocamllex code. It takes care of indentation and whitespace in your lexer and parser rules and it calls ocamlformat to format the OCaml code of their semantic actions.")
 (depends
  ("ocaml"
   (>= "4.14"))
  ("linol"
   (>= "0.10"))
  visitors
  ("containers"
   (>= "3.6"))
  ("ordering"
   (>= "3.20.2"))
  ("menhir"
   (>= "20250912"))
  ("pprint"
   (>= "20230830"))
  ("ppx_deriving_yojson"
   (>= "3.10.0"))
  (ocamlformat-rpc-lib
   (>= "0.23.0"))
  ("re"
   (>= "1.14.0"))
  ("cmdliner"
   (>= "2.1.1"))
  (alcotest :with-test)
  (ppx_expect :with-test)
  (ppx_inline_test :with-test))
 (tags
  ("format" "menhir" "ocamllex")))

(using menhir 3.0)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
