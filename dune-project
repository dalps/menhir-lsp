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
 (synopsis "LSP server for Menhir")
 (description
  "An implementation of the Language Server Protocol for Menhir grammars.")
 (depends ocaml)
 (tags
  ("lsp" "menhir")))

(using menhir 3.0)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
