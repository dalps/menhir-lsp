(lang dune 3.20)

(name menhir-lsp)

(generate_opam_files true)

(source
 (github dalps/menhir-lsp))

(authors "Federico Dal Pio Luogo <dalpio23@gmail.com>")

(maintainers "Federico Dal Pio Luogo <dalpio23@gmail.com>")

(license GPL-2.0-only)

(documentation https://url/to/documentation)

(package
 (name menhir-lsp)
 (synopsis "LSP server for Menhir")
 (description
  "An implementation of the Language Server Protocol that serves completion suggestions and code navigation for .mly files to an LSP client.")
 (depends ocaml)
 (tags
  ("lsp" "menhir")))

(using menhir 3.0)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
