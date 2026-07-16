(*******************************************************************************
  This file is borrowed from ocaml-lsp.
  Source: https://github.com/ocaml/ocaml-lsp/blob/master/ocaml-lsp-server/src/doc_to_md.mli
*******************************************************************************)

type t =
  | Raw of string
  | Markdown of string

val translate : string -> t
