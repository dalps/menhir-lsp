open Utils

type uri = Lsp.Types.DocumentUri.t

type token = {
  ocamltype : M.BaseTypes.ocamltype option;
  terminal : M.FrontTypes.terminal;
  alias : M.FrontTypes.alias;
  _attributes : M.FrontTypes.attributes;
}

open Loc

type tokens = token located list

type state = {
  grammar : M.Syntax.partial_grammar;
  tokens : token located list;
  (* While the rules are readily available in the pg_rules field of grammar,
      extracting the tokens requires a bit more work, hence the conveniece field. *)
  symbols : string located list;
}
