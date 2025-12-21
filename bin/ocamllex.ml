open OcamllexSyntax
open Utils

type state = { grammar : Syntax.lexer_definition }

let document_symbols ({ grammar } : state) : DocumentSymbol.t list =
  L.(
    let+ entry = grammar.entrypoints in
    let range = Range.of_lexical_positions entry.name.p in
    DocumentSymbol.create ~kind:Function ~name:entry.name.v ~range
      ~selectionRange:range ())
  @ Hashtbl.fold
      (fun name (p, _) ->
        let range = Range.of_lexical_positions p in
        DocumentSymbol.create ~kind:Property ~name ~range
          ~selectionRange:range ()
        |> L.cons)
      grammar.named_regexps []
