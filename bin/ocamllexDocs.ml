open Utils

let manual_ref ?(label = "Manual") section =
  spr "[%s](https://ocaml.org/manual/5.4/lexyacc.html#%s)" label section

let regex_operator_completions : CompletionItem.t list =
  [
    ( "#",
      None,
      None,
      [
        md_fenced "regexp1 # regexp2";
        "(difference of character sets) Regular expressions `regexp1` and \
         `regexp2` must be character sets defined with `[` ... `]` (or a \
         single character expression or underscore `_`. Match the difference \
         of the two specified character sets.";
        manual_ref "ss:ocamllex-regexp";
      ] );
    ( "*",
      None,
      None,
      [
        md_fenced "regexp *";
        "(repetition) Match the concatenation of zero or more strings that \
         match `regexp`.";
        manual_ref "ss:ocamllex-regexp";
      ] );
    ( "+",
      None,
      None,
      [
        md_fenced "regexp +";
        "(strict repetition) Match the concatenation of one or more strings \
         that match `regexp`.";
        manual_ref "ss:ocamllex-regexp";
      ] );
    ( "?",
      None,
      None,
      [
        md_fenced "regexp ?";
        "(option) Match the empty string, or a string matching `regexp`.";
        manual_ref "ss:ocamllex-regexp";
      ] );
    ( "|",
      None,
      None,
      [
        md_fenced "regexp1 | regexp2";
        "(alternative) Match any string that matches `regexp1` or `regexp2`. \
         If both `regexp1` and `regexp2` are character sets, this \
         constructions produces another character set, obtained by taking the \
         union of `regexp1` and `regexp2`.";
        manual_ref "ss:ocamllex-regexp";
      ] );
  ]
  |> compile_completions ~kind:Operator

let keyword_completions : CompletionItem.t list =
  [
    ( "let",
      None,
      Some "let ${1:ident} = ${0:regexp}",
      [
        md_fenced "let ident = regexp";
        "Name a regular expression.";
        manual_ref "ss:ocamllex-named-regexp";
      ] );
    ( "rule",
      None,
      Some {|rule ${1:entrypoint} = parse
  | ${2:regexp} { ${0:action} }
|},
      [
        md_fenced
          {|rule entrypoint1 [arg1… argn] =
    parse regexp { action }
        | …
        | regexp { action }
and entrypoint2 [arg1… argn] =
    parse …
and …|};
        "Define an entry point of the lexer.";
        manual_ref ~label:"Syntax of lexer definitions" "s:ocamllex-syntax";
      ] );
    ( "and",
      None,
      None,
      [ manual_ref ~label:"Syntax of lexer definitions" "s:ocamllex-syntax" ] );
    ( "parse",
      None,
      None,
      [ manual_ref ~label:"Entry points" "ss:ocamllex-entry-points" ] );
    ( "shortest",
      None,
      None,
      [ manual_ref ~label:"Entry points" "ss:ocamllex-entry-points" ] );
    ( "refill",
      None,
      Some "refill { $0 }",
      [
        md_fenced {|refill {refill_handler}|};
        "Define a refill handler.";
        manual_ref "ss:refill-handlers";
      ] );
    ( "eof",
      None,
      None,
      [
        md_fenced "eof";
        "Match the end of the lexer input.";
        manual_ref "ss:ocamllex-regexp";
      ] );
    ( "as",
      None,
      None,
      [
        md_fenced "regexp as ident";
        "Bind the substring matched by `regexp` to identifier `ident`.";
        manual_ref "ss:ocamllex-variables";
      ] );
  ]
  |> compile_completions ~kind:Keyword

let lexbuf =
  [
    ( "lexbuf",
      None,
      None,
      [
        md_fenced "Lexing.lexbuf";
        "The current lexer buffer.";
        "Can be used in conjunction with the operations on lexer buffers \
         provided by the `Lexing` standard library module.";
        manual_ref "ss:ocamllex-actions";
      ] );
  ]
  |> compile_completions ~kind:Value
