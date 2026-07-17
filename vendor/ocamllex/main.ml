let parse lexbuf =
  Lexer.init ();
  Hashtbl.reset Syntax.named_regexps;
  try
    Original_parser.lexer_definition Lexer.main lexbuf;
    try Ok (Parser.lexer_definition Lexer.main lexbuf)
    with Syntax.SyntaxError { v; p; _ } | Lexer.Lexical_error { v; p; _ } ->
      Error (v, p)
  with Original_parser.Error state ->
    let message = Original_parser_messages.message state in
    let range =
      Range.make Lexing.(lexeme_start_p lexbuf, lexeme_end_p lexbuf)
    in
    Error
      ( Printf.sprintf "Syntax error near '%s':\n%s" (Lexing.lexeme lexbuf)
          message,
        range )

let parse_file file =
  let inp = open_in file in
  let lexbuf = Lexing.from_channel inp in
  Lexing.set_filename lexbuf file;
  let ast = parse lexbuf in
  close_in inp;
  ast

let parse_string s = parse (Lexing.from_string s)
