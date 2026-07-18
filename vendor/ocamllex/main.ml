let parse get_lexbuf =
  Lexer.init ();
  Hashtbl.reset Syntax.named_regexps;
  let lexbuf = get_lexbuf () in
  try
    Original_parser.lexer_definition Lexer.main lexbuf;
    let lexbuf = get_lexbuf () in
    try Ok (Parser.lexer_definition Lexer.main lexbuf) with
    | Parser.Error ->
        Error
          ( "Parser error",
            Lexing.(lexbuf.lex_start_p, Lexing.lexeme_end_p lexbuf) )
    | Syntax.SyntaxError { v; p; _ } | Lexer.Lexical_error { v; p; _ } ->
        Error (v, p)
  with
  | Original_parser.Error state ->
      let message = Original_parser_messages.message state in
      let range =
        Range.make Lexing.(lexeme_start_p lexbuf, lexeme_end_p lexbuf)
      in
      Error
        ( Printf.sprintf "Syntax error near '%s':\n%s" (Lexing.lexeme lexbuf)
            message,
          range )
  | Syntax.SyntaxError { v; p; _ } | Lexer.Lexical_error { v; p; _ } ->
      Error (v, p)

let parse_file file =
  let inp = open_in file in
  let get_lexbuf () =
    seek_in inp 0;
    let lexbuf = Lexing.from_channel inp in
    Lexing.set_filename lexbuf file;
    lexbuf
  in
  let ast = parse get_lexbuf in
  close_in inp;
  ast

let parse_string s = parse (fun () -> Lexing.from_string s)
