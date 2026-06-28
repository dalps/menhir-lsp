let parse lexbuf =
  Lexer.init ();
  Hashtbl.reset Syntax.named_regexps;
  try Ok (Parser.lexer_definition Lexer.main lexbuf) with
  | Syntax.SyntaxError { v; p; _ } | Lexer.Lexical_error { v; p; _ } ->
      Error (v, p)
  | _ ->
      let range =
        Range.make Lexing.(lexeme_start_p lexbuf, lexeme_end_p lexbuf)
      in
      Error
        (Printf.sprintf "Syntax error near '%s'" @@ Lexing.lexeme lexbuf, range)

let parse_file file =
  let inp = open_in file in
  let lexbuf = Lexing.from_channel inp in
  let ast = parse lexbuf in
  close_in inp;
  ast

let parse_string s = parse (Lexing.from_string s)
