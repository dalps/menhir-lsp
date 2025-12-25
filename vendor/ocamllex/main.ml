let parse_file file =
  let inp = open_in file in
  let lexbuf = Lexing.from_channel inp in
  let ast = Parser.lexer_definition Lexer.main lexbuf in
  close_in inp;
  ast

let parse_string s =
  s |> Lexing.from_string |> Parser.lexer_definition Lexer.main
