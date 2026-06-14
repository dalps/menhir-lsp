open Menhirformat_lib
open Utils
module Mll = OcamllexSyntax
module MF = Ocamllex

let helper ?(config : Config.t = Config.default_config) text : unit =
  text |> Mll.Main.parse_string
  |> Result.fold
       ~ok:(fun partial_grammar ->
         MF.main ~config ~ast:partial_grammar ~doc:(doc_of_string text))
       ~error:(fun (msg, range) -> spr "%s at %s" msg (Mll.Range.show range))
  |> print_endline

let%expect_test "It handles escape sequences correctly" =
  helper
    {|rule next_token = parse
    | "\t" | "\ " {next_token lexbuf}
  | "\n" | '\n' | "\r"  {Lexing.new_line ()}
  | '\\'
  | "\\"                      { lexer_logger "\\"; Parser.LDIVIDE }
  | "\\" ("[a-z]" as c) { Printf.printf "got rare backslash sequence: \"\\%c\"\n" c}
|};
  [%expect
    {|
    rule next_token = parse
    | "\t" | " " { next_token lexbuf }
    | "\n" | '\n' | "\r" { Lexing.new_line () }
    | '\\' | "\\" { lexer_logger "\\"; Parser.LDIVIDE }
    | "\\" ("[a-z]" as c)
      { Printf.printf "got rare backslash sequence: \"\\%c\"\n" c }
    |}]
