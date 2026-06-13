open Menhirformat_lib
open Utils
module Mll = OcamllexSyntax
module MF = Ocamllex

let config : Config.t = { tabsize = 2 }

let helper text : unit =
  text |> Mll.Main.parse_string
  |> Result.fold
       ~ok:(fun partial_grammar ->
         MF.main ~config ~ast:partial_grammar
           ~doc:
             (TD.make ~position_encoding:`UTF8
                {
                  textDocument =
                    {
                      languageId = "";
                      text;
                      uri = Uri.of_path "foo/bar";
                      version = 0;
                    };
                }))
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
  [%expect {|
    rule next_token = parse
      | "\t" | " " { next_token lexbuf }
      | "\n" | '\n' | "\r" { Lexing.new_line () }
      | '\\' | "\\" { lexer_logger "\\"; Parser.LDIVIDE }
      | "\\" ("[a-z]" as c)
        { Printf.printf "got rare backslash sequence: \"\\%c\"\n" c }
    |}]
