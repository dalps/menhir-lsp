open Menhirformat_lib
open Utils
module Mly = MenhirSyntax
module MF = Menhir

let config : Config.t = { tabsize = 2 }

let helper text : unit =
  text
  |> MenhirSyntax.Main.load_grammar_from_contents 0 ""
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
       ~error:(fun _ -> "parser error")
  |> print_endline

let%expect_test "It can handle the new syntax" =
  helper
    {|%token FOO
%token BAR
%token BAZ
%%

let main == expr; EOF; { () }

let expr := expr; BAR; expr; <Bar> | FOO; <Foo>
|};
  [%expect
    {|
    %token FOO
    %token BAR
    %token BAZ

    %%

    let main ==
        expr; EOF; { () }

    let expr :=
        expr; BAR; expr; <Bar>
      | FOO; <Foo>
    |}]

let%expect_test "It preserves $ and position keywords in semantic actions" =
  helper
    {|%token FOO

%start <int, Lexing.position> main

%%

main: FOO { ($1, $loc($1)) }

rule_S: FOO; list(FOO) { ($2, $symbolstartpos) }
|};
  [%expect
    {|
    %token FOO

    %start <int, Lexing.position> main

    %%

    main:
      | FOO { $1, $loc($1) }

    rule_S:
      | FOO list(FOO) { $2, $symbolstartpos }
    |}]
