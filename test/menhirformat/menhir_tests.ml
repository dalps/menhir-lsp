open Menhirformat_lib
open Utils
open Config
module Mly = MenhirSyntax
module MF = Menhir

let helper ?(config : Config.t = default_config) text : unit =
  text
  |> MenhirSyntax.Main.load_grammar_from_contents 0 ""
  |> Result.fold
       ~ok:(fun ast -> MF.main ~config ~ast ~doc:(doc_of_string text))
       ~error:(fun (msg, range) -> spr "%s at %s" msg (Mly.Range.show range))
  |> print_endline

let calc_demo =
  {|%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start <int> main
%type  <int> expr

%%

main:
| e = expr EOL
    { e }

expr:
| i = INT
    { i }
| LPAREN e = expr RPAREN
    { e }
| e1 = expr PLUS e2 = expr
    { e1 + e2 }
| e1 = expr MINUS e2 = expr
    { e1 - e2 }
| e1 = expr TIMES e2 = expr
    { e1 * e2 }
| e1 = expr DIV e2 = expr
    { e1 / e2 }
| MINUS e = expr %prec UMINUS
    { - e }
|}

let%expect_test "It can format the traditional syntax" =
  helper calc_demo;
  [%expect
    {|
    %token <int> INT
    %token PLUS MINUS TIMES DIV
    %token LPAREN RPAREN
    %token EOL

    %start <int> main

    %type <int> expr

    %left PLUS
    MINUS /* lowest precedence */
    %left TIMES
    DIV /* medium precedence */
    %nonassoc UMINUS /* highest precedence */

    %%

    main:
    | e = expr EOL { e }

    expr:
    | i = INT { i }
    | LPAREN e = expr RPAREN { e }
    | e1 = expr PLUS e2 = expr { e1 + e2 }
    | e1 = expr MINUS e2 = expr { e1 - e2 }
    | e1 = expr TIMES e2 = expr { e1 * e2 }
    | e1 = expr DIV e2 = expr { e1 / e2 }
    | MINUS e = expr %prec UMINUS { -e }
    |}]

let%expect_test "The [noLeadingBar] option works" =
  helper ~config:{ default_config with noLeadingBar = true } calc_demo;
  [%expect
    {|
    %token <int> INT
    %token PLUS MINUS TIMES DIV
    %token LPAREN RPAREN
    %token EOL

    %start <int> main

    %type <int> expr

    %left PLUS
    MINUS /* lowest precedence */
    %left TIMES
    DIV /* medium precedence */
    %nonassoc UMINUS /* highest precedence */

    %%

    main:
      e = expr EOL { e }

    expr:
      i = INT { i }
    | LPAREN e = expr RPAREN { e }
    | e1 = expr PLUS e2 = expr { e1 + e2 }
    | e1 = expr MINUS e2 = expr { e1 - e2 }
    | e1 = expr TIMES e2 = expr { e1 * e2 }
    | e1 = expr DIV e2 = expr { e1 / e2 }
    | MINUS e = expr %prec UMINUS { -e }
    |}]

let%expect_test "The [indentOnce] option works" =
  helper
    ~config:{ default_config with tabsize = 4; indentOnce = true }
    calc_demo;
  [%expect
    {|
    %token <int> INT
    %token PLUS MINUS TIMES DIV
    %token LPAREN RPAREN
    %token EOL

    %start <int> main

    %type <int> expr

    %left PLUS
    MINUS /* lowest precedence */
    %left TIMES
    DIV /* medium precedence */
    %nonassoc UMINUS /* highest precedence */

    %%

    main:
        | e = expr EOL { e }

    expr:
        | i = INT { i }
        | LPAREN e = expr RPAREN { e }
        | e1 = expr PLUS e2 = expr { e1 + e2 }
        | e1 = expr MINUS e2 = expr { e1 - e2 }
        | e1 = expr TIMES e2 = expr { e1 * e2 }
        | e1 = expr DIV e2 = expr { e1 / e2 }
        | MINUS e = expr %prec UMINUS { -e }
    |}]

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

let%expect_test "It preserves $'s and position keywords in semantic actions" =
  helper
    {|%token FOO

%start <int, Lexing.position> main

%%

main: FOO { ($1, $loc($1)) }

rule_S: FOO; k = list(FOO) { ($loc(k),  $endpos(k), $sloc, $startpos(k)) }

declaration:
| h = HEADER /* lexically delimited by %{ ... %} */
    { locate' $loc @@ DCode h |> singleton }
| k = priority_keyword ss = clist(symbol)
    {
      let _ = $loc, $sloc in
      let prec = ParserAux.new_precedence_level $loc(k) in
      locate' $loc(k) @@ DTokenProperties (ss, k, prec) |> singleton }
|};
  [%expect
    {|
    %token FOO

    %start <int, Lexing.position> main

    %%

    main:
    | FOO { $1, $loc($1) }

    rule_S:
    | FOO k = list(FOO) { $loc(k), $endpos(k), $sloc, $startpos(k) }

    declaration:
    | h = HEADER /* lexically delimited by %{ ... %} */
      { locate' $loc @@ DCode h |> singleton }
    | k = priority_keyword ss = clist(symbol)
      {
        let _ = ($loc, $sloc) in
        let prec = ParserAux.new_precedence_level $loc(k) in
        locate' $loc(k) @@ DTokenProperties (ss, k, prec) |> singleton
      }
    |}]
