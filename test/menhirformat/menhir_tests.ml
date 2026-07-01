open Menhirformat_lib
open Utils
open Config
module Mly = MenhirSyntax
module MF = Menhir

let format, helper = get_test_helpers MF.format_string

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

    %left PLUS MINUS /* lowest precedence */
    %left TIMES DIV /* medium precedence */
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

let%expect_test "The [separateProducers] option works" =
  helper ~config:{ default_config with semiAfterProducer = true } calc_demo;
  [%expect
    {|
    %token <int> INT
    %token PLUS MINUS TIMES DIV
    %token LPAREN RPAREN
    %token EOL

    %start <int> main

    %type <int> expr

    %left PLUS MINUS /* lowest precedence */
    %left TIMES DIV /* medium precedence */
    %nonassoc UMINUS /* highest precedence */

    %%

    main:
    | e = expr; EOL; { e }

    expr:
    | i = INT; { i }
    | LPAREN; e = expr; RPAREN; { e }
    | e1 = expr; PLUS; e2 = expr; { e1 + e2 }
    | e1 = expr; MINUS; e2 = expr; { e1 - e2 }
    | e1 = expr; TIMES; e2 = expr; { e1 * e2 }
    | e1 = expr; DIV; e2 = expr; { e1 / e2 }
    | MINUS; e = expr; %prec UMINUS { -e }
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

    %left PLUS MINUS /* lowest precedence */
    %left TIMES DIV /* medium precedence */
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

let%expect_test
    "Comments can sit on top of rule branches, before the leading bar" =
  helper
    {|%token FUNCTIONBLOCK
%start <unit> reserved_word

%%

reserved_word:
  (* Keywords cannot be identifiers but it is nice to
    let them parse as such to provide a better error *)
  | FUNCTIONBLOCK { "functions", $loc, false }|};
  [%expect
    {|
    %token FUNCTIONBLOCK

    %start <unit> reserved_word

    %%

    reserved_word:
    (* Keywords cannot be identifiers but it is nice to
        let them parse as such to provide a better error *)
    | FUNCTIONBLOCK { "functions", $loc, false }
    |}]

let%expect_test "Comments can sit on top of action blocks" =
  helper
    {|%%

declaration:
| h = HEADER; /* lexically delimited by %{ ... %} */
    { locate' $loc @@ DCode h |> singleton }
| TOKEN; ty = option(ocamltype);
    ts = clist(terminal_alias_attrs);
    { locate' $loc @@ DToken (ty, ts) |> singleton } (* [menhir-lsp] Turned into a singleton. *)
| START; t = option(ocamltype); nts = clist(nonterminal);
    /* %start <ocamltype> foo is syntactic sugar for %start foo %type <ocamltype> foo */

    (* [menhir-lsp] desugared. *)
    { locate' $loc @@ DStart (t, nts) |> singleton }|};
  [%expect
    {|
    %%

    declaration:
    | h = HEADER /* lexically delimited by %{ ... %} */
      { locate' $loc @@ DCode h |> singleton }
    | TOKEN ty = option(ocamltype) ts = clist(terminal_alias_attrs)
      { locate' $loc @@ DToken (ty, ts) |> singleton } (* [menhir-lsp] Turned into a singleton. *)
    | START t = option(ocamltype) nts = clist(nonterminal)
      /* %start <ocamltype> foo is syntactic sugar for %start foo %type <ocamltype> foo */

      (* [menhir-lsp] desugared. *)
      { locate' $loc @@ DStart (t, nts) |> singleton }
    |}]

let%test "Formatting of OCaml fragments is idempotent" =
  let input =
    {|%{
(* Takes a sized_basic_type and a list of sizes and repeatedly applies then
   SArray constructor, taking sizes off the list *)
let reducearray (sbt, l) =
  List.fold_right l ~f:(fun z y -> SizedType.SArray (y, z)) ~init:sbt
%}

%%
|}
  in
  String.equal
    (input |> format |> format |> format |> format |> format)
    (format input)

let%expect_test
    "Gracefully fails on invalid OCaml code (`List.fold_right l f:(fun z y \
     ..`, `let module = ()`) and skips the bad block." =
  let input =
    {|%{
    (* Takes a sized_basic_type and a list of sizes and repeatedly applies then
        SArray constructor, taking sizes off the list *)
     let reducearray (sbt, l) =
       List.fold_right l f:(fun z y -> SizedType.SArray (y, z)) ~init:sbt
%}

%token FUNCTIONBLOCK

%start <unit> reserved_word

%%

reserved_word:
| FUNCTIONBLOCK;
    (* Keywords cannot be identifiers but it is nice to
    let them parse as such to provide a better error *)
    { "functions", $loc, false }
| FUNCTIONBLOCK;
    { let module = ()
    (* Keywords cannot be identifiers but it is nice to
    let them parse as such to provide a better error *)
    in     ("functions", $loc, false) }|}
  in
  input |> format |> format |> format |> format |> helper;
  [%expect
    {|
    %{
      (* Takes a sized_basic_type and a list of sizes and repeatedly applies then
            SArray constructor, taking sizes off the list *)
         let reducearray (sbt, l) =
           List.fold_right l f:(fun z y -> SizedType.SArray (y, z)) ~init:sbt
    %}

    %token FUNCTIONBLOCK

    %start <unit> reserved_word

    %%

    reserved_word:
    | FUNCTIONBLOCK
      (* Keywords cannot be identifiers but it is nice to
        let them parse as such to provide a better error *)
      { "functions", $loc, false }
    | FUNCTIONBLOCK
      {
        let module = ()
        (* Keywords cannot be identifiers but it is nice to
        let them parse as such to provide a better error *)
        in     ("functions", $loc, false)
      }
    |}]

let%expect_test "Formatting of parameterized rules" =
  helper
    ~config:{ default_config with noLeadingBar = true }
    {|%%

%inline generic_actual(A, B):
(* 1- *)
  symbol = symbol actuals = plist(A)
    { locate' (startp symbol, $endpos(actuals)) @@ Parameter.apply symbol actuals }
(* 2- *)
| p = B m = located(modifier)
    { locate' $loc @@ Parameter.apply m [p] }

strict_actual:
  p = generic_actual(strict_actual, strict_actual)
    { p }

actual:
  p = generic_actual(lax_actual, actual)
    { p }

lax_actual:
  p = generic_actual(lax_actual, /* cannot be lax_ */ actual)
    { p }
(* 3- *)
| /* leading bar disallowed */
  branches = located(branches)
    { locate' $loc @@ ParamAnonymous branches }|};
  [%expect {|
    %%

    %inline generic_actual(A, B):
    (* 1- *)
      symbol = symbol actuals = plist(A)
      {
        locate' (startp symbol, $endpos(actuals)) @@ Parameter.apply symbol actuals
      }
    (* 2- *)
    | p = B m = located(modifier)
      { locate' $loc @@ Parameter.apply m [ p ] }

    strict_actual:
      p = generic_actual(strict_actual, strict_actual) { p }

    actual:
      p = generic_actual(lax_actual, actual) { p }

    lax_actual:
      p = generic_actual(
        lax_actual,
        /* cannot be lax_ */
        actual
      )
      { p }
    (* 3- *)
    | /* leading bar disallowed */
      branches = located(branches)
      { locate' $loc @@ ParamAnonymous branches }
    |}]
