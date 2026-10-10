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

let dollar_demo =
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
|}

let%expect_test "It preserves $'s and position keywords in semantic actions" =
  helper dollar_demo;
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
    | h = HEADER /* lexically delimited by %{ ... %} */ {
        locate' $loc @@ DCode h |> singleton
      }
    | k = priority_keyword ss = clist(symbol) {
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
    | h = HEADER /* lexically delimited by %{ ... %} */ {
        locate' $loc @@ DCode h |> singleton
      }
    | TOKEN ty = option(ocamltype) ts = clist(terminal_alias_attrs) {
        locate' $loc @@ DToken (ty, ts) |> singleton
      } (* [menhir-lsp] Turned into a singleton. *)
    | START t = option(ocamltype) nts = clist(nonterminal) /* %start <ocamltype> foo is syntactic sugar for %start foo %type <ocamltype> foo */

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
    | FUNCTIONBLOCK (* Keywords cannot be identifiers but it is nice to
        let them parse as such to provide a better error *) {
        "functions", $loc, false
      }
    | FUNCTIONBLOCK {
        let module = ()
        (* Keywords cannot be identifiers but it is nice to
        let them parse as such to provide a better error *)
        in     ("functions", $loc, false)
      }
    |}]

let rules_demo =
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
    { locate' $loc @@ ParamAnonymous branches }|}

let%expect_test "Formatting of parameterized rules" =
  helper ~config:{ default_config with noLeadingBar = true } rules_demo;
  [%expect
    {|
    %%

    %inline generic_actual(A, B):
    (* 1- *)
      symbol = symbol actuals = plist(A) {
        locate' (startp symbol, $endpos(actuals)) @@ Parameter.apply symbol actuals
      }
    (* 2- *)
    | p = B m = located(modifier) {
        locate' $loc @@ Parameter.apply m [ p ]
      }

    strict_actual:
      p = generic_actual(strict_actual, strict_actual) { p }

    actual:
      p = generic_actual(lax_actual, actual) { p }

    lax_actual:
      p = generic_actual(
        lax_actual,
        /* cannot be lax_ */
        actual
      ) { p }
    (* 3- *)
    | /* leading bar disallowed */
      branches = located(branches) {
        locate' $loc @@ ParamAnonymous branches
      }
    |}]

let http_demo =
  {|%{ open Utils %}

%token <string> TEXT
%token VERSION "HTTP/1.1"
%token CRLF EOF
%token METHOD "GET"
%token COLON ":"
%token <int * string> STATUS_LINE
%token <string * string> FIELD_LINE

%start <Http_V1_types.request> request
%start <Http_V1_types.request> request_stream
%start <Http_V1_types.response> response
%start <Http_V1_types.response> response_stream

%%

// "\x1b[0;92mRequest parsing\x1b[0m"
let request :=
    req = terminated(request_stream, EOF); { req }

let request_stream :=
METHOD;
url = TEXT;
"HTTP/1.1";
CRLF;
header = list(header);
CRLF; { Http_V1_types.{ meth = GET; url; header } }

// "\x1b[0;95mResponse parsing\x1b[0m"
let response :=
terminated(response_stream, EOF)

let response_stream :=
    (status, message) = STATUS_LINE;
  CRLF;
  header = list(FIELD_LINE);
  CRLF; {
    log "\x1b[1;34mParsed response\x1b[0m";
    Http_V1_types.{ status; message; header; body = "" } }

(* log "\x1b[1;34mHeader rule\x1b[0m"; *)
let header :=
    field = TEXT; COLON; value = TEXT; CRLF; {
      (* log "\x1b[1;34mParsed header\x1b[0m"; *)
       field, value }|}

let%expect_test "It preserves byte escape sequences (e.g. ANSI color codes)" =
  http_demo |> format |> format |> helper;
  [%expect
    {|
    %{ open Utils %}

    %token <string> TEXT
    %token VERSION "HTTP/1.1"
    %token CRLF EOF
    %token METHOD "GET"
    %token COLON ":"
    %token <int * string> STATUS_LINE
    %token <string * string> FIELD_LINE

    %start <Http_V1_types.request> request
    %start <Http_V1_types.request> request_stream
    %start <Http_V1_types.response> response
    %start <Http_V1_types.response> response_stream

    %%

    // "\x1b[0;92mRequest parsing\x1b[0m"
    let request :=
        req = terminated(request_stream, EOF); { req }

    let request_stream :=
        METHOD;
      url = TEXT;
      "HTTP/1.1";
      CRLF;
      header = list(header);
      CRLF; { Http_V1_types.{ meth = GET; url; header } }

    // "\x1b[0;95mResponse parsing\x1b[0m"
    let response :=
        terminated(response_stream, EOF)

    let response_stream :=
        (status, message) = STATUS_LINE;
      CRLF;
      header = list(FIELD_LINE);
      CRLF;
      {
        log "\x1b[1;34mParsed response\x1b[0m";
        Http_V1_types.{ status; message; header; body = "" }
      }

    (* log "\x1b[1;34mHeader rule\x1b[0m"; *)
    let header :=
        field = TEXT;
      COLON;
      value = TEXT;
      CRLF;
      { (* log "\x1b[1;34mParsed header\x1b[0m"; *) field, value }
    |}]

let param_demo =
  {|
(* Taken from https://github.com/LexiFi/menhir/blob/master/demos/calc-param/parser.mly *)
%parameter<Semantics : sig
  type number
  val inject: int -> number
  val ( + ): number -> number -> number
  val ( - ): number -> number -> number
  val ( * ): number -> number -> number
  val ( / ): number -> number -> number
  val ( ~-): number -> number
end>

(* The parser no longer returns an integer; instead, it returns an
   abstract number. *)

%start <Semantics.number> main

(* Let us open the [Semantics] module, so as to make all of its
   operations available in the semantic actions. *)

%{

  open Semantics

%}

%%

main:
| e = expr EOL
    { e }

expr:
| i = INT
    { inject i }
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
    { - e } |}

let%expect_test "Formatting of parser parametrized by a module" =
  param_demo |> format |> format |> format |> format |> helper;
  [%expect
    {|
    (* Taken from https://github.com/LexiFi/menhir/blob/master/demos/calc-param/parser.mly *)
    %parameter <
      Semantics : sig
      type number
      val inject: int -> number
      val ( + ): number -> number -> number
      val ( - ): number -> number -> number
      val ( * ): number -> number -> number
      val ( / ): number -> number -> number
      val ( ~-): number -> number
    end
    >

    (* Let us open the [Semantics] module, so as to make all of its
       operations available in the semantic actions. *)

    %{ open Semantics %}

    (* The parser no longer returns an integer; instead, it returns an
       abstract number. *)

    %start <Semantics.number> main

    %%

    main:
    | e = expr EOL { e }

    expr:
    | i = INT { inject i }
    | LPAREN e = expr RPAREN { e }
    | e1 = expr PLUS e2 = expr { e1 + e2 }
    | e1 = expr MINUS e2 = expr { e1 - e2 }
    | e1 = expr TIMES e2 = expr { e1 * e2 }
    | e1 = expr DIV e2 = expr { e1 / e2 }
    | MINUS e = expr %prec UMINUS { -e }
    |}]

let inp = {|%%

%inline assignation:
    |
    | LET
    | SET {}
|}

let%expect_test "Formatting of empty production" =
  helper
    ~config:{ default_config with indentOnce = true; noLeadingBar = true }
    inp;
  [%expect
    {|
    %%

    %inline assignation:
      |
      | LET
      | SET {  }
    |}]

let anon_demo =
  {|%token FOO BAR SEMI COMMA

%start <int, Lexing.position> main

%%

%inline myfun(A, B):
  | A+ B {}
  |
  | A B A {}

main:
| a = myfun(nonempty_list(BAR), /*anonymous rule with leading empty production*/ { false } | SEMI* | COMMA { true }) { }
| k = FOO { 0, $loc(k) }

rule_S:
| list({}| FOO {} | BAR; SEMI? {} |{}) { 1, $symbolstartpos }
| {} | FOO | BAR | {}
|}

let%expect_test "Formatting of anonymous rules and EBNF operators" =
  anon_demo |> format |> format |> helper;
  [%expect {|
    %token FOO BAR SEMI COMMA

    %start <int, Lexing.position> main

    %%

    %inline myfun(A, B):
    | A+ B {  }
    |
    | A B A {  }

    main:
    | a = myfun(
        nonempty_list(BAR),
        /*anonymous rule with leading empty production*/
          { false }
        | SEMI*
        | COMMA { true }
      ) {  }
    | k = FOO { 0, $loc(k) }

    rule_S:
    | list({  } | FOO {  } | BAR SEMI? {  } | {  }) {
        1, $symbolstartpos
      }
    | {  }
    | FOO
    | BAR
    | {  }
    |}]

open Ast_equality

let%test "Formatted AST is equivalent to original AST" =
  let samples =
    [
      inp; calc_demo; http_demo; rules_demo; dollar_demo; param_demo; anon_demo;
    ]
  in
  let log s = log_src "  menhir-ast-equiv" s in
  let failures =
    L.filter_mapi
      (fun i s ->
        let i = succ i in
        let b = test_mly_string s in
        if b then (
          log "\x1b[0;32mtest #%d: OK\x1b[0m" i;
          None)
        else (
          log "\x1b[1;31mtest #%d: failed\x1b[0m\n\x1b[2;30m%s\x1b[0m\n" i s;
          Some (i, s)))
      samples
  in
  failures = []
