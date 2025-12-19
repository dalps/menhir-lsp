let prec_decl_doc =
  {|Associate precedences and associativities to the given symbols. All symbols on the same line are given the same precedence. They have higher precedence than symbols declared before in a `%left`, `%right` or `%nonassoc` line. They have lower precedence than symbols declared after in a `%left`, `%right` or `%nonassoc` line. The symbols are declared to associate to the left (`%left`), to the right (`%right`), or to be non-associative (`%nonassoc`). The symbols are usually tokens. They can also be dummy nonterminals, for use with the `%prec` directive inside the rules.

The precedence declarations are used in the following way to resolve reduce/reduce and shift/reduce conflicts:

-   Tokens and rules have precedences. By default, the precedence of a rule is the precedence of its rightmost terminal. You can override this default by using the `%prec` directive in the rule.
-   A reduce/reduce conflict is resolved in favor of the first rule (in the order given by the source file), and `ocamlyacc` outputs a warning.
-   A shift/reduce conflict is resolved by comparing the precedence of the rule to be reduced with the precedence of the token to be shifted. If the precedence of the rule is higher, then the rule will be reduced; if the precedence of the token is higher, then the token will be shifted.
-   A shift/reduce conflict between a rule and a token with the same precedence will be resolved using the associativity: if the token is left-associative, then the parser will reduce; if the token is right-associative, then the parser will shift. If the token is non-associative, then the parser will declare a syntax error.
-   When a shift/reduce conflict cannot be resolved using the above method, then `ocamlyacc` will output a warning and the parser will always shift.|}

let declarations =
  CCHashtbl.of_list
    [
      ( "token",
        [
          {|#### `%token` [*`constr`*](https://ocaml.org/manual/5.4/names.html#constr) … [*`constr`*](https://ocaml.org/manual/5.4/names.html#constr)|};
          {|Declare the given symbols [*`constr`*](https://ocaml.org/manual/5.4/names.html#constr) … [*`constr`*](https://ocaml.org/manual/5.4/names.html#constr) as tokens (terminal symbols). These symbols are added as constant constructors for the `token` concrete type.|};
        ] );
      ( "token_t",
        [
          {|#### `%token` < [*`typexpr`*](https://ocaml.org/manual/5.4/types.html#typexpr) > [*`constr`*](https://ocaml.org/manual/5.4/names.html#constr) … [*`constr`*](https://ocaml.org/manual/5.4/names.html#constr)|};
          {|Declare the given symbols [*`constr`*](https://ocaml.org/manual/5.4/names.html#constr) … [*`constr`*](https://ocaml.org/manual/5.4/names.html#constr) as tokens with an attached attribute of the given type. These symbols are added as constructors with arguments of the given type for the `token` concrete type. The [*`typexpr`*](https://ocaml.org/manual/5.4/types.html#typexpr) part is an arbitrary OCaml type expression, except that all type constructor names must be fully qualified (e.g. `Modname.typename`) for all types except standard built-in types, even if the proper `open` directives (e.g. `open Modname`) were given in the header section. That’s because the header is copied only to the `.ml` output file, but not to the `.mli` output file, while the [*`typexpr`*](https://ocaml.org/manual/5.4/types.html#typexpr) part of a `%token` declaration is copied to both.|};
        ] );
      ( "start",
        [
          {|#### `%start` *`symbol`* … *`symbol`*|};
          {|Declare the given symbols as entry points for the grammar. For each entry point, a parsing function with the same name is defined in the output module. Non-terminals that are not declared as entry points have no such parsing function. Start symbols must be given a type with the `%type` directive below.|};
        ] );
      ( "type",
        [
          {|#### `%type` < [*`typexpr`*](https://ocaml.org/manual/5.4/types.html#typexpr) > *`symbol`* … *`symbol`*|};
          {|Specify the type of the semantic attributes for the given symbols. This is mandatory for start symbols only. Other nonterminal symbols need not be given types by hand: these types will be inferred when running the output files through the OCaml compiler (unless the `-s` option is in effect). The [*`typexpr`*](https://ocaml.org/manual/5.4/types.html#typexpr) part is an arbitrary OCaml type expression, except that all type constructor names must be fully qualified, as explained above for `%token`.|};
        ] );
      ("left", [ {|#### `%left` *`symbol`* … *`symbol`*|}; prec_decl_doc ]);
      ("right", [ {|#### `%right` *`symbol`* … *`symbol`*|}; prec_decl_doc ]);
      ( "nonassoc",
        [ {|#### `%nonassoc` *`symbol`* … *`symbol`*|}; prec_decl_doc ] );
    ]
