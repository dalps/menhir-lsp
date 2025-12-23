open Utils
module C = Lsp.Types.CompletionItem

let assoc_decl_doc =
  {|Associate precedences and associativities to the given symbols. All symbols on the same line are given the same precedence. They have higher precedence than symbols declared before in a `%left`, `%right` or `%nonassoc` line. They have lower precedence than symbols declared after in a `%left`, `%right` or `%nonassoc` line. The symbols are declared to associate to the left (`%left`), to the right (`%right`), or to be non-associative (`%nonassoc`). The symbols are usually tokens. They can also be dummy nonterminals, for use with the `%prec` directive inside the rules.|}

let declarations =
  [
    ( "%token",
      None,
      None,
      [
        md_fenced {|%token constr ... constr|};
        {|Declare the given symbols [*`constr`*](https://ocaml.org/manual/5.4/names.html#constr) ... [*`constr`*](https://ocaml.org/manual/5.4/names.html#constr) as tokens (terminal symbols). These symbols are added as constant constructors for the `token` concrete type.|};
      ] );
    ( "%token",
      Some "<typexpr>",
      Some "%token <$1> $0",
      [
        md_fenced {|%token <typexpr> constr ... constr|};
        {|Declare the given symbols [*`constr`*](https://ocaml.org/manual/5.4/names.html#constr) ... [*`constr`*](https://ocaml.org/manual/5.4/names.html#constr) as tokens with an attached attribute of the given type. These symbols are added as constructors with arguments of the given type for the `token` concrete type. The [*`typexpr`*](https://ocaml.org/manual/5.4/types.html#typexpr) part is an arbitrary OCaml type expression, except that all type constructor names must be fully qualified (e.g. `Modname.typename`) for all types except standard built-in types, even if the proper `open` directives (e.g. `open Modname`) were given in the header section. That’s because the header is copied only to the `.ml` output file, but not to the `.mli` output file, while the [*`typexpr`*](https://ocaml.org/manual/5.4/types.html#typexpr) part of a `%token` declaration is copied to both.|};
      ] );
    ( "%start",
      None,
      None,
      [
        md_fenced {|%start symbol ... symbol|};
        {|Declare the given symbols as entry points for the grammar. For each entry point, a parsing function with the same name is defined in the output module. Non-terminals that are not declared as entry points have no such parsing function. Start symbols must be given a type with the `%type` directive below.|};
      ] );
    ( "%type",
      None,
      Some "%type <$1> $0",
      [
        md_fenced {|%type <typexpr> symbol ... symbol|};
        {|Specify the type of the semantic attributes for the given symbols. This is mandatory for start symbols only. Other nonterminal symbols need not be given types by hand: these types will be inferred when running the output files through the OCaml compiler (unless the `-s` option is in effect). The [*`typexpr`*](https://ocaml.org/manual/5.4/types.html#typexpr) part is an arbitrary OCaml type expression, except that all type constructor names must be fully qualified, as explained above for `%token`.|};
      ] );
    ( "%left",
      None,
      None,
      [ md_fenced {|%left symbol ... symbol|}; assoc_decl_doc ] );
    ( "%right",
      None,
      None,
      [ md_fenced {|%right symbol ... symbol|}; assoc_decl_doc ] );
    ( "%nonassoc",
      None,
      None,
      [ md_fenced {|%nonassoc symbol ... symbol|}; assoc_decl_doc ] );
  ]
  |> compile_completions ~kind:Keyword

(** Source: https://cambium.inria.fr/~fpottier/menhir/manual.html#sec52 *)
let position_keywords =
  [
    ( {|$startpos|},
      None,
      None,
      [
        {|start position of the first symbol in the production’s right-hand side, if there is one;|};
        {|end position of the most recently parsed symbol, otherwise|};
      ] );
    ( {|$endpos|},
      None,
      None,
      [
        {|end position of the last symbol in the production’s right-hand side, if there is one;|};
        {|end position of the most recently parsed symbol, otherwise|};
      ] );
    ( {|$startpos|},
      Some "( $i | id )",
      None,
      [ {|start position of the symbol named `$i` or `id`|} ] );
    ( {|$endpos|},
      Some {|( $i | id )|},
      None,
      [ {|end position of the symbol named `$i` or `id`|} ] );
    ( {|$symbolstartpos|},
      None,
      None,
      [
        {|start position of the leftmost symbol `id` such that `$startpos( id )` `!=` `$endpos( id )`;|};
        {|if there is no such symbol, `$endpos`|};
      ] );
    ( {|$startofs|},
      None,
      None,
      [
        {|same as `$startpos`, but produce an integer offset instead of a position|};
      ] );
    ( {|$endofs|},
      None,
      None,
      [
        {|same as `$endpos`, but produce an integer offset instead of a position|};
      ] );
    ( {|$startofs|},
      Some "( $i | id )",
      None,
      [
        {|same as `$startpos`, but produce an integer offset instead of a position|};
      ] );
    ( {|$endofs|},
      Some "( $i | id )",
      None,
      [
        {|same as `$endpos`, but produce an integer offset instead of a position|};
      ] );
    ( {|$symbolstartofs|},
      None,
      None,
      [
        {|same as `$symbolstartpos`, but produce an integer offset instead of a position|};
      ] );
    ({|$loc|}, None, None, [ {|stands for the pair `($startpos, $endpos)`|} ]);
    ( {|$loc|},
      Some "( id )",
      None,
      [ {|stands for the pair `($startpos( id ), $endpos( id ))`|} ] );
    ( {|$sloc|},
      None,
      None,
      [ {|stands for the pair `($symbolstartpos, $endpos)`|} ] );
  ]
  |> compile_completions ~kind:Value
