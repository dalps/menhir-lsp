open Utils
module C = Lsp.Types.CompletionItem

let declarations ?(range : Range.t option) () =
  let manual_ref sec =
    spr "[Manual](https://cambium.inria.fr/~fpottier/menhir/manual.html#sec%s)"
      sec
  in
  let assoc_decl_doc =
    {|assigns both a priority level and an associativity status to the symbols `uid_1`, ..., `uid_n`.

The priority level assigned to `uid_1`, ..., `uid_n` is not defined explicitly: instead, it is defined to be higher than the priority level assigned by the previous `%nonassoc`, `%left`, or `%right` declaration, and lower than that assigned by the next `%nonassoc`, `%left`, or `%right` declaration.

The symbols `uid_1`, ..., `uid_n` can be tokens (defined elsewhere by a `%token` declaration) or dummies (not defined anywhere). Both can be referred to as part of `%prec` annotations. Associativity status and priority levels allow shift/reduce conflicts to be silently resolved.|}
  in
  let token_doc =
    {|defines the identifiers `uid_1`, ..., `uid_n` as tokens, that is, as terminal symbols in the grammar specification and as data constructors in the token type.

If an OCaml type `t` is present, then these tokens are considered to carry a semantic value of type `t`, otherwise they are considered to carry no semantic value.

If a quoted identifier `qid_i` is present, then it is considered an alias for the terminal symbol `uid_i`.|}
  in
  [
    ( "%token",
      None,
      None,
      [
        md_fenced
          {|%token [ <OCaml type> ] uid_1 [ qid_1 ] ... uid_n [ qid_n ]|};
        token_doc;
        manual_ref "11";
      ] );
    ( "%token",
      Some "<OCaml type>",
      (* Some "%token <$1> $0", *) None,
      [
        md_fenced
          {|%token [ <OCaml type> ] uid_1 [ qid_1 ] ... uid_n [ qid_n ]|};
        token_doc;
        manual_ref "11";
      ] );
    ( "%left",
      None,
      None,
      [ md_fenced {|%left uid_1 ... uid_n|}; assoc_decl_doc; manual_ref "12" ]
    );
    ( "%right",
      None,
      None,
      [ md_fenced {|%right uid_1 ... uid_n|}; assoc_decl_doc; manual_ref "12" ]
    );
    ( "%nonassoc",
      None,
      None,
      [
        md_fenced {|%nonassoc uid_1 ... uid_n|}; assoc_decl_doc; manual_ref "12";
      ] );
    ( "%type",
      None,
      (* Some "%type <$1> $0", *) None,
      [
        md_fenced {|%type <OCaml type> lid_1 ... lid_n|};
        {|assigns an OCaml type to each of the nonterminal symbols `lid_1`, ..., `lid_n`. For start symbols, providing an OCaml type is mandatory, but is usually done as part of the `%start` declaration. For other symbols, it is optional. Providing type information can improve the quality of OCaml’s type error messages.|};
        manual_ref "13";
      ] );
    ( "%start",
      None,
      None,
      [
        md_fenced {|%start [ <OCaml type> ] lid_1 ... lid_n|};
        {|declares the nonterminal symbols `lid_1`, ..., `lid_n` to be start symbols. Each such symbol must be assigned an OCaml type either as part of the %start declaration or via separate `%type` declarations. Each of `lid_1`, ..., `lid_n` becomes the name of a function whose signature is published in the `.mli` file and that can be used to invoke the parser.|};
        manual_ref "14";
      ] );
    ( "%prec",
      None,
      None,
      [
        md_fenced "%prec id";
        {|indicates that the precedence level of the production group is the level assigned to the symbol `id` via a previous `%nonassoc`, `%left`, or `%right` declaration. In the absence of a `%prec` annotation, the precedence level assigned to each production is the level assigned to the rightmost terminal symbol that appears in it.|};
        manual_ref "%3Aprec";
      ] );
    ( "%on_error_reduce",
      None,
      None,
      [
        md_fenced "%on_error_reduce lid_1 ... lid_n";
        {|marks the nonterminal symbols `lid_1`, ..., `lid_n` as potentially eligible for reduction when an invalid token is found. This may cause one or more extra reduction steps to be performed before the error is detected.|};
        manual_ref "%3Aonerrorreduce";
      ] );
    ( "%public",
      None,
      None,
      [
        md_fenced "%public option(X):\n    | { None }\n    | x = X { Some x }";
        "exposes the nonterminal symbol `option` for use within client grammar \
         modules";
        manual_ref "%3Asplit";
      ] );
    ( "%inline",
      None,
      None,
      [
        md_fenced {|%inline op:
    | PLUS { ( + ) }
    | TIMES { ( * ) }|};
        "causes all references to `op` to be replaced with its definition.";
        manual_ref "%3Ainline";
      ] );
    ( "%parameter",
      None,
      None,
      [
        md_fenced "%parameter <uid : OCaml module type>";
        {|causes the entire parser to become parameterized over the OCaml module `uid`, that is, to become an OCaml functor.|};
        manual_ref "%3Aparameter";
      ] );
  ]
  |> compile_completions ?range ~kind:Keyword

(** https://cambium.inria.fr/~fpottier/menhir/manual.html#fig%3Asugar *)
let ebnf_operators =
  [
    ( "?",
      None,
      None,
      [ md_fenced "actual ?"; "is syntactic sugar for `option(actual)`" ] );
    ( "+",
      None,
      None,
      [ md_fenced "actual +"; "is syntactic sugar for `nonempty_list(actual)`" ]
    );
    ( "*",
      None,
      None,
      [ md_fenced "actual *"; "is syntactic sugar for `list(actual)`" ] );
  ]
  |> compile_completions ~kind:Operator

let position_keywords ?(range : Range.t option) () =
  let manual_ref =
    "[Manual](https://cambium.inria.fr/~fpottier/menhir/manual.html#sec52)"
  in
  [
    ( {|$startpos|},
      None,
      None,
      [
        {|start position of the first symbol in the production’s right-hand side, if there is one;|};
        {|end position of the most recently parsed symbol, otherwise|};
        manual_ref;
      ] );
    ( {|$endpos|},
      None,
      None,
      [
        {|end position of the last symbol in the production’s right-hand side, if there is one;|};
        {|end position of the most recently parsed symbol, otherwise|};
        manual_ref;
      ] );
    ( {|$startpos|},
      Some "($i | id)",
      (* Some {|\$startpos($0)|}, *) None,
      [ {|start position of the symbol named `$i` or `id`|}; manual_ref ] );
    ( {|$endpos|},
      Some "($i | id)",
      (* Some {|\$endpos($0)|}, *) None,
      [ {|end position of the symbol named `$i` or `id`|}; manual_ref ] );
    ( {|$symbolstartpos|},
      None,
      None,
      [
        {|start position of the leftmost symbol `id` such that `$startpos(id) != $endpos(id)`;|};
        {|if there is no such symbol, `$endpos`|};
        manual_ref;
      ] );
    ( {|$startofs|},
      None,
      None,
      [
        {|same as `$startpos`, but produce an integer offset instead of a position|};
        manual_ref;
      ] );
    ( {|$endofs|},
      None,
      None,
      [
        {|same as `$endpos`, but produce an integer offset instead of a position|};
        manual_ref;
      ] );
    ( {|$startofs|},
      Some "($i | id)",
      (* Some {|\$startofs($0)|}, *) None,
      [
        {|same as `$startpos`, but produce an integer offset instead of a position|};
        manual_ref;
      ] );
    ( {|$endofs|},
      Some "($i | id)",
      (* Some {|\$endofs($0)|}, *) None,
      [
        {|same as `$endpos`, but produce an integer offset instead of a position|};
        manual_ref;
      ] );
    ( {|$symbolstartofs|},
      None,
      None,
      [
        {|same as `$symbolstartpos`, but produce an integer offset instead of a position|};
        manual_ref;
      ] );
    ( {|$loc|},
      None,
      None,
      [ {|stands for the pair `($startpos, $endpos)`|}; manual_ref ] );
    ( {|$loc|},
      Some "(id)",
      (* Some {|\$loc($0)|}, *) None,
      [ {|stands for the pair `($startpos(id), $endpos(id))`|}; manual_ref ] );
    ( {|$sloc|},
      None,
      None,
      [ {|stands for the pair `($symbolstartpos, $endpos)`|}; manual_ref ] );
  ]
  |> compile_completions ?range ~kind:Value
