(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module defines the abstract syntax of Menhir's input files, whose
   extension is [.mly].

   The type [partial_grammar] describes the abstract syntax of a partial
   grammar, that is, just a fragment of a grammar. This syntax is produced by
   the parser. An [.mly] file represents a partial grammar. Several partial
   grammars can be joined so as to obtain a complete grammar.

   The type [grammar] describes the abstract syntax of a complete grammar. It
   differs from [partial_grammar] in that declarations are organized in a more
   useful way and a number of well-formedness checks have been performed. *)

(* include FrontTypes [menhir-lsp]: hard-include *)

(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

include BaseTypes
open Attribute

type ocamltype = OCamlType.ocamltype =
  | Declared of string located
  | Inferred of string

and terminal = string
(**A terminal symbol. *)

and nonterminal = string
(**A nonterminal symbol. *)

and symbol = string
(**A terminal or nonterminal symbol. *)

and symbols = symbol list
(**A list of symbols. *)

and alias = string option
(**In a somewhat fragile convention, in a partial grammar, a reference to a
   terminal symbol either is a normal identifier [LID], in which case it is the
   name of the terminal symbol, or is a quoted identifier [QID], in which case
   it is a token alias.

   Token aliases are eliminated by replacing them with the corresponding
   terminal symbols very early on during the joining of the partial grammars;
   see the module [ExpandTokenAliases].

   In a complete grammar, there are no token aliases any longer. That is, we
   keep track of the aliases that have been declared (they can be found via the
   field [tk_alias]), but we never use them, since they have been eliminated up
   front. *)

and identifier = string
(**An identifier is used to refer to a symbol's semantic value. *)

and filename = string
(**A file name. *)

and action = (Action.t[@opaque])
(**A semantic action. *)

(**The associativity status of a terminal symbol. *)
and associativity = LeftAssoc | RightAssoc | NonAssoc | UndefinedAssoc

(**The precedence level of a terminal symbol.

   The special value [UndefinedPrecedence] is used for the special tokens
   [error] and [#]. It is also used for pseudo-tokens (which are not declared,
   but appear in a [%prec] annotation). A normal token receives a precedence
   level of the form [PrecedenceLevel (file, level)], where [file] is an input
   file and [level] is an integer level. Within each file, later declarations
   receive greater levels, which represent higher priority. Two declarations
   that originate in different files are incomparable. *)
and precedence_level =
  | UndefinedPrecedence
  | PrecedenceLevel of ((InputFile.file[@opaque]) * int) located

(**A "production level" is used to solve reduce/reduce conflicts. It reflects
   which production appears first in the grammar. See [ParserAux]. *)
and production_level = ProductionLevel of (InputFile.file[@opaque]) * int

and prec_annotation =
  symbol located located option (* [menhir-lsp] double-located *)
(**A [%prec] annotation is optional. A production can carry at most one. If
   there is one, it is a symbol name. See [ParserAux]. *)

and on_error_reduce_level =
  production_level (* we re-use the above type, to save code *)
(**A level is attached to every [%on_error_reduce] declaration. It is used to
   decide what to do when several such declarations are applicable in a single
   state. *)

and properties = {
  tk_ocamltype : ocamltype option;
      (**The OCaml type of this terminal symbol can be provided via a [%type]
         declaration. It is optional. *)
  tk_position : range; [@opaque]
      (**The position where this terminal symbol is declared. *)
  tk_alias : alias;
      (**A "token alias" can be declared for this terminal symbol. It is
         optional. *)
  tk_attributes : attributes; [@opaque] [@opaque]
      (**The attributes attached with this terminal symbol. *)
  tk_associativity : associativity;
      (**The associativity status of this terminal symbol. *)
  tk_precedence : precedence_level;
      (**The precedence level of this terminal symbol. *)
  tk_is_declared : bool;
      (**Whether a declaration of this terminal symbol has been found. When no
         declaration exists for a terminal symbol, then it is considered a
         "pseudo-token". In such a case, it is really just a name for a
         precedence level; it can appear in a [%prec] annotation. *)
}
(**The properties of a terminal symbol. *)
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(**Attributes allow the user to annotate the grammar with information that is
   ignored by Menhir, but can be exploited by other tools, via the SDK. *)
(**Attributes can be attached in the following places:

   - with the grammar:         %[@bar ...]
   - with a terminal symbol:   %token FOO [@bar ...]
   - with a rule:              foo(X) [@bar ...]: ...
   - with a production:        e = expr { e } [@bar ...]
   - with a producer:          e = foo(quux) [@bar ...]
   - with an arbitrary symbol: %attribute FOO foo(quux) [@bar ...]

   After expanding away parameterized nonterminal symbols, things become
   a bit simpler, as %attribute declarations are desugared away. *)
(**The old rule syntax. Although old, still used internally. The new syntax is
   translated down to it. *)

(**A parameter is either a symbol or an application of a symbol to a nonempty
   tuple of parameters. These two cases correspond to the constructors
   [ParamVar] and [ParamApp].

   Although it might seem tempting to fuse these constructors into one, one must
   not do so. Indeed, every application must have sort [*], whereas a variable
   can have an arbitrary sort. Therefore, a variable should not be viewed as an
   application of a variable to zero arguments.

   Before anonymous rules have been eliminated, a parameter can also be an
   anonymous rule, represented as a list of branches. *)
and parameter =
  | ParamVar of (symbol[@opaque]) located
  | ParamApp of (symbol[@opaque]) located * parameters
  | ParamAnonymous of parameterized_branch located list located

and parameters = parameter list
(* -------------------------------------------------------------------------- *)

and producer = (identifier[@opaque]) located * parameter * (attributes[@opaque])
(**A producer is a pair of identifier and a parameter. In concrete syntax, it
   could be [e = expr], for instance. The identifier [e] is always present. (A
   use of the keyword [$i] in a semantic action is turned by the lexer and
   parser into a reference to an identifier [_i].) A producer carries a number
   of attributes. *)
(* -------------------------------------------------------------------------- *)

and parameterized_branch = {
  (* pb_position         : range; *)
  (*[menhir-lsp] lifted [pb_position : range] to outer located *)
  pb_producers : producer located list;
      (**The producers. ([menhir-lsp]: made located) *)
  pb_action : action;  (**The semantic action. ([menhir-lsp]: made located) *)
  pb_prec_annotation : prec_annotation;  (**An optional [%prec] annotation. *)
  pb_production_level : production_level; [@opaque]
      (**The branch's production level. *)
  pb_attributes : attributes; [@opaque]
      (**The attributes attached with this branch. *)
}
(**A branch (the right-hand side of a production) is a sequence of producers
   followed with a semantic action. *)
(* -------------------------------------------------------------------------- *)

and parameterized_rule = {
  pr_public : bool;  (**Is the [%public] keyword present? *)
  pr_inline : bool;  (**Is the [%inline] keyword present? *)
  pr_nt : nonterminal located;
      (**The name of the nonterminal symbol that is being defined. *)
  (* pr_positions  : ranges; *)
  (* [menhir-lsp] removed pr_positions field in favor of located values. *)
  pr_attributes : attributes; [@opaque]
      (**Attributes attached with this nonterminal symbol. *)
  pr_parameters : symbol located list;
      (**The parameters of this nonterminal symbol. *)
  pr_branches : parameterized_branch located list;  (**The productions. *)
}
(**A rule is the definition of a nonterminal symbol.

   A rule has a header and several branches (productions). *)
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(**A declaration. (Only before joining.) *)
and declaration =
  | DCode of string located  (**Raw OCaml code. *)
  | DParameter of string located  (**Raw OCaml functor parameter. *)
  | DToken of ocamltype option * terminal * alias * (attributes[@opaque])
      (**Terminal symbol (token) declaration. *)
  | DStart of nonterminal  (**Start symbol declaration. *)
  | DTokenProperties of terminal * associativity * precedence_level
      (**Priority and associativity declaration. *)
  | DType of ocamltype * parameter  (**Type declaration. *)
  | DGrammarAttribute of (attribute[@opaque])
      (**Grammar-level attribute declaration. *)
  | DSymbolAttributes of parameter list * (attributes[@opaque])
      (**Attributes shared among multiple symbols, i.e., [%attribute]. *)
  | DOnErrorReduce of parameter * on_error_reduce_level
      (**On-error-reduce declaration. *)
(* -------------------------------------------------------------------------- *)

and partial_grammar = {
  pg_filename : filename;
  pg_postlude : string located option;
  pg_declarations : declaration located list;
  pg_rules : parameterized_rule located list;
}
(**A partial grammar. (Only before joining.) *)

and 'a located = 'a Located.located = { p : range; [@opaque] v : 'a }
[@@deriving
  visitors { name = "ast_map"; variety = "map"; polymorphic = true },
  visitors { name = "ast_reduce"; variety = "reduce"; polymorphic = true },
  visitors { name = "ast_iter"; variety = "iter"; polymorphic = true }]

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

type grammar = {
  p_preludes : string located list;
  p_postludes : string located list;
  p_parameters : string located list;
  p_start_symbols : range StringMap.t;
  p_types : (parameter * ocamltype located) list;
  p_tokens : properties StringMap.t;
  p_on_error_reduce : (parameter * on_error_reduce_level) list;
  p_grammar_attributes : attributes;
  p_symbol_attributes : (parameter list * attributes) list;
  p_rules : parameterized_rule StringMap.t;
}
(**A grammar. (Only after joining.)

   The differences with partial grammars (above) are as follows: 1. the file
   name is gone (there could be several file names, anyway). 2. there can be
   several postludes. 3. declarations are organized by kind: preludes,
   postludes, functor %parameters, %start symbols, %types, %tokens,
   %on_error_reduce, grammar attributes, %attributes. 4. rules are stored in a
   map, indexed by symbol names, instead of a list. 5. token aliases have been
   replaced with ordinary named terminal symbols. *)

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(**The new rule syntax. *)

(**In the user's eyes, the new rule syntax replaces (or complements) the old
   rule syntax, which corresponds to the types [parameter], [producer],
   [parameterized_branch], and [parameterized_rule] above. *)

(**Internally, the new rule syntax is translated down to the old rule syntax;
   see the module [NewRuleSyntax]. This is done on the fly during parsing. *)

(**A pattern. See the manual. *)
type pattern =
  | SemPatVar of identifier located
  | SemPatWildcard
  | SemPatTilde of range
  | SemPatTuple of pattern list

type raw_action =
  [ `DollarsDisallowed | `DollarsAllowed ] -> identifier option array -> action
(**The ugly type [raw_action] is produced by the lexer for an [ACTION] token. *)

type expression = choice_expression located
(**A toplevel expression is a choice expression. *)

(**A choice expression is a list of branches. *)
and choice_expression = EChoice of branch list

(**A branch is a sequence expression and an ugly [production_level]. *)
and branch = Branch of seq_expression * production_level

and seq_expression = raw_seq_expression located

(**A sequence is either a cons [p = e1; e2] or a lone symbol expression [e] or a
   semantic action. *)
and raw_seq_expression =
  | ECons of pattern * symbol_expression * seq_expression
  | ESingleton of symbol_expression
  | EAction of extended_action * prec_annotation * attributes

(**A symbol expression is a symbol, possibly accompanied with actual parameters
   and attributes. *)
and symbol_expression =
  | ESymbol of symbol located * expression list * attributes

(**A semantic action is either traditional { ... } or point-free.
   There are two forms of point-free actions, <> and <id>.
   In the latter case, [id] is an OCaml identifier. *)
and extended_action =
  | XATraditional of raw_action
  | XAPointFree of string located option

type rule = {
  rule_public : bool;  (**Is the [%public] keyword present? *)
  rule_inline : bool;  (**Is the [%inline] keyword present? *)
  rule_lhs : symbol located;
      (**The name of the nonterminal symbol that is being defined. *)
  rule_attributes : attributes;
      (**Attributes attached with this nonterminal symbol. *)
  rule_formals : symbol located list;
      (**The parameters of this nonterminal symbol. *)
  rule_rhs : expression;  (**The productions. *)
}
(**The type [rule] in the new rule syntax corresponds roughly to the type
   [parameterized_rule] in the old rule syntax. *)
