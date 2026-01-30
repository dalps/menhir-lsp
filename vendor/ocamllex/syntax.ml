(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* This apparently useless implementation file is in fact required
   by the pa_ocamllex syntax extension *)

(* The shallow abstract syntax *)

open Located
open Range

exception SyntaxError of string located

type location = unit located

and character_class_syntax =
  | Wildcard of unit located
  | Character of int located
  | Range of int located * int located
  | Union of character_class_syntax located * character_class_syntax located
  | Complement of character_class_syntax located

and regular_expression_syntax =
  | Epsilon of unit located
  | CharSet of character_class_syntax located
  | String of string located
  | EOF of unit located
  | Seq of regular_expression_syntax located * regular_expression_syntax located
  | Alt of regular_expression_syntax located * regular_expression_syntax located
  | CharSetDifference of
      regular_expression_syntax located * regular_expression_syntax located
  | Rep of regular_expression_syntax located
  | Rep1 of regular_expression_syntax located
  | Ref of string located
  | Group of regular_expression_syntax located
  | Option of regular_expression_syntax located
  | As of regular_expression_syntax located * string located

and named_regexp = {
  name : string located;
  regexp : regular_expression_syntax located;
}

(* [menhir-lsp] To simplify the visitor, I fixed the type parameters ['arg] and ['action] to [string located] and [location], respectively. *)
and entry = {
  name : string located;
  shortest : bool located;
  args : string located list;
  clauses : (regular_expression_syntax located * location) list;
}

and lexer_definition = {
  header : location option;
  entrypoints : entry located list;
  trailer : location option;
  refill_handler : location option;
  named_regexps : named_regexp list;
}

and 'a located = 'a Located.located = { p : range; [@opaque] v : 'a }
[@@deriving
  visitors { name = "syntax_map"; variety = "map"; polymorphic = true },
  visitors { name = "syntax_reduce"; variety = "reduce"; polymorphic = true },
  visitors { name = "syntax_iter"; variety = "iter"; polymorphic = true }]

(* Also tried:

[@@deriving
  visitors
    {
      name = "regexp_visitor";
      variety = "iter";
      ancestors = [ "located_visitor" ];
    }]

  but produces warnings.
*)

type regular_expression =
  | Epsilon
  | Characters of Cset.t
  | Eof
  | Sequence of regular_expression * regular_expression
  | Alternative of regular_expression * regular_expression
  | Repetition of regular_expression
  | Bind of regular_expression * string located

(* type 'a named = { name : string located; v : 'a } *)

let named_regexps :
    (string, named_regexp located * regular_expression) Hashtbl.t =
  Hashtbl.create 13
