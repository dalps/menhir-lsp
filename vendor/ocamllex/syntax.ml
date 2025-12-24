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

type location = range

type regular_expression =
  | Epsilon
  | Characters of Cset.t
  | Eof
  | Sequence of regular_expression * regular_expression
  | Alternative of regular_expression * regular_expression
  | Repetition of regular_expression
  | Ref of string located (* added by [menhir-lsp] *)
  | Bind of regular_expression * string located

type ('arg, 'action) entry = {
  name : string located;
  shortest : bool;
  args : 'arg;
  clauses : (regular_expression * 'action) list;
}

type lexer_definition = {
  header : location;
  entrypoints : (string located list, location) entry list;
  trailer : location;
  refill_handler : location option;
  named_regexps : (string located * regular_expression) list;
}

let named_regexps : (string, Range.range * regular_expression) Hashtbl.t =
  Hashtbl.create 13
