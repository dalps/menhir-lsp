(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers a pretty-printer for the language [IL]. *)

open IL

(**[Make] offers the most general interface to the pretty-printer. *)
module Make (_ : sig
  val f : out_channel
  (**The output channel that is written to. *)

  val print_line_directives : string option
  (**[print_line_directives] controls the way we print OCaml source code
     fragments (types and semantic actions). If it is [Some dstfilename], where
     [dstfilename] is the name of the file that is being written, then we
     surround fragments with OCaml line directives of the form
     [# <line number> <filename>]. If it is [None], then we don't. *)

  (* Providing line directives allows the OCaml typechecker to report type
     errors in the .mly file, instead of in the generated .ml / .mli files.
     Line directives also affect the dynamic semantics of any [assert]
     statements contained in semantic actions: when they are provided, the
     [Assert_failure] exception carries a location in the .mly file. As a
     general rule of thumb, line directives should always be provided, except
     perhaps where we think that they decrease readability (e.g., in a
     generated .mli file). *)

  val comment : bool
  (**[comment] determines whether the comments carried by [EComment] are printed
     or erased. *)
end) : sig
  val program : program -> unit
  (**A printer of programs. *)

  val expr : expr -> unit
  (**A printer of expressions. *)

  val interface : interface -> unit
  (**A printer of interfaces. *)
end

val print_expr : out_channel -> expr -> unit
(**[print_expr] is a printer of expressions to an output channel. It does not
   print any line directives or comments. It is a special case of {!Make}. *)

val string_of_expr : expr -> string
(**[string_of_expr] is a printer of expressions to a string. It does not print
   any line directives or comments. It is a special case of {!Make}. *)
