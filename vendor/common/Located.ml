(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Range

type 'a located =
{ p : range; v : 'a; mutable comment: comments }

and comments = comment list option

and comment = {text: string; relpos: Menhir_lsp_lib.Utils.relpos}


let[@inline] locate ?comment p v =
  { p; v; comment }

let[@inline] position { p; _ } =
  p

let[@inline] value { v; _ } =
  v

let map f ({ v; _ } as loc) =
  { loc with v = f v }

let iter (f : 'a -> 'b) ({ v; _ } : 'a located) =
  f v

let parenthesize { p; v; comment } =
  locate ?comment (Range.decrement p) ("(" ^ v ^ ")")

let startp { p; _ } = Range.startp p

let endp { p; _ } = Range.endp p
