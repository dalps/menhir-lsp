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
open Located
open Syntax

type t = parameter

let var x =
  ParamVar (locate dummy x)

let apply p ps =
  match ps with
  | [] ->
      ParamVar p
  | _ ->
      ParamApp (p, ps)

let destruct p =
  match p with
  | ParamVar x ->
      x, []
  | ParamApp (x, ps) ->
      x, ps
  | _ ->
      assert false

let head p =
  let x, ps = destruct p in
  assert (ps = []);
  x

let rec iter f p =
  match p with
  | ParamVar x ->
      f x
  | ParamApp (x, ps) ->
      f x;
      List.iter (Located.iter (iter f)) ps
  | _ ->
      assert false

let rec map f p =
  match p with
  | ParamVar x ->
      ParamVar (f x)
  | ParamApp (x, ps) ->
      ParamApp (f x, List.map (Located.map (map f)) ps)
  | _ ->
      assert false

let[@inline] apply' (p : parameter) (ps : parameters) : parameter =
  (* In a well-sorted grammar, only a variable can have higher sort.
     Here, [p] must have higher sort, so it must be a variable. *)
  apply (head p) ps

let rec subst f p =
  match p with
  | ParamVar x ->
      f x
  | ParamApp (x, ps) ->
      apply' (f x) (List.map (Located.map @@ subst f) ps)
  | _ ->
      assert false

let rec occurs x p =
  match p with
  | ParamVar y ->
      x = value y
  | ParamApp (y, ps) ->
      x = value y || List.exists (Located.iter @@ occurs x) ps
  | _ ->
      assert false

let rec equal x y =
  match x, y with
    | ParamVar x, ParamVar y ->
        value x = value y
    | ParamApp (p1, p2), ParamApp (p1', p2') ->
        value p1 = value p1' && List.for_all2 (fun l1 l2 -> equal l1.v l2.v) p2 p2'
    | _ ->
        false

let hash p =
  match p with
  | ParamVar x
  | ParamApp (x, _) ->
      Hashtbl.hash (value x)
  | _ ->
      assert false

let position p =
  match p with
  | ParamVar x
  | ParamApp (x, _) ->
      position x
  | ParamAnonymous bs ->
      position bs
  | ParamEBNFApp
      (p, m) -> (Located.startp p, Located.endp m)

let locate p =
  locate (position p) p

let print_modifier = function
  | MList -> "*"
  | MNonempty_list -> "+"
  | MOption -> "?"

(* [menhir-lsp] Added unhandled cases. *)
let rec print separator p =
  match p with
  | ParamVar x
  | ParamApp (x, []) ->
      value x
  | ParamApp (x, ps) ->
      Printf.sprintf "%s(%s)"
        (value x)
        (MString.separated_list (Located.iter @@ print separator) separator ps)
  | ParamEBNFApp (p, m) ->
      Printf.sprintf "%s%s"
        (print separator (value p))
        (print_modifier (value m))
  | ParamAnonymous _ -> "<anonymous rule>"
