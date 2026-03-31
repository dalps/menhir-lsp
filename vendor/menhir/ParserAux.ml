(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Located
open Syntax
(* open Attribute *)

exception ParserError of string located

(* [menhir-lsp] types moved to Syntax *)

let new_precedence_level =
  let c = ref 0 in
  fun (pos1, pos2) ->
    incr c;
    let range = Range.make (pos1, pos2) in
    PrecedenceLevel (locate range (InputFile.current (), !c))

let new_production_level =
  let c = ref 0 in
  fun () ->
    incr c;
    ProductionLevel (InputFile.current (), !c)

let new_on_error_reduce_level = new_production_level
(* the counter is shared with [new_production_level],
       but this is irrelevant *)

module IdSet = struct
  include Set.Make (struct
    type t = identifier located

    let compare id1 id2 = compare (value id1) (value id2)
  end)

  (* Symmetric difference. *)
  let xor s1 s2 = union (diff s1 s2) (diff s2 s1)
end

let defined_identifiers ({ v = ido, _, _; _ } : early_producer located)
    (accu : IdSet.t) : IdSet.t =
  Option.fold ~none:accu ~some:(Fun.flip IdSet.add accu) ido

let defined_identifiers (producers : early_producers) =
  List.fold_right defined_identifiers producers IdSet.empty

let check_production_group (right_hand_sides : early_productions) =
  match right_hand_sides with
  | [] ->
      (* A production group cannot be empty. *)
      assert false
  | { v = producers, _, _; _ } :: right_hand_sides -> (
      let ids = defined_identifiers producers in
      right_hand_sides
      |> List.iter @@ fun { v = producers, _, _; _ } ->
         let ids' = defined_identifiers producers in
         try
           let id = IdSet.choose (IdSet.xor ids ids') in
           raise
           @@ ParserError
                (locate (position id)
                @@ Printf.sprintf
                     "two productions that share a semantic action must define \
                      exactly\n\
                      the same identifiers. Here, \"%s\" is defined\n\
                      in one production, but not in all of them."
                     (value id))
         with Not_found -> ())

(* [normalize_producer i p] assigns a name of the form [_i]
   to the unnamed producer [p]. *)
let normalize_producer i
    Located.{ v = opt_identifier, parameter, attrs; p = pos; _ } =
  let id =
    match opt_identifier with
    | Some id -> id
    | None -> locate pos ("_" ^ string_of_int (i + 1))
  in
  locate pos (id, parameter, attrs)

let normalize_producers (producers : early_producers) : producer located list =
  List.mapi normalize_producer producers

let override pos o1 o2 =
  match (o1, o2) with
  | Some _, Some _ ->
      raise
      @@ ParserError
           (locate pos "this production carries two %%prec declarations.")
  | None, Some _ -> o2
  | _, None -> o1

(* Only unnamed producers can be referred to using positional identifiers.
   Besides, such positions lie in the interval [1 .. length producers].
   The output array [p] is such that [p.(idx) = Some x] if [idx] must be
   referred to using [x], not [$(idx + 1)]. *)
let producer_names (producers : early_producers) =
  producers
  |> List.map (fun { v = oid, _, _; _ } -> Option.map value oid)
  |> Array.of_list

let validate_pointfree_action (fragment : string located) =
  let s = value fragment in
  if Lexpointfree.validate_pointfree_action (Lexing.from_string s) then
    Some fragment (* a non-empty action *)
  else None (* an empty action *)

(* Test whether a string is a valid OCaml lowercase identifier. *)

(* [x] should be a LID, UID, or QID, produced by Menhir's main lexer.
   Testing its first character should suffice, but we are more cautious
   and validate it thoroughly. *)

let valid_ocaml_identifier (x : identifier located) : bool =
  Lex.valid_ocaml_identifier (Lexing.from_string (value x))

let dollars = ref `DollarsAllowed
