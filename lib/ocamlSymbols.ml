open Ppxlib
open Utils
module T = Ppxlib.Ast_traverse
module Names = Set.Make (String)

let range_of_ppxlocation ~(from : Lexing.position)
    ({ loc_start; loc_end; _ } : Ppxlib.Location.t) :
    Lexing.(position * position) =
  let debug_pos ({ pos_fname; pos_lnum; pos_bol; pos_cnum } : Lexing.position) =
    spr "{lnum = %d; bol = %d; cnum = %d}" pos_lnum pos_bol pos_cnum
  in
  epr "loc_start: %s, loc_end: %s\n" (debug_pos loc_start) (debug_pos loc_end);
  let ( + ) p1 p2 =
    Lexing.
      {
        p2 with
        pos_lnum = p1.pos_lnum + p2.pos_lnum - 1;
        pos_bol =
          (if p2.pos_lnum = 1 then (
             assert (p2.pos_bol = 0);
             p1.pos_bol)
           else p1.pos_cnum + p2.pos_bol);
        pos_cnum = p1.pos_cnum + p2.pos_cnum;
      }
  in
  (from + loc_start, from + loc_end)

let get_fvars ast : string loc list =
  let v =
    object (self)
      inherit [Names.t * string loc list] T.fold as super

      (* This object would be slightly less boilerplate-y if ppxlib provided dedicated visitors for each AST constructor, thus we could visit [Ppat_var] or [Pexp_record] directly. Oh well :/ *)

      method! pattern_desc ptn ((env, names) as acc) =
        match ptn with
        | Ppat_var name -> (Names.add name.txt env, names)
        | desc -> super#pattern_desc desc acc

      method! value_binding vb =
        (* The default implementation visits the pattern first, and we would shadow some of the names of the bound expression. However (TODO), if the binding is recursive, we should preserve the original order and let the shadowing occur. *)
        self#expression vb.pvb_expr >> self#pattern vb.pvb_pat

      method! binding_op bop =
        self#expression bop.pbop_exp >> self#pattern bop.pbop_pat

      method! expression_desc expr acc =
        (* Some lids, such as unpunned record keys ({ k = v; _ }), should be excluded from the result. *)
        match expr with
        | Pexp_record (assoc, expr_with) ->
            acc
            |> self#list
                 (fun (k, v) acc ->
                   (* let acc = self#longident_loc k acc in *)
                   self#expression v acc)
                 assoc
            |> self#option self#expression expr_with
        | expr -> super#expression_desc expr acc

      method! longident_loc lid (env, names) =
        ( env,
          let name = Longident.name lid.txt in
          if Names.mem name env then names
          else Loc.map ~f:Longident.name lid :: names )
    end
  in
  v#structure ast (Names.empty, []) |> snd

(** Returns a top-down stack of ranges that contain [pos] for the given OCaml
    [ast].

    [from] is the start offset of the embedded action and is added to every
    visited location.

    This visitor relies on the assumption that the locatoin field of a node is
    always visited after a syntax node's content. *)
let get_ranges_for_pos pos from ast : Range.t list =
  let v =
    object
      inherit [Range.t list] T.fold as super

      (* [loc] just wraps leaves nodes, identifiers and labels, unlike our AST approach where we wrap every node with [located]. Hence, overriding this method is not very useful to us. *)
      (* method! loc visit_a loc =
        let range =
          loc
          |> located_of_ppxloc ~from:(fst action_text.p)
          |> MenhirSyntax.Located.position
          |> Range.of_lexical_positions
        in
        if Position.is_inside pos range then (
          parent_ref :=
            O.some
            @@ SelectionRange.create ?parent:!parent_ref ~range ();
          visit_a loc.txt) *)

      method! location loc acc =
        let range =
          loc |> range_of_ppxlocation ~from |> Range.of_lexical_positions
        in
        epr "[ocaml ranges] checking %s" (Range.show range);
        if Position.is_inside pos range then (
          epr "[ocaml ranges] entering %s\n" (Range.show range);
          range :: acc)
        else acc
    end
  in
  v#structure ast []
