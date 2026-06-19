open Ppxlib
open Utils
module T = Ppxlib.Ast_traverse
module Names = Set.Make (String)

let get_fvars ast =
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
