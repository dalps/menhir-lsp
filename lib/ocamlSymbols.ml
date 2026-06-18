open Ppxlib
open Utils
module T = Ppxlib.Ast_traverse
module Names = Set.Make (String)

let get_fvars ast =
  let v =
    object (self)
      inherit [Names.t * string loc list] T.fold as super

      method! pattern ptn (env, names) =
        (* It would be much easier if ppxlib provided dedicated visitors for each constructor and we could simply visit Ppat_var, where the information lies. Oh well :/ *)
        let rec go env ptn =
          match ptn.ppat_desc with
          | Ppat_var name -> Names.add name.txt env
          | Ppat_tuple ptns | Ppat_array ptns ->
              L.fold_left (fun acc ptn -> Names.union acc (go env ptn)) env ptns
          | Ppat_record (assoc, _) ->
              L.fold_left
                (fun acc (_name, ptn) -> Names.union acc (go env ptn))
                env assoc
          | Ppat_alias (ptn, name) -> Names.add name.txt (go env ptn)
          | Ppat_or (p1, p2) -> Names.union (go env p1) (go env p2)
          | Ppat_constraint (p, _)
          | Ppat_variant (_, Some p)
          | Ppat_construct (_, Some (_, p))
          | Ppat_lazy p ->
              go env p
          | Ppat_type _ | Ppat_unpack _ | Ppat_exception _ | Ppat_extension _
          | Ppat_open (_, _)
          | Ppat_constant _ | Ppat_any
          | Ppat_interval (_, _)
          | _ ->
              env
        in
        (go env ptn, names)

      (* method! function_param_desc function_param_desc (bound, names) = _ *)

      (* method! letop letop (bound, names) =
        let bound_by_let, _ = self#pattern letop.let_.pbop_pat (bound, names) in
        let bound_by_ands =
          List.fold_left
            (fun acc bop ->
              let bba, _ = self#pattern bop.pbop_pat (bound, names) in
              Names.union bba acc)
            bound_by_let letop.ands
        in
        self#expression letop.body (bound_by_ands, names) *)

      method! longident_loc lid (env, names) =
        ( env,
          if Names.mem (Longident.name lid.txt) env then names
          else Loc.map ~f:Longident.name lid :: names )
    end
  in
  v#structure ast (Names.empty, []) |> snd
