open Utils
open Loc
open M.Syntax

let process_symbols (grammar : partial_grammar) :
    M.FrontTypes.symbol located list =
  (* let symbols = ref [] in *)
  let f = L.flat_map in
  let rec visit_branch (pb : parameterized_branch) =
    f visit_producer pb.pb_producers @ O.to_list pb.pb_prec_annotation
  and visit_producer ((ide, par, _) : producer) = ide :: visit_parameter par
  and visit_parameter (par : parameter) =
    match par with
    | ParamVar sym -> [ sym ]
    | ParamApp (sym, pars) -> sym :: f visit_parameter pars
    | ParamAnonymous { p = _; v = branches } -> f visit_branch branches
  and visit_decl ({ p; v } : declaration located) =
    match v with
    | DToken (_, sym, _, _) | DTokenProperties (sym, _, _) | DStart sym ->
        [ locate p sym ]
    | DType (_, par) | DOnErrorReduce (par, _) -> visit_parameter par
    | DSymbolAttributes (_, _) | DGrammarAttribute _ | DCode _ | DParameter _ ->
        []
  and visit_parameterized_rule (p : parameterized_rule) =
    (p.pr_nt :: p.pr_parameters) @ f visit_branch p.pr_branches
  in
  f visit_decl grammar.pg_declarations
  @ f visit_parameterized_rule grammar.pg_rules
