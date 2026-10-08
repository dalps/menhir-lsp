open Menhirformat_lib
open Utils
open Config
module Mly = MenhirSyntax
module Fmt_mly = Menhir
module Mll = OcamllexSyntax
module Fmt_mll = Ocamllex

(* Reference: https://gallium.inria.fr/~fpottier/visitors/manual.pdf#subsection.2.10 *)

class mly_equal_vtor =
  object
    inherit [_] Mly.Syntax.ast_iter2 as super

    method! visit_located visit_v env loc1 loc2 =
      (* Skip comments and locations. *)
      log "comparing nodes %a %a" Range.pp_lexing loc1.p Range.pp_lexing loc2.p;
      visit_v env loc1.v loc2.v;
      log "ok"

    (* I can also label it [@opaque] in Syntax.ml *)
    method! visit_production_level _ _ _ = ()
  end

class mll_equal_vtor =
  object
    inherit [_] Mll.Syntax.ast_iter2 as super

    method! visit_located visit_v env loc1 loc2 =
      (* Skip comments and locations. *)
      visit_v env loc1.v loc2.v
  end

let test' compare a b =
  try
    compare () a b;
    true
  with VisitorsRuntime.StructuralMismatch -> false

let test parse format compare inp =
  try
    let a = inp |> parse |> R.get_exn in
    let b = inp |> format |> R.get_exn |> parse |> R.get_exn in
    compare () a b;
    true
  with R.Get_error | VisitorsRuntime.StructuralMismatch -> false

let test_mly_string =
  test
    (Mly.Main.load_grammar_from_contents 0 "")
    (Fmt_mly.format_string ~config:Config.default_config)
    (new mly_equal_vtor)#visit_partial_grammar

let test_mll_string =
  test Mll.Main.parse_string
    (Fmt_mly.format_string ~config:Config.default_config)
    (new mll_equal_vtor)#visit_lexer_definition
