open Menhir_lsp_lib
open Menhirformat_lib
open Utils
open Config
module Mly = MenhirSyntax
module Fmt_mly = Menhir
module Mll = OcamllexSyntax
module Fmt_mll = Ocamllex

(* Reference: https://gallium.inria.fr/~fpottier/visitors/manual.pdf#subsection.2.10 *)

(** The visitor that decides whether two Menhir syntax trees are equivalent.
    It ignores OCaml fragments. *)
class mly_equal_vtor =
  let open Mly.Syntax in
  let open Mly.Located in
  (* Skip comments and locations. *)
  let visit_located visit_v env loc1 loc2 =
    visit_v env loc1.v loc2.v;
    log ~debug:false "compared nodes %a %a" Range.pp_lexing loc1.p
      Range.pp_lexing loc2.p
  in

  object (self)
    inherit [_] Mly.Syntax.ast_iter2 as super
    method! visit_located = visit_located

    (* Note: I can also label it [@opaque] in Syntax.ml if I don't plan to visit for other purposes.

    Not sure whether skipping this and [precedence_level] is cheating... *)
    method! visit_production_level _ _ _ = ()

    method! visit_partial_grammar _ pg1 pg2 =
      try
        self#visit_declarations pg1.pg_declarations pg2.pg_declarations;
        List.iter2 (self#visit_rule ()) pg1.pg_rules pg2.pg_rules
      with
      | VisitorsRuntime.StructuralMismatch ->
          log "\x1b[1;31m[Structural mismatch]\x1b[0m";
          raise VisitorsRuntime.StructuralMismatch
      | Invalid_argument msg ->
          log "\x1b[1;31m[Rule mismatch] left: %d vs right: %d %s\x1b[0m"
            (L.length pg1.pg_rules) (L.length pg2.pg_rules) msg;
          raise VisitorsRuntime.StructuralMismatch

    (* menhirformat might change the order of the declarations, what's important is that the existing declarations are preserved and none is removed or added. We delegate this check to a visitor of a record of declaration buckets. Declaration buckets are implemented with lists, so we're also checking that the formatter preserves the order among declarations of the same kind. *)
    method private visit_declarations (decls1 : declaration located list)
        (decls2 : declaration located list) : unit =
      let open DBuckets in
      let v =
        object
          inherit [_] buckets_iter2
          method! visit_located = visit_located

          (* See note on [visit_production_level] *)
          method! visit_precedence_level _ _ _ = ()
        end
      in
      v#visit_t () (from_declarations decls1) (from_declarations decls2)
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
