open Utils
open MenhirSyntax
open PPrint

include Comment_location.Make (struct
  type syntax = Syntax.main

  include Located
  include Range
end)

class formatter ~(notify_back : notify_back) ~(doc : Text_document.t) =
  let open Syntax in
  let _ = (notify_back, doc) in
  object (self)
    inherit [_] ast_reduce as super
    method zero = empty
    method plus = ( ^^ )

    method private with_located :
        'env 'a. ('a -> document) -> 'a located -> document =
      fun f b -> self#visit_located (fun _ v -> f v) () b

    method! visit_located visit_v env ({ comment; _ } as located) =
      let comments =
        optional (separate_map hardline (Located.value >> text)) comment
      in
      group @@ (comments // super#visit_located visit_v env located)

    method! visit_partial_grammar =
      fun _ { pg_postlude = _; pg_declarations; pg_rules; _ } ->
        separate_map
          (hardline ^^ break 1)
          (self#with_located (super#visit_declaration ()))
          pg_declarations
        //// text "%%"
        //// separate_map
               (hardline ^^ break 1)
               (self#with_located (self#visit_parameterized_rule ()))
               pg_rules

    method! visit_DTokenProperties =
      fun _ located associativity _precedence_level ->
        super#visit_associativity () associativity
        ^-^ self#visit_loctext located

    method! visit_NonAssoc _ = text "%nonassoc"
    method! visit_LeftAssoc _ = text "%left"
    method! visit_RightAssoc _ = text "%right"

    method! visit_DStart =
      fun _ located -> text "%start" ^-^ self#visit_loctext located

    method! visit_DCode =
      fun _ located ->
        surround 2 1 (text "%{") (self#visit_loctext located) (text "%}")

    method! visit_DType =
      fun _ ocamltype parameter ->
        text "%type"
        ^-^ self#visit_ocamltype () ocamltype
        ^-^ super#visit_parameter () parameter

    method! visit_ParamAnonymous = fun _ _located -> text "TODO"
    method! visit_ParamVar = fun _ located -> self#visit_loctext located

    method! visit_ParamApp =
      fun _ located parameters ->
        self#visit_loctext located
        ^^ surround 2 0 lparen
             (separate_map (text ",") (self#visit_parameter ()) parameters)
             rparen

    method! visit_DToken =
      fun _ ocamltype name alias _attributes ->
        text "%token"
        ^-^ optional (self#visit_ocamltype ()) ocamltype
        ^-^ self#with_located (self#visit_terminal ()) name
        ^-^ self#visit_alias () alias

    method! visit_ocamltype =
      fun _ ocamltype ->
        surround 2 0 langle (super#visit_ocamltype () ocamltype) rangle

    method! visit_Declared = self#visit_terminal >> self#with_located
    method! visit_Inferred = self#visit_terminal
    method! visit_alias _ = optional @@ self#visit_loctext
    method private visit_loctext = self#with_located text
    method! visit_terminal _ = text
    method! visit_nonterminal _ = text

    method! visit_parameterized_rule =
      fun _
          {
            pr_public;
            pr_inline;
            pr_nt;
            pr_attributes = _;
            pr_parameters;
            pr_branches;
          } ->
        prefix 2 1
          (if_ pr_public (text "%public")
          ^-^ if_ pr_inline (text "%inline")
          ^-^ self#visit_loctext pr_nt
          ^^ self#visit_rule_args pr_parameters
          ^^ colon)
          (separate_map (break 1)
             (self#with_located @@ self#visit_parameterized_branch ())
             pr_branches)

    method private visit_rule_args =
      surround_separate_map 2 1 empty  lparen (break 1) rparen self#visit_loctext

    method! visit_parameterized_branch =
      fun _
          {
            pb_producers;
            pb_action;
            pb_prec_annotation;
            pb_production_level = _;
            pb_attributes = _;
          } ->
        bar
        ^-^ separate_map (blank 1)
              (self#with_located (self#visit_producer ()))
              pb_producers
        ^-^ self#with_located (self#visit_action ()) pb_action
        ^-^ self#visit_prec_annotation () pb_prec_annotation

    method! visit_action _ Action.{ expr; _ } =
      match expr with
      | IL.ETextual located ->
          self#with_located
            (fun v -> surround 2 1 lbrace (v |> String.trim |> text) rbrace)
            located
      | _ -> text ""

    method! visit_prec_annotation _ =
      optional @@ fun p ->
      text "%prec" ^-^ self#with_located self#visit_loctext p
  end
