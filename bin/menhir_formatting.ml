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
    method zero : document = empty
    method plus : document -> document -> document = ( ^^ )

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
        ^-^ separate_map (break 1) (self#visit_loctext) located

    method! visit_NonAssoc _ = text "%nonassoc"
    method! visit_LeftAssoc _ = text "%left"
    method! visit_RightAssoc _ = text "%right"

    method! visit_DStart =
      fun _ ocamltype located ->
        prefix 2 1 (text "%start")
          (optional (self#visit_ocamltype ()) ocamltype
          ^-^ flow_map (break 1) self#visit_loctext located)

    method! visit_DCode =
      fun _ located ->
        surround 2 1 (text "%{") (self#visit_loctext located) (text "%}")

    method! visit_DType =
      fun _ ocamltype parameter ->
        prefix 2 1 (text "%type")
          (self#visit_ocamltype () ocamltype
          ^-^ flow_map (break 1)
                (self#with_located @@ super#visit_parameter ())
                parameter)

    method! visit_ParamAnonymous =
      fun _ ->
        self#with_located
          (flow_map (text " | ")
             (self#with_located (self#visit_parameterized_branch ())))

    method! visit_ParamVar = fun _ located -> self#visit_loctext located

    method! visit_ParamApp =
      fun _ located parameters ->
        self#visit_loctext located
        ^^ surround 2 0 lparen
             (separate_map (text ",") (self#visit_parameter ()) parameters)
             rparen

    method! visit_DToken =
      fun _ ocamltype data ->
        prefix 2 1 (text "%token")
        @@ flow (break 1)
             [
               optional (self#visit_ocamltype ()) ocamltype;
               data
               |> flow_map (break 1)
                    (self#with_located (fun (name, alias, attributes) ->
                         flow (break 1)
                           [
                             self#with_located (self#visit_terminal ()) name;
                             self#visit_alias () alias;
                             self#visit_attributes () attributes;
                           ]));
             ]

    method! visit_ocamltype =
      fun _ ocamltype ->
        surround 2 0 langle (super#visit_ocamltype () ocamltype) rangle

    method! visit_Declared = self#visit_terminal >> self#with_located
    method! visit_Inferred = self#visit_terminal
    method! visit_alias _ = optional @@ self#visit_loctext

    method private visit_loctext : string located -> document =
      self#with_located text

    method! visit_terminal _ = text
    method! visit_nonterminal _ = text
    method! visit_identifier _ = text

    method! visit_producer =
      fun _ (ident, param, attrs) ->
        if_
          (not @@ String.starts_with ~prefix:"_" ident.v)
          ~then_:(self#with_located (self#visit_identifier ()) ident ^-^ equals)
        ^-^ self#visit_parameter () param
        ^-^ self#visit_attributes () attrs

    method! visit_attributes _ = flow_map (break 1) (self#visit_attribute ())

    method! visit_attribute _ ({ key; payload; _ } : Attribute.attribute) =
      enclose lbracket rbracket (at ^^ text key ^-^ text payload)

    method! visit_parameterized_rule =
      fun _
          {
            pr_public;
            pr_inline;
            pr_nt;
            pr_attributes;
            pr_parameters;
            pr_branches;
          } ->
        prefix 2 1
          (if_ pr_public ~then_:(text "%public")
          ^-^ if_ pr_inline ~then_:(text "%inline")
          ^-^ self#visit_loctext pr_nt
          ^^ self#visit_rule_args pr_parameters
          ^^ colon)
          (separate_map (break 1)
             (self#with_located @@ self#visit_parameterized_branch ())
             pr_branches)
        ^-^ self#visit_attributes () pr_attributes

    method private visit_rule_args =
      surround_separate_map 2 0 empty lparen (break 1) rparen self#visit_loctext

    method! visit_parameterized_branch =
      fun _
          {
            pb_producers;
            pb_action;
            pb_prec_annotation;
            pb_production_level = _;
            pb_attributes;
          } ->
        prefix 2 1 bar
        @@ separate_map (blank 1)
             (self#with_located (self#visit_producer ()))
             pb_producers
        ^-^ self#with_located (self#visit_action ()) pb_action
        ^-^ self#visit_prec_annotation () pb_prec_annotation
        ^-^ self#visit_attributes () pb_attributes

    method! visit_action _ Action.{ expr; _ } =
      let content =
        match expr with
        | IL.ETextual located ->
            log_info ~notify_back "action text: %s" located.v;
            self#with_located (fun v -> v |> String.trim |> text) located
        | _ -> text ""
      in
      surround 2 1 lbrace content rbrace

    method! visit_prec_annotation _ =
      optional @@ fun p ->
      text "%prec" ^-^ self#with_located self#visit_loctext p
  end
