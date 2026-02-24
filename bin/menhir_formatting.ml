open Utils
open MenhirSyntax
open PPrint

include Comment_location.Make (struct
  type syntax = Syntax.main

  include Located
  include Range
end)

(* let v =
  object
    inherit [_] Syntax.ast_reduce
    method zero : document = empty
    method plus = ( ^^ )
  end *)

class formatter ~(notify_back : notify_back) ~(doc : Text_document.t) =
  let open Syntax in
  let _ = (notify_back, doc) in
  object (self)
    inherit [_] ast_reduce as super
    method zero : document = empty
    method plus = ( ^^ )

    method private with_located :
        'env 'a. ('a -> document) -> 'a located -> document =
      fun f b -> self#visit_located (fun _ v -> f v) () b

    method! visit_located visit_v env =
      render_located (super#visit_located visit_v env)

    method! visit_partial_grammar =
      fun _ { pg_postlude = _; pg_declarations; pg_rules; _ } ->
        self#visit_declarations pg_declarations
        //// text "%%"
        //// separate_map (hardline ^^ break 1) (self#visit_rule ()) pg_rules

    method private visit_declarations (decls : declaration located list) :
        document =
      let open DBuckets in
      (* ugly *)
      let buckets =
        L.fold_right
          (fun (d : declaration located) (acc : DBuckets.t) ->
            (* used destruct + multiline editing here *)
            match d.v with
            | DCode _ -> { acc with dCode = d :: acc.dCode }
            | DParameter _ -> { acc with dParameter = d :: acc.dParameter }
            | DToken _ -> { acc with dToken = d :: acc.dToken }
            | DStart _ -> { acc with dStart = d :: acc.dStart }
            | DTokenProperties _ ->
                { acc with dTokenProperties = d :: acc.dTokenProperties }
            | DType _ -> { acc with dType = d :: acc.dType }
            | DGrammarAttribute _ ->
                { acc with dGrammarAttribute = d :: acc.dGrammarAttribute }
            | DSymbolAttributes _ ->
                { acc with dSymbolAttributes = d :: acc.dSymbolAttributes }
            | DOnErrorReduce _ ->
                { acc with dOnErrorReduce = d :: acc.dOnErrorReduce })
          decls DBuckets.init
      in
      let v =
        object
          inherit [_] buckets_reduce
          method zero : document = empty
          method plus = ( //// )

          method! visit_bucket _ =
            separate_map hardline
              (self#with_located (super#visit_declaration ()))
        end
      in
      v#visit_t () buckets

    method! visit_DTokenProperties =
      fun _ located associativity _precedence_level ->
        super#visit_associativity () associativity
        ^-^ separate_map (break 1) self#visit_loctext located

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
      fun _ brances ->
        ifflat empty barspace
        ^^ self#with_located self#visit_old_rule_branches brances

    method! visit_ParamVar = fun _ located -> self#visit_loctext located

    method! visit_ParamApp =
      fun _ located parameters ->
        self#visit_loctext
          located (* ^^ align --- only if subtree is ParamAnon *)
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

    method! visit_old_rule =
      fun _
          {
            pr_public;
            pr_inline;
            pr_nt;
            pr_attributes;
            pr_parameters;
            pr_branches;
          } ->
        (if_ pr_public ~then_:(text "%public")
        ^-^ if_ pr_inline ~then_:(text "%inline")
        ^-^ self#visit_loctext pr_nt
        ^^ self#visit_rule_args pr_parameters
        ^^ colon)
        ^^ nest 2
             (hardline ^^ twice space
             ^^ self#visit_old_rule_branches pr_branches
             ^/^ self#visit_attributes () pr_attributes)

    method private visit_rule_args =
      surround_separate_map 2 0 empty lparen
        (comma ^^ break 1)
        rparen self#visit_loctext

    method private visit_old_rule_branches branches =
      separate_map
        (break 1 ^^ barspace)
        (self#with_located (self#visit_parameterized_branch ()))
        branches

    method! visit_parameterized_branch =
      fun _
          {
            pb_producers;
            pb_action;
            pb_prec_annotation;
            pb_production_level = _;
            pb_attributes;
          } ->
        nest 2
        @@ prefix 2 1
             (flow_map (break 1)
                (self#with_located (self#visit_producer ()))
                pb_producers)
        @@ separate (break 1)
             [
               self#with_located (self#visit_action ()) pb_action;
               self#visit_prec_annotation () pb_prec_annotation;
               self#visit_attributes () pb_attributes;
             ]

    method! visit_action _ Action.{ expr; _ } =
      let content =
        match expr with
        | IL.ETextual located ->
            self#with_located
              (fun v ->
                Ocamlformat_client.format v
                |> R.get_or ~default:v |> String.trim |> arbitrary_string)
              located
        | _ -> text ""
      in
      surround 2 1 lbrace content rbrace

    method! visit_prec_annotation _ =
      optional @@ fun p ->
      text "%prec" ^-^ self#with_located self#visit_loctext p

    method! visit_new_rule =
      fun _
          {
            pr_public;
            pr_inline;
            pr_nt;
            pr_attributes;
            pr_parameters;
            pr_branches;
          } ->
        (if_ pr_public ~then_:(text "%public")
        ^-^ text "let"
        ^-^ (self#visit_loctext pr_nt ^^ self#visit_rule_args pr_parameters)
        ^-^ if_ pr_inline ~then_:(text "==") ~else_:(text ":="))
        ^^ nest 2
             (hardline ^^ twice space
             ^^ self#visit_expression () pr_branches
             ^/^ self#visit_attributes () pr_attributes)

    method! visit_SemPatTilde _ _ = tilde
    method! visit_SemPatWildcard _ = underscore
    method! visit_SemPatTuple _ = separate_map comma (self#visit_pattern ())
    method! visit_SemPatVar _ = self#visit_loctext

    method! visit_XAPointFree _ a =
      surround 2 0 langle (optional self#visit_loctext a) rangle

    method! visit_XATraditional _ _a =
      (* self#visit_action () (a `DollarsDisallowed [||]) *)
      (* raises Invalid_argument *)
      text "{ action }"

    method! visit_EChoice _ branches =
      separate_map
        (break 1 ^^ barspace)
        ((* self#with_located *) self#visit_branch ())
        branches

    method! visit_ECons _ pattern symbol_expression seq_expression =
      flow (break 1)
        [
          self#visit_pattern () pattern;
          equals;
          self#visit_symbol_expression () symbol_expression;
          semi;
          self#visit_seq_expression () seq_expression;
        ]

    method! visit_EAction _ extended_action prec_annotation attributes =
      flow (break 1)
        [
          self#visit_extended_action () extended_action;
          self#visit_prec_annotation () prec_annotation;
          self#visit_attributes () attributes;
        ]

    method! visit_ESymbol _ located list attributes =
      self#visit_loctext located (* ^^ align --- only if subtree is ParamAnon *)
      ^^ surround 2 0 lparen
           (separate_map (text ",") (self#visit_expression ()) list)
           rparen
      ^/^ self#visit_attributes () attributes
  end
