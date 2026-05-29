open Utils
open MenhirSyntax
open PPrint

include Comment_location.Make (struct
  type syntax = Syntax.main

  include Located
  include Range

  (* let get_comments = Lexer.get_comments *)
end)

(* Keep this to query the reducer's type. *)
(* let v =
  object
    inherit [_] Syntax.ast_reduce
    method zero : document = empty
    method plus = ( ^^ )
  end *)

class formatter (config : Config.t) =
  let Config.{ tabsize } = config in
  let open Syntax in
  object (self)
    inherit [_] ast_reduce as super
    method zero : document = empty
    method plus = ( ^^ )

    method private with_located :
        'env 'a. ('a -> document) -> 'a located -> document =
      fun f b -> self#visit_located (fun _ v -> f v) () b

    method private with_located_debug :
        'env 'a. ('a -> document) -> 'a located -> document =
      fun f b ->
        self#visit_located (fun _ v -> f v) () b
        ^^ arbitrary_string (Range.show b.p)

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

    method! visit_DOnErrorReduce =
      fun _ parameters _on_error_reduce_level ->
        text "%on_error_reduce"
        ^-^ separate_map (break 1)
              (self#with_located @@ self#visit_parameter ())
              parameters

    method! visit_NonAssoc _ = text "%nonassoc"
    method! visit_LeftAssoc _ = text "%left"
    method! visit_RightAssoc _ = text "%right"

    method! visit_DStart =
      fun _ ocamltype located ->
        prefix tabsize 1 (text "%start")
          (optional (self#visit_ocamltype ()) ocamltype
          ^-^ flow_map (break 1) self#visit_loctext located)

    method! visit_DCode =
      fun _ ->
        self#with_located (fun code ->
            surround tabsize 1 (text "%{") (self#visit_ocaml code) (text "%}"))

    method! visit_DType =
      fun _ ocamltype parameter ->
        prefix tabsize 1 (text "%type")
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
        ^^ surround tabsize 0 lparen
             (separate_map (text ",") (self#visit_parameter_loc ()) parameters)
             rparen

    method! visit_DToken =
      fun _ ocamltype data ->
        prefix tabsize 1 (text "%token")
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
        surround tabsize 0 langle (super#visit_ocamltype () ocamltype) rangle

    method! visit_Declared _ = self#with_located self#visit_ocaml
    method! visit_Inferred _ = self#visit_ocaml
    method! visit_alias _ = optional self#visit_loctext

    method private visit_loctext : string located -> document =
      self#with_located text

    method! visit_terminal _ = text
    method! visit_nonterminal _ = text
    method! visit_identifier _ = text

    method private visit_parameter_loc _ =
      self#with_located @@ super#visit_parameter ()

    method! visit_early_producer =
      fun _ (ident, param, attrs) ->
        optional
          (fun ident ->
            self#with_located (self#visit_identifier ()) ident ^-^ equals)
          ident
        ^-^ self#visit_parameter_loc () param
        ^-^ self#visit_attributes () attrs

    method! visit_producer =
      fun _ (ident, param, attrs) ->
        if_
          (not @@ String.starts_with ~prefix:"_" ident.v)
          ~then_:(self#with_located (self#visit_identifier ()) ident ^-^ equals)
        ^-^ self#visit_parameter_loc () param
        ^-^ self#visit_attributes () attrs

    method! visit_attributes _ = flow_map (break 1) (self#visit_attribute ())

    method! visit_attribute _ ({ key; payload; _ } : Attribute.attribute) =
      enclose lbracket (at ^^ text key ^-^ text payload) rbracket

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
        ^^ nest tabsize @@ hardline
        ^^ self#visit_old_rule_branches pr_branches
        ^/^ self#visit_attributes () pr_attributes

    method private visit_rule_args =
      surround_separate_map tabsize 0 empty lparen
        (comma ^^ break 1)
        rparen self#visit_loctext

    method private visit_old_rule_branches branches =
      separate_map hardline
        (self#with_located (fun branch ->
             self#visit_parameterized_branch () branch))
        branches

    method! visit_early_production =
      fun _ (producers, prec_annotation, _) ->
        bar
        ^-^ flow_map (break 1)
              (self#with_located (self#visit_early_producer ()))
              producers
        ^/^ self#visit_prec_annotation () prec_annotation

    method! visit_parameterized_branch =
      fun _ { pb_productions; pb_action; pb_prec_annotation; pb_attributes } ->
        nest tabsize @@ group
        @@ separate_map hardline
             (self#with_located (self#visit_early_production ()))
             pb_productions
        ^/^ separate (break 1)
              [
                self#with_located (self#visit_action ()) pb_action;
                self#visit_prec_annotation () pb_prec_annotation;
                self#visit_attributes () pb_attributes;
              ]

    method private visit_ocaml (code : string) : document =
      code |> Ocamlformat_client.format |> R.get_or ~default:code |> String.trim
      |> arbitrary_string

    method! visit_action _ expr =
      surround tabsize 1 lbrace
        (match expr with
        | IL.ETextual located -> self#with_located self#visit_ocaml located
        | _ -> text "")
        rbrace

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
        ^^ nest tabsize
             (hardline ^^ twice space
             ^^ self#visit_expression () pr_branches
             ^/^ self#visit_attributes () pr_attributes)

    method! visit_SemPatTilde _ _ = tilde
    method! visit_SemPatWildcard _ = underscore

    method! visit_SemPatTuple _ =
      separate_map (text ", ") (self#visit_pattern ()) >> parens

    method! visit_SemPatVar _ = self#visit_loctext

    method! visit_XAPointFree _ a =
      surround tabsize 0 langle (optional self#visit_loctext a) rangle

    method! visit_XATraditional = self#visit_action

    method! visit_EChoice _ branches =
      separate_map
        (break 1 ^^ barspace)
        (self#with_located @@ self#visit_branch ())
        branches

    method! visit_ECons _ pattern symbol_expression seq_expression =
      flow (break 1)
        [
          (match pattern with
          | SemPatWildcard -> empty
          | _ -> self#visit_pattern () pattern ^-^ equals);
          self#with_located (self#visit_symbol_expression ()) symbol_expression
          ^^ semi;
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
      ^^ (match list with
        | [] -> empty
        | _ :: _ ->
            surround tabsize 0 lparen
              (separate_map (text ", ") (self#visit_expression ()) list)
              rparen)
      ^/^ self#visit_attributes () attributes
  end

(* This should really go in comment_location, but I couldn't figure out how to generalize it over the endo visitor :/ *)
let main ~config ~ast ~doc =
  let buf = Buffer.create 80 in
  let bag_of_comments = Lexer.get_comments () |> init_bag in
  let attach_vtor =
    object
      inherit [_] Syntax.ast_endo
      method! visit_located = visit_attach ~bag_of_comments ~doc
    end
  in
  attach_comments ast (attach_vtor#visit_main ()) ~bag_of_comments ~doc
  |> (new formatter config)#visit_main ()
  |> PPrint.ToBuffer.pretty 0.8 80 buf;
  Buffer.contents buf
