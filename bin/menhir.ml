open Utils
open M.Located
open MenhirSyntax
open Syntax
module Range = Utils.Range

type zone = OCaml | Declaration | Rule | Action

module Ivl_map = Interval_map.Make (struct
  include Position

  let compare p1 p2 = compare p1 p2 |> Ordering.to_int
end)

module Ivl = Ivl_map.Interval

type token = {
  ocamltype : ocamltype option;
  terminal : terminal;
  alias : string option;
  _attributes : M.FrontTypes.attributes;
}

type tokens = token located list

type state = {
  grammar : partial_grammar;
  tokens : token located list;
  symbols : string located list;
  intervals : zone Ivl_map.t;
}

let get_cmly_file = fetch_build_dir ~ext:".cmly"
let get_conflicts_file = fetch_build_dir ~ext:".conflicts"

let debug_ast (state : state) : string =
  let open Menhirformat_lib.Utils in
  let open PPrint in
  let with_label :
      'env 'a. string -> ('env -> 'a -> document) -> 'env -> 'a -> document =
   fun label v env x -> angles (string label) ^^ v env x
  in
  let v =
    object
      inherit [_] ast_reduce as super
      method zero = empty
      method plus = ( ^^ )

      method private with_label :
          'env 'a. string -> ('env -> 'a -> document) -> 'env -> 'a -> document
          =
        fun label v env x -> angles (string label) ^^ v env x

      method! visit_parameterized_rule v =
        with_label "rule" (super#visit_parameterized_rule v)

      method! visit_parameterized_branch =
        with_label "production_group" super#visit_parameterized_branch

      method! visit_early_production =
        with_label "production" super#visit_early_production

      method! visit_early_producer =
        with_label "producer" super#visit_early_producer

      method! visit_parameter = with_label "parameter" super#visit_parameter

      method! visit_symbol_expression =
        with_label "symbol_expression" super#visit_symbol_expression

      method! visit_choice_expression =
        with_label "choice_expression" super#visit_choice_expression

      method! visit_branch = with_label "branch" super#visit_branch

      method! visit_raw_seq_expression =
        with_label "seq_expression" super#visit_raw_seq_expression

      method! visit_terminal _ = string
      method! visit_nonterminal _ = string
      method! visit_symbol _ = string
      method! visit_identifier _ = string

      method! visit_located =
        fun visit_v v loc ->
          (hardline ^^ arbitrary_string
          @@ Range.(of_lexical_positions loc.p |> show))
          ^^ nest 4 (super#visit_located visit_v v loc)
    end
  in
  let buf = Buffer.create 80 in
  v#visit_partial_grammar () state.grammar |> PPrint.ToBuffer.pretty 0.8 80 buf;
  Buffer.contents buf

let yojson_of_ast (grammar : partial_grammar) : Json.t =
  let open Json in
  let string s = `String s in
  let with_label :
      'env 'a. string -> ('env -> 'a -> Json.t) -> 'env -> 'a -> Json.t =
   fun label v env x -> `Assoc [ ("type", `String label); ("value", v env x) ]
  in
  let v =
    object
      inherit [_] ast_reduce as super
      method zero : t = `List []

      method plus (o1 : t) (o2 : t) =
        match (o1, o2) with
        | `List o1, `List o2 -> `List (o1 @ o2)
        | `List o1, o2 -> `List (o1 @ [ o2 ])
        | o1, `List o2 -> `List (o1 :: o2)
        | _, _ -> `List [ o1; o2 ]

      method! visit_located =
        fun visit_v env loc ->
          let range = Range.of_lexical_positions loc.p |> Range.yojson_of_t in
          let start, end_ =
            CCPair.map_same (fun (pos : Lexing.position) -> pos.pos_cnum) loc.p
          in
          let value = visit_v env loc.v in
          let _comments =
            loc.comment
            |> O.map
               @@ List.map (fun { text; _ } ->
                   `Assoc [ ("text", `String text) ])
            |> O.get_or_nil
          in
          `Assoc
            [
              ("range", range);
              ("rawRange", `List [ `Int start; `Int end_ ]);
              ("value", value);
            ]

      method! visit_terminal _ = string
      method! visit_nonterminal _ = string
      method! visit_symbol _ = string
      method! visit_identifier _ = string

      method! visit_parameterized_rule v =
        with_label "rule" (super#visit_parameterized_rule v)

      method! visit_parameterized_branch =
        with_label "production_group" super#visit_parameterized_branch

      method! visit_early_production =
        with_label "production" super#visit_early_production

      method! visit_early_producer =
        with_label "producer" super#visit_early_producer

      method! visit_parameter = with_label "parameter" super#visit_parameter

      method! visit_symbol_expression =
        with_label "symbol_expression" super#visit_symbol_expression

      method! visit_choice_expression =
        with_label "choice_expression" super#visit_choice_expression

      method! visit_branch = with_label "branch" super#visit_branch

      method! visit_raw_seq_expression =
        with_label "seq_expression" super#visit_raw_seq_expression

      method! visit_raw_action = with_label "action" super#visit_raw_action
      method! visit_action = with_label "action" super#visit_action

      method! visit_declaration =
        with_label "declaration" super#visit_declaration

      method! visit_alias = with_label "alias" super#visit_alias
      method! visit_ocamltype = with_label "ocamltype" super#visit_ocamltype
      method! visit_attribute = with_label "attribute" super#visit_attribute
    end
  in
  v#visit_partial_grammar () grammar

let rec string_of_params : parameter -> string = function
  | ParamVar p -> p.v
  | ParamApp (p, ps) ->
      spr "%s(%s)" p.v
        L.(ps >|= Located.iter string_of_params |> String.concat ", ")
  | ParamAnonymous _ -> ""

let process_symbols : partial_grammar -> symbol located list =
  let aliases : (string, string) Hashtbl.t = Hashtbl.create 99 in
  let v =
    object
      inherit [_] ast_reduce as super
      method zero : symbol located list = []
      method plus = ( @ )

      method! visit_DToken =
        fun _ _option ts ->
          ts
          |> L.map (fun { v = terminal, alias, _attributes; _ } ->
              O.iter (fun a -> Hashtbl.add aliases a.v terminal.v) alias;
              terminal)

      method! visit_DTokenProperties =
        fun _ ts _associativity _precedence_level -> ts

      method! visit_DStart = fun _ _ nts -> nts

      method! visit_ParamVar =
        fun _ sym ->
          (* Resolve token aliases *)
          let t_name = Hashtbl.find_opt aliases sym.v in
          [ O.map_or ~default:sym (fun s -> { sym with v = s }) t_name ]

      method! visit_ParamApp =
        fun _ sym parameters -> sym :: super#visit_parameters () parameters

      (* [ide] could be _1, _2, etc. when the producer is unbound, in this case it shares the same range of [parameter], which throws off the [definition] query.

      We will consider [ide] when we'll support [definition] requests inside semantic actions.
      *)
      (* method! visit_producer =
        fun _ (ide, parameter, _) -> ide :: super#visit_parameter () parameter *)

      method! visit_prec_annotation =
        fun _ prec_annotation ->
          O.(prec_annotation >|= M.Located.value |> to_list)

      method! visit_parameterized_rule =
        fun v env parameterized_rule ->
          (parameterized_rule.pr_nt :: parameterized_rule.pr_parameters)
          @ super#visit_parameterized_rule v env parameterized_rule

      (* new syntax visitors *)

      method! visit_ESymbol =
        fun _ located list _attributes ->
          located :: L.flat_map (super#visit_expression ()) list
    end
  in
  v#visit_partial_grammar ()

let load_state_from_partial_grammar (grammar : partial_grammar) =
  let symbols = process_symbols grammar in
  let add : 'a. zone -> 'a located -> zone Ivl_map.t -> zone Ivl_map.t =
   fun (zone : zone) (located : _ located) ->
    let rng = Range.of_lexical_positions located.p in
    let ivl = Ivl.create (Included rng.start) (Included rng.end_) in
    Ivl_map.add ivl zone
  in
  let map_ref = ref Ivl_map.empty in
  let add zone loc = map_ref := add zone loc !map_ref in
  (* Don't refactor the explicit parameter, it keeps these polymorphic! *)
  let ocaml_zone loc = add OCaml loc in
  let decls_zone loc = add Declaration loc in
  let rules_zone loc = add Rule loc in
  let action_zone loc = add Action loc in
  let v =
    object
      inherit [_] ast_iter as super
      (* inherit [_] ast_reduce as super
        method zero = Ivl_map.empty

        method plus t1 t2 =
          Ivl_map.fold
            (fun ivl vs acc ->
              vs |> List.fold_left (fun acc' v -> Ivl_map.add ivl v acc') acc)
            t2 t1 *)

      method! visit_DCode _ = ocaml_zone
      method! visit_Declared _ = ocaml_zone
      method! visit_DParameter _ = ocaml_zone

      method! visit_action _ =
        function ETextual loc -> action_zone loc | _ -> ()

      method! visit_partial_grammar =
        fun _ ({ pg_declarations; pg_rules; pg_postlude; _ } as grammar) ->
          pg_declarations |> List.iter decls_zone;
          pg_rules
          |> List.iter (function
            | Old loc -> rules_zone loc
            | New loc -> rules_zone loc);
          pg_postlude |> Option.iter ocaml_zone;
          super#visit_partial_grammar () grammar
    end
  in
  v#visit_main () grammar;
  let tokens : tokens =
    L.flat_map
      (function
        | { v = DToken (ocamltype, ts); _ } ->
            ts
            |> L.map @@ Located.map
               @@ fun (terminal, alias, _attributes) ->
               {
                 ocamltype;
                 terminal = terminal.v;
                 alias = O.map Located.value alias;
                 _attributes;
               }
        | _ -> [])
      grammar.pg_declarations
  in
  { grammar; tokens; symbols; intervals = !map_ref }

let load_state_from_contents (file_name : string) (file_contents : string) :
    (state, Diagnostic.t list) result =
  let open R in
  let mk_diag msg range =
    Diagnostic.create ~message:(`String msg) ~range () ~source:server_name
  in
  M.Main.load_grammar_from_contents 0 file_name file_contents
  |> map_err (fun (msg, rng) ->
      mk_diag msg (Range.of_lexical_positions rng) :: [])
  |> map load_state_from_partial_grammar

let standard_lib =
  Standard.menhir_standard_library_grammar |> R.get_exn
  |> load_state_from_partial_grammar

let default_completions ?range:(orange : Range.t option)
    ?(docs : (string, string) Hashtbl.t = Hashtbl.create 0)
    ({ tokens; grammar; _ } : state) : CompletionItem.t list =
  let open L in
  (let* t = tokens in
   let comp = CompletionItem.create ~kind:CompletionItemKind.Value in
   let typ =
     O.(t.v.ocamltype >|= function Declared { v; _ } | Inferred v -> v)
   in
   comp ~label:t.v.terminal
     ?textEdit:
       O.(
         let+ range = orange in
         `TextEdit TextEdit.{ newText = t.v.terminal; range })
     ?detail:typ ()
   :: O.(
        (let+ alias = t.v.alias in
         comp ~label:alias ?detail:typ
           ~documentation:
             (`MarkupContent
                (MarkupContent.create ~kind:Markdown
                   ~value:(spr "alias for `%s`" t.v.terminal)))
           ())
        |> to_list))
  @ let+ rule = grammar.pg_rules in
    let label = match rule with Old r -> r.v.pr_nt.v | New r -> r.v.pr_nt.v in
    let params_o =
      match
        match rule with
        | Old r -> r.v.pr_parameters
        | New r -> r.v.pr_parameters
      with
      | [] -> None
      | ps -> Some ps
    in
    let comp =
      CompletionItem.create ~kind:CompletionItemKind.Function ~label
        ?documentation:
          O.(
            CCHashtbl.get docs label >|= fun doc ->
            `MarkupContent (MarkupContent.create ~kind:Markdown ~value:doc))
        ?insertTextFormat:
          O.(
            let+ _ = params_o in
            InsertTextFormat.Snippet)
        ?insertText:
          O.(
            let+ _ = params_o in
            label ^ "($0)")
        ?labelDetails:
          O.(
            let+ params = params_o in
            CompletionItemLabelDetails.create
              ~detail:
                (L.to_string ~start:"(" ~stop:")" (fun { v; _ } -> v) params)
              ())
    in
    comp ()

let standard_lib_completions =
  default_completions standard_lib ~docs:Standard.menhir_standard_library_doc

let document_symbols ({ grammar = { pg_rules; _ }; tokens; _ } : state) :
    DocumentSymbol.t list =
  (* Here we extract a listing of the defined tokens and grammar rules. *)
  L.(
    (let+ t = tokens in
     let range = Range.of_lexical_positions t.p in
     DocumentSymbol.create ~kind:SymbolKind.Constant ~name:t.v.terminal ~range
       ~selectionRange:range
       ~detail:(O.get_or ~default:"" t.v.alias)
       ())
    @ let+ rule = pg_rules in
      let p = match rule with Old r -> r.p | New r -> r.p in
      let range = Range.of_lexical_positions p in
      let selectionRange = Range.of_lexical_positions p in
      DocumentSymbol.create ~kind:SymbolKind.Function
        ~name:(match rule with Old r -> r.v.pr_nt.v | New r -> r.v.pr_nt.v)
        ~range ~selectionRange
        ~children:
          (let v =
             object
               inherit [_] Syntax.ast_reduce
               method zero = []
               method plus = ( @ )

               method! visit_producer =
                 fun _ (binder, par, _) ->
                   let range = Range.of_lexical_positions binder.p in
                   match
                     let open CCParse in
                     parse_string ((char '_' <|> char '$') *> U.int) binder.v
                   with
                   (* don't show positional binders *)
                   | Ok _ -> []
                   | Error _ ->
                       [
                         DocumentSymbol.create ~kind:SymbolKind.Variable
                           ~name:binder.v ~range ~selectionRange:range
                           ~detail:(string_of_params par.v) ();
                       ]

               method! visit_SemPatVar =
                 fun _ binder ->
                   [
                     DocumentSymbol.create ~kind:SymbolKind.Variable
                       ~name:binder.v ~range ~selectionRange:range ();
                   ]
             end
           in
           v#visit_rule () rule)
        ())

let symbol_at_position (state : state) (pos : Position.t) :
    (Range.t * string located) option =
  let open L in
  let*? (s : string located) = state.symbols in
  let rng = Range.of_lexical_positions s.p in
  let res = Position.is_inside pos rng in
  if res then Some (rng, s) else None

(** Produce hover information at a particular position. For:
    - token aliases, we display their full name;
    - standard library rules, their documentation; *)
let hover (state : state) ~(pos : Position.t) : Hover.t option =
  let open O in
  let* rng, sym = symbol_at_position state pos in
  let+ contents, range =
    (let+ stdlib_doc =
       Hashtbl.find_opt Standard.menhir_standard_library_doc sym.v
     in
     (stdlib_doc, rng))
    <+> L.find_map
          (fun ({ v = t; _ } : token located) ->
            if_
              (fun _ -> t.alias = Some sym.v || t.terminal = sym.v)
              ( O.map_or ~default:""
                  (function
                    | M.BaseTypes.Declared { v; _ } | M.BaseTypes.Inferred v ->
                        spr "<%s> " v)
                  t.ocamltype
                ^ t.terminal
                |> md_fenced,
                rng ))
          state.tokens
  in
  Hover.create
    ~contents:
      (`MarkupContent
         (MarkupContent.create ~kind:MarkupKind.Markdown ~value:contents))
    ~range ()

let diagnostics ~(notify_back : notify_back) ~(uri : uri) (_s : state) :
    Diagnostic.t list =
  let log = log_info ~notify_back in
  let open R in
  get_or_nil
  @@
  let* conflicts_file = get_conflicts_file uri in
  log "conflicts_file: %s" conflicts_file;
  let module P = CCParse in
  let module S = CCString in
  let mk_diag (toks : token located list) lines =
    let message = S.concat "\n" @@ lines in
    Diagnostic.create ~range:Range.first_line ~source:conflicts_file
      ~relatedInformation:
        L.(
          let+ tk = toks in
          (* log
             "token name: %s %s" tk.v.terminal
                 Range.(of_lexical_positions tk.p |> show); *)
          DiagnosticRelatedInformation.create
            ~location:
              (Location.create ~uri ~range:(Range.of_lexical_positions tk.p))
            ~message:(spr "%s is involved." tk.v.terminal))
      ~message:
        (* Contrary to the OCaml type, the protocol doesn't support Markdown in the diagnostic message. (https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic), and in fact the vscode extension crashes. Quite the pickle, because the bits of the conflict message showing derivations trees require a monospace font for best viewing.

            (`MarkupContent (MarkupContent.create ~kind:Markdown ~value:message))

          *)
        (`String message) ()
  in
  let tokens_prefix = "** Tokens involved:" in
  In_channel.with_open_text conflicts_file (fun inp ->
      In_channel.input_all inp |> S.trim |> S.split ~by:"\n"
      (* |> S.replace ~sub:"\n\n" ~by:"\n```\n" *)
      (* markdown not supported *)
      |> fun lines ->
      List.fold_right
        (fun line acc ->
          let chop s =
            s |> S.chop_prefix ~pre:"** " |> O.get_or ~default:line
          in
          match acc with
          | toks, current_diag, diags
            when String.starts_with ~prefix:"** Conflict" line ->
              ([], [], mk_diag toks (chop line :: current_diag) :: diags)
          | _, current_diag, diags
            when String.starts_with ~prefix:tokens_prefix line ->
              ( S.chop_prefix ~pre:tokens_prefix line
                |> Option.get |> S.trim |> S.split ~by:" "
                |> L.map (fun tk ->
                    L.find (fun { v; _ } -> S.equal v.terminal tk) _s.tokens),
                chop line :: current_diag,
                diags )
          | toks, current_diag, diags -> (toks, chop line :: current_diag, diags))
        lines ([], [], [])
      |> fun (_, _, diags) -> Ok diags)

let references (state : state) ~uri ~(pos : Position.t) : Location.t list =
  (let open O in
   let* _sym_range, sym = symbol_at_position state pos in
   (* Is it a token alias? If so, use token's full name. *)
   let sym_name =
     get_or ~default:sym.v
       (L.find_map
          (fun t ->
            let* alias = t.v.alias in
            if_ (fun _ -> alias = sym.v) alias)
          state.tokens)
   in
   epr "Looking for references of %s\n" sym_name;
   Some
     (L.filter_map
        (fun { v; p; _ } ->
          epr "Comparing with %s at %s\n" v
            Range.(show @@ of_lexical_positions p);
          if_
            (fun _ -> v = sym_name)
            (Location.create ~uri ~range:(Range.of_lexical_positions p)))
        state.symbols))
  |> O.to_list |> L.flatten

let definition (state : state) ~uri ~(pos : Position.t) : Locations.t =
  let mk_location locs = `Location locs in
  let open O in
  mk_location @@ to_list
  @@
  (* Get the symbol under the cursor, if any. *)
  let* sym_range, sym = symbol_at_position state pos in
  (* log_info ~notify_back "[Definition] symbol under cursor: %s" sym.v; *)
  (* Search for the symbol in the terminals or in the nonterminals. *)
  let+ def =
    let open L in
    (let*? (t : token located) = state.tokens in
     O.if_
       (fun _ -> String.equal t.v.terminal sym.v || t.v.alias = Some sym.v)
       (locate t.p t.v.terminal))
    <|> fun () ->
    let*? rule = state.grammar.pg_rules in
    let pr_nt, pr_params, range =
      match rule with
      | Old r -> (r.v.pr_nt, r.v.pr_parameters, r.p)
      | New r -> (r.v.pr_nt, r.v.pr_parameters, r.p)
    in
    let range = Range.of_lexical_positions range in
    O.if_ (fun _ -> String.equal pr_nt.v sym.v) pr_nt
    <|>
    (* It could be a formal parameter of the rule! *)
    fun () ->
    if Range.contains range sym_range then
      L.find_opt (fun param -> String.equal param.v sym.v) pr_params
    else None
  in
  (* log_info ~notify_back "[Definition] found definition at: %s"
  @@ M.Range.show def.p; *)
  Location.create ~range:(Range.of_lexical_positions def.p) ~uri

let completions ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
    ~(word : word option) ~(pos : Position.t) ~(uri : uri) (state : state) :
    CompletionItem.t list =
  let open O in
  get_or_nil
  @@
  (* let range =
    let+ { p; _ } = word in
    p
  in *)
  let+ { p = rng; _ } = word in
  let query = Ivl.create (Included rng.start) (Included rng.end_) in
  let merlin_compls () =
    (let* word = word in
     get_merlin_compls ~notify_back ~uri ~pos word)
    |> get_or ~default:[]
  in
  (* If we are inside a semantic action we shall suggest bound variables, position keywords and OCaml symbols *)
  let _action_completions branch =
    Keywords.position_keywords ~range:rng ()
    @ (let open L in
       let* { v = producers, _, _; _ } = branch.pb_productions in
       let* { v = binder, par, _; _ } = producers in
       (* We only suggest the explicitly named producers. *)
       match binder with
       | None -> [] (* hide $1, $2... corresponding to anonymous parameters *)
       | Some { v = binder; _ } ->
           [
             CompletionItem.create ~kind:Variable
               ~detail:(string_of_params par.v) ~label:binder
               ~textEdit:(`TextEdit TextEdit.{ newText = binder; range = rng })
               ();
           ])
    @ merlin_compls ()
  in
  let _grammar_completions () =
    default_completions ~range:rng state
    @ standard_lib_completions
    @ Keywords.declarations ~range:rng ()
  in

  let string_of_ivl ({ low; high } : Ivl.t) =
    let open Ivl_map.Bound in
    let string_of_v = Position.show in
    let a =
      match low with
      | Included v -> [ "["; string_of_v v ]
      | Excluded v -> [ "("; string_of_v v ]
      | Unbounded -> [ "∞" ]
    in
    let b =
      match high with
      | Included v -> [ string_of_v v; "]" ]
      | Excluded v -> [ string_of_v v; ")" ]
      | Unbounded -> [ "∞" ]
    in
    spr "%s, %s" (String.concat "" a) (String.concat "" b)
  in
  let res = Ivl_map.query_interval_list query state.intervals in
  log_info ~notify_back "interval query for %s produced %d results"
    (string_of_ivl query)
  @@ L.length res;
  res
  |> List.iter (fun (ivl, zones) ->
      zones
      |> List.iter (fun zone ->
          let z =
            match zone with
            | OCaml -> "ocaml"
            | Declaration -> "decl"
            | Rule -> "rule"
            | Action -> "action"
          in
          log_info ~notify_back "%s --> %s" (string_of_ivl ivl) z));
  []

let prepare_rename (state : state) ~(pos : Position.t) : Range.t option =
  let open O in
  let+ sym_range, _ = symbol_at_position state pos in
  sym_range

let rename (state : state) ~uri ~(pos : Position.t) ~(newName : string) :
    WorkspaceEdit.t =
  let edits : TextEdit.t list =
    O.(
      let+ _sym_range, sym = symbol_at_position state pos in
      L.filter_map
        (fun (s : string located) ->
          if_
            (fun _ -> CCString.equal s.v sym.v)
            (* (TextEdit.create ~newText:newName
              ~range:(Range.of_lexical_positions s.p)) *)
            (TextEdit.create ~newText:newName
               ~range:(Range.of_lexical_positions s.p)))
        state.symbols)
    |> O.to_list |> L.flatten
  in
  WorkspaceEdit.create ~changes:[ (uri, edits) ] ()

let code_actions (state : state) ~uri ~(range : Range.t) : CodeActionResult.t =
  let open O in
  let* sym_range, sym = symbol_at_position state range.start in
  (* Is is a token declaration? Does it *not* have an alias? *)
  L.flat_map
    (function
      | { v = { terminal; alias = None; _ }; _ } when terminal = sym.v ->
          [
            `Command
              (Command.create
                 ~title:
                   ("Define an alias for " ^ terminal
                  ^ " and replace all its occurrences")
                 ~command:"menhir-lsp-client.promptAlias"
                 ~arguments:
                   (* send the token name and the ranges of its occurrences to the client *)
                   [
                     `String terminal;
                     Range.yojson_of_t sym_range;
                     DocumentUri.yojson_of_t uri;
                     `List
                       (L.filter_map
                          (fun sym' ->
                            let range = Range.of_lexical_positions sym'.p in
                            if_
                              (fun _ ->
                                sym.v = sym'.v
                                && Range.compare sym_range range <> Eq)
                              (Range.yojson_of_t range))
                          state.symbols);
                   ]
                 ());
          ]
      | { v = { terminal; alias = Some alias; _ }; _ } when terminal = sym.v ->
          [
            `CodeAction
              (CodeAction.create ~kind:Refactor
                 ~title:
                   ("Replace all occurrences of " ^ terminal ^ " with alias")
                 ~edit:
                   (WorkspaceEdit.create
                      ~changes:
                        [
                          ( uri,
                            L.filter_map
                              (fun sym ->
                                let range = Range.of_lexical_positions sym.p in
                                if_
                                  (fun _ ->
                                    sym.v = terminal
                                    && Range.compare sym_range range <> Eq)
                                  (TextEdit.create ~newText:alias ~range))
                              state.symbols );
                        ]
                      ())
                 ());
          ]
      | _ -> [])
    state.tokens
  |> some

let selection_range ({ grammar; _ } as _state : state)
    ~(positions : Position.t list) ~(notify_back : notify_back) :
    SelectionRange.t list =
  let json = yojson_of_ast grammar in
  log_info ~notify_back "%s" @@ Json.pretty_to_string json;
  let open L in
  let@* i, pos = positions in
  let parent_ref = ref @@ None in
  (* This visitor descends the grammar's syntax tree nodes which contain pos, connecting them in a ladder of [SelectionRange]s. *)
  let v =
    object
      inherit [_] ast_iter

      method! visit_located =
        fun visit_a _env located ->
          let range = Range.of_lexical_positions located.p in

          if Position.is_inside pos range then
            parent_ref :=
              O.some @@ SelectionRange.create ?parent:!parent_ref ~range ();
          visit_a _env located.v
    end
  in
  v#visit_partial_grammar () grammar;
  O.(
    let+ res = !parent_ref in
    log_info ~notify_back "Range for pos #%d %s: %s" i (Position.show pos)
      (Range.show res.SelectionRange.range);
    res)
  |> O.to_list

let format (state : state) ~(doc : Text_document.t)
    ~options:(_ : FormattingOptions.t) : TextEdit.t list =
  let newText = Menhirformat_lib.Menhir.main ~doc ~ast:state.grammar in
  [ TextEdit.create ~newText ~range:Range.(whole_document doc) ]
