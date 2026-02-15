open Utils
open Loc
open MenhirSyntax
open Syntax
module Range = Utils.Range

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
}

let get_cmly_file = fetch_build_dir ~ext:".cmly"
let get_conflicts_file = fetch_build_dir ~ext:".conflicts"

let rec string_of_params : parameter -> string = function
  | ParamVar p -> p.v
  | ParamApp (p, ps) ->
      spr "%s(%s)" p.v L.(ps >|= string_of_params |> String.concat ", ")
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
        fun _ parameterized_rule ->
          parameterized_rule.pr_nt
          :: super#visit_parameterized_rule () parameterized_rule
    end
  in
  v#visit_partial_grammar ()

let load_state_from_partial_grammar (grammar : partial_grammar) =
  let symbols = process_symbols grammar in
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
  { grammar; tokens; symbols }

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
  @ let+ { v = rule; _ } = grammar.pg_rules in
    let label = rule.pr_nt.v in
    let params_o =
      match rule.pr_parameters with [] -> None | _ -> Some rule.pr_parameters
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
    @ let+ { v = rule; p; _ } = pg_rules in
      let range = Range.of_lexical_positions p in
      let selectionRange = Range.of_lexical_positions p in
      DocumentSymbol.create ~kind:SymbolKind.Function ~name:rule.pr_nt.v ~range
        ~selectionRange
        ~children:
          (let* { v = branch; _ } = rule.pr_branches in
           let* { v = binder, par, _; _ } = branch.pb_producers in
           let range = Range.of_lexical_positions binder.p in
           match
             let open CCParse in
             parse_string ((char '_' <|> char '$') *> U.int) binder.v
           with
           (* don't show positional binders *)
           | Ok _ -> []
           | Error _ ->
               [
                 DocumentSymbol.create ~kind:SymbolKind.Variable ~name:binder.v
                   ~range ~selectionRange:range ~detail:(string_of_params par)
                   ();
               ])
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
  let* _sym_range, sym = symbol_at_position state pos in
  (* log_info ~notify_back "[Definition] symbol under cursor: %s" sym.v; *)
  (* Search for the symbol in the terminals or in the nonterminals. *)
  let+ def =
    let open L in
    (let*? (t : token located) = state.tokens in
     O.if_
       (fun _ -> String.equal t.v.terminal sym.v || t.v.alias = Some sym.v)
       (locate t.p t.v.terminal))
    <|> fun () ->
    let*? { v = r; _ } = state.grammar.pg_rules in
    O.if_ (fun _ -> String.equal r.pr_nt.v sym.v) r.pr_nt
  in
  (* log_info ~notify_back "[Definition] found definition at: %s"
  @@ M.Range.show def.p; *)
  Location.create ~range:(Range.of_lexical_positions def.p) ~uri

let completions ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
    ~(word : word option) ~(pos : Position.t) ~(uri : uri)
    ({ grammar; _ } as state : state) : CompletionItem.t list =
  let open O in
  let pos_inside = Position.is_inside pos in
  let merlin_compls () =
    (let* word = word in
     get_merlin_compls ~notify_back ~uri ~pos word)
    |> get_or ~default:[]
  in
  let declaration_completions () =
    L.find_map
      (fun ({ v; _ } : declaration located) ->
        match v with
        | DCode { p; _ }
        | DType (Declared { p; _ }, _)
        | DToken (Some (Declared { p; _ }), _)
        | DParameter { p; _ } ->
            let range = Range.of_lexical_positions p in
            if pos_inside range then Some (merlin_compls ()) else None
        | _ -> None)
      grammar.pg_declarations
  in
  let _postlude =
    grammar.pg_postlude
    >|= (fun { p; _ } -> Range.of_lexical_positions p)
    |> to_list
  in
  let word_range =
    let+ { p; _ } = word in
    p
  in
  (* If we are inside a semantic action we shall suggest bound variables, position keywords and OCaml symbols *)
  let action_completions () =
    L.find_map
      (fun { v = rule; _ } ->
        L.find_map
          (fun { v = branch; _ } ->
            let* action_range =
              match branch.pb_action.v.expr with
              | M.IL.ETextual { p; _ } -> Some (Range.of_lexical_positions p)
              | _ -> None
            in
            if pos_inside action_range then
              Keywords.position_keywords ?range:word_range ()
              @ (let open L in
                 let+ { v = binder, par, _; _ } = branch.pb_producers in
                 let binder =
                   O.(
                     CCString.chop_prefix ~pre:"_" binder.v
                     >|= ( ^ ) "$" |> get_or ~default:binder.v)
                 in
                 CompletionItem.create ~kind:Variable
                   ~detail:(string_of_params par) ~label:binder
                   ?textEdit:
                     O.(
                       let+ range = word_range in
                       `TextEdit TextEdit.{ newText = binder; range })
                   ())
              @ merlin_compls ()
              |> some
            else None)
          rule.pr_branches)
      grammar.pg_rules
  in
  let grammar_completions () =
    some
    @@ default_completions ?range:word_range state
    @ standard_lib_completions
    @ Keywords.declarations ?range:word_range ()
  in
  declaration_completions () <|> action_completions <|> grammar_completions
  |> get_or ~default:[]

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

let selection_range ({ grammar; _ } : state) ~(positions : Position.t list)
    ~(notify_back : notify_back) : SelectionRange.t list =
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

let format (state : state) ~notify_back ~(doc : Text_document.t)
    ~options:(_ : FormattingOptions.t) : TextEdit.t list =
  let open Menhir_formatting in
  let buf = Buffer.create 80 in
  let bag_of_comments = init_bag (Lexer.get_comments ()) in
  let attach_vtor =
    object
      inherit [_] Syntax.ast_endo
      method! visit_located = visit_located ~bag_of_comments ~notify_back ~doc
    end
  in
  attach_comments state.grammar
    (attach_vtor#visit_main ())
    ~bag_of_comments ~notify_back ~doc
  |> (new formatter ~notify_back ~doc)#visit_main ()
  |> PPrint.ToBuffer.pretty 0.8 80 buf;
  let newText = Buffer.contents buf in
  [ TextEdit.create ~newText ~range:Range.(whole_document doc) ]
