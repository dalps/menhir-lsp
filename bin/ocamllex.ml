open OcamllexSyntax
open Utils
open Syntax

type state = {
  grammar : lexer_definition;
  symbols : string located list;
  regexps :
    [ `Declared of named_regexp located
    | `Anonymous of regular_expression_syntax located ]
    list;
}

let regexp_bindings =
  let v =
    object
      inherit [_] syntax_reduce as super
      method zero = []
      method plus = ( @ )

      method! visit_As =
        fun _ re name -> name :: super#visit_regular_expression_syntax () re.v
    end
  in
  v#visit_regular_expression_syntax ()

let process_symbols : lexer_definition -> string located list =
  let v =
    object
      inherit [_] syntax_reduce as super
      method zero = []
      method plus = ( @ )
      method! visit_Ref = fun _ name -> [ name ]

      method! visit_entry =
        fun _env entry ->
          (entry.name :: entry.args) @ super#visit_entry _env entry

      method! visit_named_regexp =
        fun _ named_regexp ->
          named_regexp.name :: super#visit_named_regexp () named_regexp
    end
  in
  v#visit_lexer_definition ()

let symbol_at_position (state : state) (pos : Position.t) :
    (Range.t * string located) option =
  L.find_map
    (fun (s : string located) ->
      let rng = Range.of_lexical_positions s.p in
      O.if_ (fun _ -> Position.compare_inclusion pos rng = `Inside) (rng, s))
    state.symbols

let load_state_from_contents (_filename : string) (contents : string) :
    (state, Diagnostic.t list) result =
  let open R in
  let mk_diag msg range =
    Diagnostic.create ~message:(`String msg) ~range () ~source:server_name
  in
  let+ grammar =
    OcamllexSyntax.Main.parse_string contents
    |> map_err (fun (msg, rng) ->
        mk_diag msg (Range.of_lexical_positions rng) :: [])
  in
  let symbols = process_symbols grammar in
  let regexps =
    L.(
      (let+ nr = grammar.named_regexps in
       `Declared nr)
      @ let* { v = entry; _ } = grammar.entrypoints in
        let+ re, _ = entry.clauses in
        `Anonymous re)
  in
  { grammar; symbols; regexps }

let document_symbols ({ grammar; _ } : state) : DocumentSymbol.t list =
  L.(
    let+ { v = entry; _ } = grammar.entrypoints in
    let range = Range.of_lexical_positions entry.name.p in
    DocumentSymbol.create ~kind:Function ~name:entry.name.v ~range
      ~selectionRange:range
      ~children:
        (entry.clauses
        |> flat_map_i (fun _i (regexp, _) ->
            match regexp_bindings regexp.v with
            | [] -> []
            | binders ->
                let+ binder = binders in
                let range = Range.of_lexical_positions binder.p in
                DocumentSymbol.create
                  ~kind:Variable (* ~detail:(spr "case %d" i) *)
                  ~name:binder.v ~range ~selectionRange:range ()))
      ())
  @ L.map
      (fun ({ v = { name; _ }; _ } : named_regexp located) ->
        let range = Range.of_lexical_positions name.p in
        DocumentSymbol.create ~kind:Property ~name:name.v ~range
          ~selectionRange:range ())
      grammar.named_regexps

let diagnostics _ = []

let references (state : state) ~uri ~(pos : Position.t) : Location.t list =
  (let open O in
   let+ _sym_range, sym = symbol_at_position state pos in
   L.filter_map
     (fun { v; p } ->
       if_
         (fun _ -> v = sym.v)
         (Location.create ~uri ~range:(Range.of_lexical_positions p)))
     state.symbols)
  |> O.to_list |> L.flatten

let definition ({ grammar; _ } as state : state) ~uri ~(pos : Position.t) :
    Locations.t =
  let open O in
  ((* Get the symbol under the cursor, if any. *)
   let* _sym_range, sym = symbol_at_position state pos in
   (* Search for the symbol in the named regexps or in the lexer entries. *)
   let+ def =
     L.find_map
       (fun { v = entry; _ } ->
         if_ (fun _ -> String.equal entry.name.v sym.v) entry.name)
       grammar.entrypoints
     <+> L.find_map
           (fun ({ v = { name; _ }; _ } : named_regexp located) ->
             if_ (fun _ -> String.equal name.v sym.v) name)
           grammar.named_regexps
   in
   Location.create ~range:(Range.of_lexical_positions def.p) ~uri)
  |> O.to_list
  |> fun locs -> `Location locs

let manual_ref ?(label = "Manual") =
  spr "[%s](https://ocaml.org/manual/5.4/lexyacc.html#%s)" label

let regex_operator_completions : CompletionItem.t list =
  compile_completions ~kind:Operator
    [
      ( "#",
        None,
        None,
        [
          md_fenced "regexp1 # regexp2";
          "(difference of character sets) Regular expressions `regexp1` and \
           `regexp2` must be character sets defined with `[` ... `]` (or a \
           single character expression or underscore `_`. Match the difference \
           of the two specified character sets.";
          manual_ref "ss:ocamllex-regexp";
        ] );
      ( "*",
        None,
        None,
        [
          md_fenced "regexp *";
          "(repetition) Match the concatenation of zero or more strings that \
           match `regexp`.";
          manual_ref "ss:ocamllex-regexp";
        ] );
      ( "+",
        None,
        None,
        [
          md_fenced "regexp +";
          "(strict repetition) Match the concatenation of one or more strings \
           that match `regexp`.";
          manual_ref "ss:ocamllex-regexp";
        ] );
      ( "?",
        None,
        None,
        [
          md_fenced "regexp ?";
          "(option) Match the empty string, or a string matching `regexp`.";
          manual_ref "ss:ocamllex-regexp";
        ] );
      ( "|",
        None,
        None,
        [
          md_fenced "regexp1 | regexp2";
          "(alternative) Match any string that matches `regexp1` or `regexp2`. \
           If both `regexp1` and `regexp2` are character sets, this \
           constructions produces another character set, obtained by taking \
           the union of `regexp1` and `regexp2`.";
          manual_ref "ss:ocamllex-regexp";
        ] );
    ]

let keyword_completions : CompletionItem.t list =
  compile_completions ~kind:Keyword
    [
      ( "let",
        None,
        Some "let ${1:ident} = ${0:regexp}",
        [
          md_fenced "let ident = regexp";
          "Name a regular expression.";
          manual_ref "ss:ocamllex-named-regexp";
        ] );
      ( "rule",
        None,
        Some {|rule ${1:entrypoint} = parse
  | ${2:regexp} { ${0:action} }
|},
        [
          md_fenced
            {|rule entrypoint1 [arg1… argn] =
    parse regexp { action }
        | …
        | regexp { action }
and entrypoint2 [arg1… argn] =
    parse …
and …|};
          "Define an entry point of the lexer.";
          manual_ref ~label:"Syntax of lexer definitions" "s:ocamllex-syntax";
        ] );
      ( "and",
        None,
        None,
        [ manual_ref ~label:"Syntax of lexer definitions" "s:ocamllex-syntax" ]
      );
      ( "parse",
        None,
        None,
        [ manual_ref ~label:"Entry points" "ss:ocamllex-entry-points" ] );
      ( "shortest",
        None,
        None,
        [ manual_ref ~label:"Entry points" "ss:ocamllex-entry-points" ] );
      ( "refill",
        None,
        Some "refill { $0 }",
        [
          md_fenced {|refill {refill_handler}|};
          "Define a refill handler.";
          manual_ref "ss:refill-handlers";
        ] );
      ( "eof",
        None,
        None,
        [
          md_fenced "eof";
          "Match the end of the lexer input.";
          manual_ref "ss:ocamllex-regexp";
        ] );
      ( "as",
        None,
        None,
        [
          md_fenced "regexp as ident";
          "Bind the substring matched by `regexp` to identifier `ident`.";
          manual_ref "ss:ocamllex-variables";
        ] );
    ]

let completions
    ({ grammar = { header; trailer; refill_handler; _ } as grammar; _ } : state)
    ~(notify_back : Linol_lwt.Jsonrpc2.notify_back) ~(word : word option)
    ~(pos : Position.t) ~(uri : uri) : CompletionItem.t list =
  let open O in
  let pos_inside = Position.is_inside pos in
  let merlin_compls () =
    (let* word = word in
     get_merlin_compls ~notify_back ~uri ~pos word)
    |> get_or_nil
  in
  let region_completions (oregion : string located option) () =
    let* range = oregion >|= (Located.position >> Range.of_lexical_positions) in
    if_ (fun _ -> pos_inside range) (merlin_compls ())
  in
  (* Inside actions we shall suggest `lexbuf`, the variables bound with `as` in the current clause, the lexer entrypoints, and OCaml symbols *)
  let open L in
  let action_completions () =
    let*? { v = rule; _ } = grammar.entrypoints in
    let*? regexp, action = rule.clauses in
    let range = Range.of_lexical_positions action.p in
    O.if_
      (fun _ -> pos_inside range)
      ((let+ arg = rule.args in
        CompletionItem.create ~kind:Value ~label:arg.v ())
      @ (let+ { v = entry; _ } = grammar.entrypoints in
         CompletionItem.create ~kind:Function ~label:entry.name.v ())
      @ (let+ binder = regexp_bindings regexp.v in
         CompletionItem.create ~kind:Value ~label:binder.v ())
      @ compile_completions ~kind:Value
          [
            ( "lexbuf",
              None,
              None,
              [
                md_fenced "Lexing.lexbuf";
                "The current lexer buffer.";
                "Can be used in conjunction with the operations on lexer \
                 buffers provided by the `Lexing` standard library module.";
                manual_ref "ss:ocamllex-actions";
              ] );
          ]
      @ merlin_compls ())
  in
  let lexer_completions =
    regex_operator_completions @ keyword_completions
    @
    let+ ({ v = { name; _ }; _ } : named_regexp located) =
      grammar.named_regexps
    in
    (* let _range = Range.of_lexical_positions p in *)
    CompletionItem.create ~kind:Property ~label:name.v ()
  in
  region_completions header ()
  <|> region_completions refill_handler
  <|> region_completions trailer <|> action_completions
  |> get_or ~default:lexer_completions

let print_symbols ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
    (state : state) =
  notify_back#send_log_msg ~type_:Info
    (L.mapi
       (fun i s ->
         spr "%3d ) %20s at %20s" i s.v Range.(show @@ of_lexical_positions s.p))
       state.symbols
    |> CCString.concat "\n"
    |> spr "\nLexer symbols:\n%s\n")
  |> ignore

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
            (TextEdit.create ~newText:newName
               ~range:(Range.of_lexical_positions s.p)))
        state.symbols)
    |> O.to_list |> L.flatten
  in
  WorkspaceEdit.create ~changes:[ (uri, edits) ] ()

let selection_range ({ grammar; _ } : state) ~(positions : Position.t list)
    ~(notify_back : notify_back) : SelectionRange.t list =
  let open L in
  let@* i, pos = positions in
  let parent_ref = ref @@ None in
  (* This visitor descends the lexer's syntax tree nodes which contain pos, connecting them in a ladder of [SelectionRange]s. *)
  let v =
    object
      inherit [_] syntax_iter

      method! visit_located =
        fun visit_a _env located ->
          let range = Range.of_lexical_positions located.p in

          if Position.is_inside pos range then
            parent_ref :=
              O.some @@ SelectionRange.create ?parent:!parent_ref ~range ();
          visit_a _env located.v
    end
  in
  v#visit_lexer_definition () grammar;
  O.(
    let+ res = !parent_ref in
    log_info ~notify_back "Range for pos #%d %s: %s" i (Position.show pos)
      (Range.show res.SelectionRange.range);
    res)
  |> O.to_list

(** We implement a code action that extracts a named regexp from a valid
    selection. Inspired by ocaml-lsp's 'Extract local' action. *)
let code_actions ({ regexps; grammar; _ } : state) ~(notify_back : notify_back)
    ~(doc : Text_document.t) ~range:(rng : Range.t) : CodeActionResult.t =
  let module TD = Text_document in
  let uri = TD.documentUri doc in
  let open L in
  let@*? _i, re = regexps in
  let regexp =
    match re with
    | `Declared { v = { regexp; _ }; _ } | `Anonymous regexp -> regexp
  in
  let node : Range.t option ref = ref None in
  (* This visitor searches the smallest regexp node that contains [range] and stores it into [node]. *)
  let v =
    object
      inherit [_] syntax_iter

      (* Bail out on character sets, so the substitution always produces valid regular expressions. *)
      method! visit_character_class_syntax = fun _env _cls -> node := None

      method! visit_located =
        fun visit_a _env located ->
          let range = Range.of_lexical_positions located.p in

          if Range.contains range rng then (
            node := Some range;
            visit_a _env located.v)
    end
  in
  v#visit_located v#visit_regular_expression_syntax () regexp;
  let open O in
  let* extract_range = !node in
  let* local_text = substring doc extract_range in
  log_info ~notify_back "Code action available for selection: %s %s" local_text
    (Range.show extract_range);

  (* Where does the extracted regexp's new declaration go? *)
  let edit_pos : Position.t =
    let get_range : 'a. 'a located -> Range.t =
     fun a -> a |> Located.position |> Range.of_lexical_positions
    in
    match re with
    (* If we're inside a rule, under the last declaration if there exists one,
      otherwise under the grammar header or ultimately the first line. *)
    | `Anonymous _ ->
        last_opt grammar.named_regexps
        >|= (get_range >> Range.end_)
        <|> (fun () -> grammar.header >|= (get_range >> Range.end_))
        |> get_or ~default:(Position.create ~character:0 ~line:0)
    (* If we're inside a declaration [d], right before [d]. *)
    | `Declared d -> get_range d |> Range.start
  in

  let new_name = "regexp_name" in
  let newText = spr "\nlet %s = %s\n" new_name local_text in
  let insert_range = Range.create ~start:edit_pos ~end_:edit_pos in
  log_info ~notify_back "newText: %s" newText;
  let edits =
    [
      TextEdit.create ~newText ~range:insert_range;
      TextEdit.create ~newText:new_name ~range:extract_range;
    ]
  in
  let extract_action =
    CodeAction.create ~title:"Extract to named regexp"
      ~kind:CodeActionKind.RefactorExtract
      ~edit:(WorkspaceEdit.create ~changes:[ (uri, edits) ] ())
      ~command:
        (Command.create ~title:"Give it a good name"
           ~command:"editor.action.rename" ())
      ()
  in
  Some [ `CodeAction extract_action ]

let format (state : state) ~notify_back ~(doc : Text_document.t)
    ~options:(_ : FormattingOptions.t) : TextEdit.t list =
  let dcst = Format.AST2DCST.main state.grammar in
  O.(
    let+ cst = Parser.Settle.lexer_definition dcst in
    let buf = Buffer.create 80 in
    let pprint_doc = Format.CST2Document.main cst in
    PPrint.ToBuffer.pretty 0.8 80 buf pprint_doc;
    let newText = Buffer.contents buf in
    [ TextEdit.create ~newText ~range:Range.(whole_document doc) ])
  |> fun o ->
  match o with
  | None ->
      log_info ~notify_back
        "Lexer formatting failed. Please report this error to menhir-lsp's \
         developer.";
      []
  | Some e -> e
