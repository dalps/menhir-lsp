open OcamllexSyntax
open Utils
open Located

type state = {
  grammar : Syntax.lexer_definition;
  symbols : string located list;
  regexps : Syntax.regular_expression located list;
}

let rec regexp_bindings = function
  | Syntax.Sequence (re1, re2) | Alternative (re1, re2) ->
      regexp_bindings re1.v @ regexp_bindings re2.v
  | Repetition re -> regexp_bindings re.v
  | Bind (re, n) -> n :: regexp_bindings re.v
  | _ -> []

let process_symbols (grammar : Syntax.lexer_definition) : string located list =
  let module S = Syntax in
  let open S in
  let open L in
  let rec visit_entry (entry : (string located list, location) entry) =
    (entry.name :: entry.args)
    @ (entry.clauses >>= fun (re, _action) -> visit_regexp re.v)
  and visit_regexp = function
    | Sequence (re1, re2) | Alternative (re1, re2) ->
        visit_regexp re1.v @ visit_regexp re2.v
    | Repetition re -> visit_regexp re.v
    | Bind (re, n) -> n :: visit_regexp re.v
    | Ref n -> [ n ]
    | Characters _ | _ -> []
  and visit_named_regexp (name, regexp) = name :: visit_regexp regexp.v in
  let f = L.flat_map in
  let s_entries = f visit_entry grammar.entrypoints in
  let s_regexps = f visit_named_regexp grammar.named_regexps in
  s_entries @ s_regexps

(* repetitive, move to a functor *)
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
      (let+ _, re = grammar.named_regexps in
       re)
      @ let* entry = grammar.entrypoints in
        let+ re, _ = entry.clauses in
        re)
  in
  { grammar; symbols; regexps }

let document_symbols ({ grammar; _ } : state) : DocumentSymbol.t list =
  L.(
    let+ entry = grammar.entrypoints in
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
      (fun (name, _) ->
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
       (fun e -> if_ (fun _ -> String.equal e.Syntax.name.v sym.v) e.name)
       grammar.entrypoints
     <+> L.find_map
           (fun (name, _) -> if_ (fun _ -> String.equal name.v sym.v) name)
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

let completions ({ grammar = { header; trailer; _ } as grammar; _ } : state)
    ~(notify_back : Linol_lwt.Jsonrpc2.notify_back) ~(word : word option)
    ~(pos : Position.t) ~(uri : uri) : CompletionItem.t list =
  let open O in
  let pos_inside = Position.is_inside pos in
  let header = Range.of_lexical_positions header in
  let trailer = Range.of_lexical_positions trailer in
  let merlin_compls () =
    (let* word = word in
     get_merlin_compls ~notify_back ~uri ~pos word)
    |> get_or ~default:[]
  in
  let header_completions () =
    if_ (fun _ -> pos_inside header || pos_inside trailer) (merlin_compls ())
  in
  let refill_handler_completions () =
    let* range = grammar.refill_handler in
    if_
      (fun _ -> pos_inside (Range.of_lexical_positions range))
      (merlin_compls ())
  in
  (* Inside actions we shall suggest `lexbuf`, the variables bound with `as` in the current clause, the lexer entrypoints, and OCaml symbols *)
  let action_completions () =
    L.find_map
      (fun (rule : _ Syntax.entry) ->
        L.find_map
          (fun (regexp, r) ->
            let range = Range.of_lexical_positions r in
            if pos_inside range then
              L.(
                let+ arg = rule.args in
                CompletionItem.create ~kind:Value ~label:arg.v ())
              @ L.(
                  let+ entry = grammar.entrypoints in
                  CompletionItem.create ~kind:Function ~label:entry.name.v ())
              @ L.(
                  let+ binder = regexp_bindings regexp.v in
                  CompletionItem.create ~kind:Value ~label:binder.v ())
              @ compile_completions ~kind:Value
                  [
                    ( "lexbuf",
                      None,
                      None,
                      [
                        md_fenced "Lexing.lexbuf";
                        "The current lexer buffer.";
                        "Can be used in conjunction with the operations on \
                         lexer buffers provided by the `Lexing` standard \
                         library module.";
                        manual_ref "ss:ocamllex-actions";
                      ] );
                  ]
              @ merlin_compls ()
              |> some
            else None)
          rule.clauses)
      grammar.entrypoints
  in
  let lexer_completions =
    regex_operator_completions @ keyword_completions
    @ L.map
        (fun (name, _) ->
          (* let _range = Range.of_lexical_positions p in *)
          CompletionItem.create ~kind:Property ~label:name.v ())
        grammar.named_regexps
  in
  header_completions () <|> refill_handler_completions <|> action_completions
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

(* extract_to_named_regex_code_action *)

let selection_range ({ regexps; _ } : state) ~uri:_ ~(pos : Position.t)
    ~(notify_back : notify_back) : SelectionRange.t list =
  let open L in
  let* re = regexps in
  let if_ ?(parent : SelectionRange.t option)
      (range : Lexing.position * Lexing.position) =
    (* let parent = O.map Range.of_lexical_positions parent in *)
    let range = Range.of_lexical_positions range in
    if Position.is_inside pos range then
      [ SelectionRange.create ?parent ~range () ]
    else []
  in
  (* traverse the regexp and collect all the containing nodes *)

  let rec visit_regexp ?(parent : SelectionRange.t option)
      ({ p; v } : Syntax.regular_expression located) : SelectionRange.t list =
    let range = Range.of_lexical_positions p in
    let parent = SelectionRange.create ?parent ~range () in
    match v with
    | Syntax.Epsilon -> if_ ~parent p
    | Syntax.Characters { p; v } -> visit_char_class ~parent { p; v = fst v }
    | Syntax.Eof -> if_ ~parent p
    | Syntax.Sequence (e1, e2) | Syntax.Alternative (e1, e2) ->
        visit_regexp ~parent e1 @ visit_regexp ~parent e2
    | Syntax.Repetition e -> visit_regexp ~parent e
    | Syntax.Ref { p; _ } -> if_ ~parent p
    | Syntax.Bind (e, _) -> visit_regexp ~parent e @ if_ ~parent p
  and visit_char_class ?(parent : SelectionRange.t option)
      ({ p; v } : Syntax.character_class located) : SelectionRange.t list =
    let range = Range.of_lexical_positions p in
    let parent = SelectionRange.create ?parent ~range () in
    match v with
    | Syntax.Wildcard -> if_ ~parent p
    | Syntax.Character { p; _ } -> if_ ~parent p
    | Syntax.Difference (cls1, cls2) ->
        visit_regexp ~parent cls1 @ visit_regexp ~parent cls2
    | Syntax.Range (c1, c2) -> if_ ~parent c1.p @ if_ ~parent c2.p
    | Syntax.Union (cls1, cls2) ->
        visit_char_class ~parent cls1 @ visit_char_class ~parent cls2
    | Syntax.Complement cls -> visit_char_class ~parent cls
  in
  let res = visit_regexp re in
  (* log_info ~notify_back "Ranges for pos %s: %s" (Position.show pos)
    (L.to_string (fun s -> Range.show s.SelectionRange.range) res); *)
  res

(* let code_actions (state : state) ~uri ~(range : Range.t) : CodeActionResult.t =
  None *)
