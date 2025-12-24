open OcamllexSyntax
open Utils
open Located

type state = {
  grammar : Syntax.lexer_definition;
  symbols : string located list;
}

let rec regexp_bindings = function
  | Syntax.Sequence (re1, re2) | Alternative (re1, re2) ->
      regexp_bindings re1 @ regexp_bindings re2
  | Repetition re -> regexp_bindings re
  | Bind (re, n) -> n :: regexp_bindings re
  | _ -> []

let process_symbols (grammar : Syntax.lexer_definition) : string located list =
  let module S = Syntax in
  let open S in
  let open L in
  let rec visit_entry (entry : (string located list, location) entry) =
    (entry.name :: entry.args)
    @ (entry.clauses >>= fun (re, _action) -> visit_regexp re)
  and visit_regexp = function
    | Sequence (re1, re2) | Alternative (re1, re2) ->
        visit_regexp re1 @ visit_regexp re2
    | Repetition re -> visit_regexp re
    | Bind (re, n) -> n :: visit_regexp re
    | Ref n -> [ n ]
    | Characters _ | _ -> []
  and visit_named_regexp
      ((name, (loc, regexp)) : string * (location * regular_expression)) =
    locate loc name :: visit_regexp regexp
  in
  let f = L.flat_map in
  let s_entries = f visit_entry grammar.entrypoints in
  let s_regexps =
    f visit_named_regexp @@ CCHashtbl.to_list grammar.named_regexps
  in
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
  try
    let grammar = OcamllexSyntax.Main.parse_string contents in
    let symbols = process_symbols grammar in
    Ok { grammar; symbols }
  with exn ->
    let diags =
      match exn with
      | Syntax.SyntaxError { v = msg; p } ->
          [
            Diagnostic.create ~message:(`String msg)
              ~range:(Range.of_lexical_positions p)
              ();
          ]
      | Parser.Error ->
          [
            Diagnostic.create ~message:(`String "There are syntax errors.")
              ~range:Range.first_line ();
          ]
      | _ -> []
    in
    Error diags

let document_symbols ({ grammar; _ } : state) : DocumentSymbol.t list =
  L.(
    let+ entry = grammar.entrypoints in
    let range = Range.of_lexical_positions entry.name.p in
    DocumentSymbol.create ~kind:Function ~name:entry.name.v ~range
      ~selectionRange:range
      ~children:
        L.(
          entry.clauses
          |> flat_map_i (fun i (regexp, r) ->
              let range = Range.of_lexical_positions r in
              match regexp_bindings regexp with
              | [] -> []
              | binders ->
                  [
                    DocumentSymbol.create ~kind:Enum ~name:(spr "case %d" i)
                      ~range ~selectionRange:range
                      ~children:
                        (let+ binder = binders in
                         let range = Range.of_lexical_positions binder.p in
                         DocumentSymbol.create ~kind:Variable ~name:binder.v
                           ~range ~selectionRange:range ())
                      ();
                  ]))
      ())
  @ Hashtbl.fold
      (fun name (p, _) ->
        let range = Range.of_lexical_positions p in
        DocumentSymbol.create ~kind:Property ~name ~range ~selectionRange:range
          ()
        |> L.cons)
      grammar.named_regexps []

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
           (fun (name, (range, _)) ->
             if_ (fun _ -> String.equal name sym.v) (locate range name))
           (CCHashtbl.to_list grammar.named_regexps)
   in
   Location.create ~range:(Range.of_lexical_positions def.p) ~uri)
  |> O.to_list
  |> fun locs -> `Location locs

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
        ] );
      ( "*",
        None,
        None,
        [
          md_fenced "regexp *";
          "(repetition) Match the concatenation of zero or more strings that \
           match `regexp`.";
        ] );
      ( "+",
        None,
        None,
        [
          md_fenced "regexp +";
          "(strict repetition) Match the concatenation of one or more strings \
           that match `regexp`.";
        ] );
      ( "?",
        None,
        None,
        [
          md_fenced "regexp ?";
          "(option) Match the empty string, or a string matching `regexp`.";
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
        ] );
    ]

let keyword_completions : CompletionItem.t list =
  compile_completions ~kind:Keyword
    [
      ("let", None, None, [ md_fenced "let ident = regexp" ]);
      ( "rule",
        None,
        None,
        [
          md_fenced
            {|rule entrypoint1 [arg1… argn] =
  parse regexp { action }
      | …
      | regexp { action }
and entrypoint2 [arg1… argn] =
  parse …
and …|};
        ] );
      ("and", None, None, []);
      ("parse", None, None, []);
      ("shortest", None, None, []);
      ("refill", None, None, []);
      ("eof", None, None, [ "Match the end of the lexer input." ]);
      ( "as",
        None,
        None,
        [
          md_fenced "regexp as ident";
          "Bind the substring matched by `regexp` to identifier `ident`.";
        ] );
    ]

let completions_for_action (pos : Position.t) ({ grammar; _ } : state) =
  (* in actions, we want to suggest `lexbuf`, the variables bound with `as` in the current clause and the other lexer entrypoints *)
  L.(
    let* rule = grammar.entrypoints in
    let* regexp, r = rule.clauses in
    let range = Range.of_lexical_positions r in
    if Position.compare_inclusion pos range = `Inside then
      (let+ arg = rule.args in
       CompletionItem.create ~kind:Value ~label:arg.v ())
      @ (let+ entry = grammar.entrypoints in
         CompletionItem.create ~kind:Function ~label:entry.name.v ())
      @ (let+ binder = regexp_bindings regexp in
         CompletionItem.create ~kind:Value ~label:binder.v ())
      @ compile_completions ~kind:Value
          [
            ( "lexbuf",
              None,
              None,
              [
                "The current lexer buffer.\n\n\
                 Can be used in conjunction with the operations on lexer \
                 buffers provided by the `Lexing` standard library module. \
                 [See examples in the \
                 manual](https://ocaml.org/manual/5.4/lexyacc.html#ss:ocamllex-actions)";
              ] );
          ]
    else [])

let completions ({ grammar = { header; trailer; _ }; _ } as state : state)
    ~(pos : Position.t) : CompletionItem.t list =
  let header = Range.of_lexical_positions header in
  let trailer = Range.of_lexical_positions trailer in
  if
    Position.(
      compare_inclusion pos header = `Inside
      || compare_inclusion pos trailer = `Inside)
  then []
  else
    match completions_for_action pos state with
    | [] ->
        regex_operator_completions @ keyword_completions
        @ Hashtbl.fold
            (fun name (_, _) ->
              (* let _range = Range.of_lexical_positions p in *)
              CompletionItem.create ~kind:Property ~label:name () |> L.cons)
            state.grammar.named_regexps []
    | l -> l

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
