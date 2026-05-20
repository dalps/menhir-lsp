open OcamllexSyntax
open OcamllexDocs
open Utils
open Syntax

type zone =
  | OCaml
  | RegexpDefinition of string located list
  | Case
  | Action of string located list

type state = {
  grammar : lexer_definition;
  symbols : string located list;
  regexps :
    [ `Declared of named_regexp located
    | `Anonymous of regular_expression_syntax located ]
    list;
  intervals : zone Ivl_map.t;
}

let yojson_of_ast (grammar : lexer_definition) : Json.t =
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
               @@ List.map (fun ({ text; _ } : Located.comment) ->
                   `Assoc [ ("text", string text) ])
            |> O.get_or_nil
          in
          `Assoc
            [
              ("range", range);
              ("rawRange", `List [ `Int start; `Int end_ ]);
              ("value", value);
            ]

      method! visit_named_regexp =
        with_label "named_regexp" super#visit_named_regexp

      method! visit_regular_expression_syntax =
        with_label "regular_expression" super#visit_regular_expression_syntax

      method! visit_character_class_syntax =
        with_label "character_class" super#visit_character_class_syntax

      method! visit_action = with_label "action" super#visit_action
      method! visit_entry = with_label "entry" super#visit_entry
      method! visit_case = with_label "case" super#visit_case
      method! visit_string _ = string
    end
  in
  v#visit_main () grammar

let regexp_bindings =
  let v =
    object
      inherit [_] ast_reduce as super
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
      inherit [_] ast_reduce as super
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
        let+ { v = re, _; _ } = entry.clauses in
        `Anonymous re)
  in
  let map_ref = ref Ivl_map.empty in
  let add_interval : 'a. zone -> Ivl.t -> unit =
   fun (zone : zone) (ivl : Ivl.t) -> map_ref := Ivl_map.add ivl zone !map_ref
  in
  let add_located : 'a. zone -> 'a located -> unit =
   fun (zone : zone) (located : _ located) ->
    let start, end_ = located.p in
    let ivl = Ivl.create (Included start.pos_cnum) (Included end_.pos_cnum) in
    add_interval zone ivl
  in
  let ocaml_zone loc = add_located OCaml loc in
  let case_zone loc = add_located Case loc in

  (* Stores the named regexp defined insofar into the declaration zone. *)
  let named_regexp_ref : string located list ref = ref [] in

  (* Stores the name and arguments of the currently visited entry. *)
  let current_rule_ref : string located list ref = ref [] in

  (* Stores the names bound in the currently visited entry case. *)
  let case_vars_ref : _ list ref = ref [] in

  let add_case_var var = case_vars_ref := var :: !case_vars_ref in

  let regexp_zone loc = add_located (RegexpDefinition !named_regexp_ref) loc in
  let action_zone loc =
    let start, end_ = loc.p in
    (* Don't count the braces in *)
    let ivl = Ivl.create (Excluded start.pos_cnum) (Excluded end_.pos_cnum) in
    add_interval (Action (!current_rule_ref @ !case_vars_ref)) ivl
  in
  let v =
    object (self)
      inherit [_] ast_iter as super

      method! visit_lexer_definition =
        fun _ ld ->
          O.iter ocaml_zone ld.header;
          L.iter
            (fun loc ->
              regexp_zone loc;
              self#visit_named_regexp () loc.v)
            ld.named_regexps;
          L.iter
            (fun loc ->
              case_zone loc;
              self#visit_entry () loc.v)
            ld.entrypoints;
          O.iter ocaml_zone ld.refill_handler;
          O.iter ocaml_zone ld.trailer

      method! visit_named_regexp =
        fun _ nr ->
          named_regexp_ref := nr.name :: !named_regexp_ref;
          super#visit_named_regexp () nr

      (* No need to add the rule's name and params manually, they are included in merlin completions *)
      (* method! visit_entry =
        fun _ entry ->
          current_rule_ref := entry.name :: entry.args;
          L.iter
            (fun loc ->
              case_zone loc;
              self#visit_case () loc.v)
            entry.clauses *)

      method! visit_action _ = action_zone

      method! visit_case =
        fun _ case ->
          case_vars_ref := [];
          super#visit_case () case

      method! visit_As = fun _ _regexp binder -> add_case_var binder
    end
  in
  v#visit_main () grammar;
  { grammar; symbols; regexps; intervals = !map_ref }

let document_symbols ({ grammar; _ } : state) : DocumentSymbol.t list =
  L.(
    let+ { v = entry; p; _ } = grammar.entrypoints in
    let range = Range.of_lexical_positions p in
    let selectionRange = Range.of_lexical_positions p in
    DocumentSymbol.create ~kind:Function ~name:entry.name.v ~range
      ~selectionRange
      ~children:
        (entry.clauses
        |> flat_map_i (fun _i { v = regexp, _; _ } ->
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
      (fun ({ v = { name; _ }; p; _ } : named_regexp located) ->
        let range = Range.of_lexical_positions p in
        let selectionRange = Range.of_lexical_positions p in
        DocumentSymbol.create ~kind:Property ~name:name.v ~range ~selectionRange
          ())
      grammar.named_regexps

let diagnostics _ = []

let references (state : state) ~uri ~(pos : Position.t) : Location.t list =
  (let open O in
   let+ _sym_range, sym = symbol_at_position state pos in
   L.filter_map
     (fun { v; p; _ } ->
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

let completions
    ({ grammar = { header; trailer; refill_handler; _ } as grammar; _ } as state :
      state) ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
    ~(word : word option) ~(pos : Position.t) ~(uri : uri) :
    CompletionItem.t list =
  let open O in
  let merlin_compls () =
    (let* word = word in
     get_merlin_compls ~notify_back ~uri ~pos word)
    |> get_or_nil
  in
  let lexer_completions () =
    regex_operator_completions @ keyword_completions
    @ L.map
        (fun ({ v = { name; _ }; _ } : named_regexp located) ->
          CompletionItem.create ~kind:Property ~label:name.v ())
        grammar.named_regexps
  in
  get_lazy lexer_completions
  @@
  let* { offset; _ } = word in
  let query = Ivl.create (Included offset) (Included offset) in
  (* Inside actions we shall suggest `lexbuf`, the variables bound with `as` in the current clause, the lexer entrypoints, and OCaml symbols *)
  let action_completions binders =
    let open L in
    (* Merlin alread reports the rule's args... *)
    (* (let+ arg = rule.args in
    CompletionItem.create ~kind:Value ~label:arg.v ()) *)

    (* ...but not the lexer rules themselves. *)
    (let+ { v = entry; _ } = grammar.entrypoints in
     CompletionItem.create ~kind:Function ~label:entry.name.v ())
    @ (let+ binder = binders in
       CompletionItem.create ~kind:Value ~label:binder.v
         ~documentation:(`String "(previously captured in this case)") ())
    @ lexbuf @ merlin_compls ()
  in
  let res = Ivl_map.query_interval ~order:Desc query state.intervals in
  let* (ivl, zones), gen = Ivl_map.Gen.next res in
  let+ innermost_zone = L.head_opt zones in
  match innermost_zone with
  | OCaml -> merlin_compls ()
  | RegexpDefinition defs ->
      regex_operator_completions @ keyword_completions
      @ L.map
          (fun (name : string located) ->
            CompletionItem.create ~kind:Property ~label:name.v ())
          defs
  | Case -> lexer_completions ()
  | Action binders -> action_completions binders

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
      inherit [_] ast_iter

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

let format (state : state) ~(doc : Text_document.t)
    ~options:(_ : FormattingOptions.t) : TextEdit.t list =
  let newText = Menhirformat_lib.Ocamllex.main ~doc ~ast:state.grammar in
  [ TextEdit.create ~newText ~range:Range.(whole_document doc) ]
