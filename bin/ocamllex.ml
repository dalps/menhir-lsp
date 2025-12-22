open OcamllexSyntax
open Utils
open Located

type state = {
  grammar : Syntax.lexer_definition;
  symbols : string located list;
}

let process_symbols (grammar : Syntax.lexer_definition) : string located list =
  let module S = Syntax in
  let open S in
  let open L in
  let rec visit_entry (entry : (string located list, location) entry) =
    (entry.name :: entry.args)
    @ (entry.clauses >>= fun (re, _action) -> visit_regexp re)
  and visit_regexp = function Bind (_, n) -> [ n ] | _ -> []
  and visit_named_regexp
      ((name, (loc, regexp)) : string * (location * regular_expression)) =
    locate loc name :: visit_regexp regexp
  in
  let* s_entries = grammar.entrypoints in
  let* s_regexps = CCHashtbl.to_list grammar.named_regexps in
  visit_entry s_entries @ visit_named_regexp s_regexps

(* repetitive, functorize *)
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
      ~selectionRange:range ())
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
