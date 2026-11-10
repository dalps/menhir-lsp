open Menhir_lsp_lib.Utils
module MF = Menhirformat_lib
module Located = Utils.Range.Located
open L
open O

let log_action s = log_src "action" s
let log_error s = log_src "error" s
let ( !! ) = Option.get

(** This module is responsible for driving the a parser automaton encoded in the
    CMLY format. It takes a compiled CMLY Grammar module as input and returns
    and adapter that does computes derivation trees. The facilities provided are
    similar to those of Menhir's Incremental API. *)
module Make (G : MenhirSdk.Cmly_api.GRAMMAR) = struct
  open G

  type 'a located = 'a Utils.Range.Located.t
  type entrypoint = nonterminal * production * lr1

  let get_entrypoint name : entrypoint option =
    L.find_opt
      (fun (nt, _, _) -> String.equal (Nonterminal.name nt) name)
      Grammar.entry_points

  let main entrypoint_idx =
    let nt, p, lr1 = Grammar.entry_points.-[0] in
    let nt = Nonterminal.name nt in
    nt

  let stats =
    pr "number of lr0 states / lr1 states: %d / %d\n" Lr0.count Lr1.count;
    pr "terminals:\n";
    Terminal.iter (fun t -> pr "- %a\n" Print.terminal t);
    pr "nonterminals:\n";
    let pp_nt_kind t =
      match Nonterminal.kind t with
      | `START -> "`START"
      | `REGULAR -> "`REGULAR"
    in
    Nonterminal.iter (fun t ->
        pr "- [%s] %a : %a\n" (pp_nt_kind t) Print.nonterminal t
          Format.(pp_print_option ~none:(fun _ _ -> pr "unit") pp_print_string)
          (Nonterminal.typ t));
    pr "productions:\n";
    Production.iter (fun t -> pr "* %a" Print.production t)

  let print_transitions start =
    log "transitions from state %d: %a" (Lr1.to_int start)
      (L.pp (fun out (sym, lr1) ->
           pf out "%s %s to state %d"
             (match sym with T _ -> "shift" | N _ -> "reduce")
             (Symbol.name sym) (Lr1.to_int lr1)))
      (Lr1.transitions start)

  let print_reductions start =
    log "%a"
      (L.pp (fun out (t, prod) ->
           pf out "%s, %s%a" (Terminal.name t)
             (match Production.kind prod with
             | `START -> "(START) "
             | `REGULAR -> "")
             Print.production prod))
      (Lr1.get_reductions start)

  let pp_stack =
    L.pp (fun out (sym, lr1) ->
        pf out "%a(%d)" Print.symbol sym (Lr1.to_int lr1))

  let print_stack = log "%a" pp_stack

  let terminal_of_name name =
    G.Terminal.fold
      (fun t acc -> if G.Terminal.name t = name then Some t else acc)
      None
    |> Option.get

  let lift_range r =
    Utils.Range.of_lexical_positions (Range.startp r, Range.endp r)

  (** Get the the location a terminal's declaration. Returns [None] on the
      pseudotokens [error] and [#]. *)
  let position_of_terminal =
    let toks = Surface.Syntax.tokens Surface.before_inlining in
    Terminal.tabulate (fun t ->
        L.find_map
          (fun (name, data) ->
            if Terminal.name t = name then
              Some (Surface.Token.position data |> lift_range)
            else None)
          toks)

  let get ~by ~eq (lr1 : lr1) sym =
    L.find_map (fun (x, state') -> O.wrap (eq sym x) state') (by lr1)

  let get_trans = get ~by:Lr1.transitions ~eq:Symbol.equal
  let get_prod = get ~by:Lr1.get_reductions ~eq:Terminal.equal
  let end_pseudotoken = terminal_of_name "#"

  exception Rejected of symbol
  exception Conflict of (symbol * lr1) * production

  type ('t, 'nt) derivation_tree =
    | Leaf of ('t[@opaque])
    | Branches of ('nt[@opaque]) * ('t, 'nt) derivation

  and ('t, 'nt) derivation = ('t, 'nt) derivation_tree list
  [@@deriving
    visitors { name = "deriv_map"; variety = "map" },
    visitors { name = "deriv_reduce"; variety = "reduce" },
    visitors { name = "deriv_iter"; variety = "iter" },
    visitors { name = "deriv_endo"; variety = "endo" }]

  type simple_derivation_tree = (terminal, nonterminal) derivation_tree
  type simple_derivation = (terminal, nonterminal) derivation

  type token_derivation_tree =
    (terminal located, nonterminal located) derivation_tree

  type token_derivation = (terminal located, nonterminal located) derivation

  let yojson_of_derivation ~visit_t ~visit_nt :
      ('t, 'nt) derivation -> Yojson.Safe.t =
    let v =
      object
        inherit [_] deriv_reduce as super
        method zero = `List []
        method plus = Utils.combine_json_lists
        method! visit_Leaf () = visit_t

        method! visit_Branches () nt dv =
          `Assoc
            [ ("nt", visit_nt nt); ("children", super#visit_derivation () dv) ]
      end
    in
    v#visit_derivation ()

  let yojson_of_simple_derivation : simple_derivation -> Yojson.Safe.t =
    yojson_of_derivation
      ~visit_t:(fun t -> `String (Terminal.name t))
      ~visit_nt:(fun nt -> `String (Nonterminal.name nt))

  let yojson_of_token_derivation : token_derivation -> Yojson.Safe.t =
    yojson_of_derivation
      ~visit_t:(Located.yojson_of_t (fun t -> `String (Terminal.name t)))
      ~visit_nt:(Located.yojson_of_t (fun nt -> `String (Nonterminal.name nt)))

  let rec loop ((start_nt, _, start_lr1) as entrypoint) state
      (stack : (symbol * lr1) list) tokens derivation : simple_derivation_tree =
    (* We require the stream be persistent as opposed to ephemeral. *)
    let t, tokens' = Seq.uncons tokens |> Option.get in

    log "## State %d, looking at %s" (Lr1.to_int state) (Terminal.name t);
    log "### Available transitions";
    print_transitions state;
    log "### Available reductions";
    print_reductions state;
    let default_reduction = Lr1.default_reduction state in
    log "### Default reduction";
    log "%a" (Format.pp_print_option Print.production) default_reduction;
    log "### Stack";
    print_stack stack;

    (* Consume a token and shift to a new state, or reduce a production *)
    let shift_state : lr1 option = get_trans state (T t) in
    let reduce_production : production option =
      default_reduction <|> fun () -> get_prod state t
    in
    match (shift_state, reduce_production) with
    | Some state', None ->
        (* If there is a matching shift transition for the lookahead token, run that, otherwise look for a matching reduction. *)
        log_action "shifting %s to state %d" (G.Terminal.name t)
          (G.Lr1.to_int state');
        loop entrypoint state'
          (push stack (T t, state'))
          tokens' (push derivation (Leaf t))
    | None, Some p -> (
        (* When a default reduction is indicated, there are no possible shift transitions and we continue with a reduction. *)
        let rhs =
          Production.rhs p |> Array.to_list |> L.map (fun (s, _, _) -> s)
        in
        let lhs = Production.lhs p in
        (* Remove the top elements corresponding to the production's RHS from the stack *)
        let stack' =
          sub ~cmp:(fun x1 (x2, _) -> Symbol.compare x1 x2) (L.rev rhs) stack
        in
        let reductees, derivation' =
          sub_split
            ~cmp:(fun symbol derivation_tree ->
              match (symbol, derivation_tree) with
              | T t1, Leaf t2 -> Terminal.compare t1 t2
              | N nt1, Branches (nt2, _) -> Nonterminal.compare nt1 nt2
              | _ -> 1)
            (L.rev rhs) derivation
        in
        (* Query the transitions of the current top element on the lhs nonterminal to get the next lr1 state *)
        let top_lr1 : lr1 =
          peek stack' |> O.map snd |> O.value ~default:start_lr1
        in
        match get_trans top_lr1 (N lhs) with
        | None when Production.kind p = `START ->
            log_action "reducing start production %a" Print.production p;
            log_action "accepting";
            Branches (lhs, derivation)
        | None ->
            log_error "rejected nonterminal %s" (Nonterminal.name lhs);
            raise (Rejected (N lhs))
        | Some state' ->
            log_action "reducing production %a to state %d" Print.production p
              (G.Lr1.to_int state');
            loop entrypoint state'
              (push stack' (N lhs, state'))
              tokens
              (push derivation' (Branches (lhs, L.rev reductees))))
    | Some s', Some p when Terminal.equal t end_pseudotoken ->
        log
          "[End Of Stream Conflict] between shifting token %s to state %d and \
           reducing production %a"
          (Terminal.name t) (Lr1.to_int s') Print.production p;
        raise (Conflict ((T t, s'), p))
    | Some s', Some p ->
        log
          "[Shift/Reduce Conflict] between shifting token %s to state %d and \
           reducing production %a"
          (Terminal.name t) (Lr1.to_int s') Print.production p;
        raise (Conflict ((T t, s'), p))
    | None, None ->
        log_error "rejected token %s" (Terminal.name t);
        raise (Rejected (T t))

  let parse_terminals (entrypoint : entrypoint) (input : string list) =
    let start_nt, _, start_lr1 = entrypoint in
    let tokens = input |> L.map terminal_of_name |> L.to_seq in
    let tokens = Seq.(append tokens (repeat end_pseudotoken)) in
    loop entrypoint start_lr1 [] tokens []

  module ActionMap = struct
    include Map.Make (Int)

    (* todo: move json (de)serializer here *)
  end

  type token = Tokenizer.Token.t

  let lexer_error msg =
    log_error "lexer failure: %s" msg;
    `LexerError msg

  (** The sequence [tokens] should also be infinite so that [Option.get] shall
      never fail. This is guaranteed by the built in behaviour of the lexer we
      use: whenever the lexbuf is at the end of file, it always returns the same
      value. *)
  let read tokens =
    let open R in
    let res, _ = !!(Seq.uncons tokens) in
    let+ t, token = res |> R.map_err lexer_error in
    (t, token)

  (* Checks whether [s] has a reduction on the [#] pseudotoken. This is used for setting the
      [please_discard] flag: when [s] also has a default reduction, then it's on [#], which
      appears when the start symbol is about to be reduced, so reading the input stream is not necessary. *)
  let has_red_on_sharp (s : lr1) =
    L.exists
      (fun (t, _) -> Terminal.equal t end_pseudotoken)
      (Lr1.get_reductions s)

  let rec loop_with_positions ((start_nt, _, start_lr1) as entrypoint) state
      (stack : (symbol * lr1) located list) (tok : (terminal * token) option)
      (tokens : (terminal * token, string) result Seq.t) start_pos end_pos
      (please_discard : bool) (derivation : token_derivation) :
      (token_derivation_tree, _) result =
    let open Utils.Range in
    let module Range = Utils.Range in
    let open Utils.Range.Located in
    let default_reduction = Lr1.default_reduction state in

    let open R in
    let* tok =
      if please_discard then read tokens
      else
        (* Since the loop is called with [please_discard] set to [true] and we don't need to worry about recognizing the empty language or the singleton epsilon language (because Menhir rejects them statically), this [Option.get] should be safe and sound. *)
        Ok !!tok
    in
    let t, token = tok in
    let start_pos, end_pos = (token.p.start, token.p.end_) in

    let shift_state : lr1 option = get_trans state (T t) in

    (* Note: a default reduction does not need a lookahead token *)
    let reduce_production : production option =
      default_reduction <|> fun () -> get_prod state t
    in

    log "###################################################";
    log "looking at %a, please_discard: %B"
      (fun out (t, _) -> pf out "%a" Print.terminal t)
      tok please_discard;
    log "default reduction for state %d: %a" (Lr1.to_int state)
      (pp_option Print.production)
      default_reduction;
    log "shift transition: %a"
      (pp_option (fun out d -> pf out "%d" (Lr1.to_int d)))
      shift_state;
    log "reduce transition: %a" (pp_option Print.production) reduce_production;

    (* Selects between the (default) reduction on the current lookahead or shifting a new token. *)
    match (shift_state, reduce_production) with
    | Some state', None ->
        (* Hold off reading another token if we've arrived at the start symbol.
          See https://cambium.inria.fr/~fpottier/menhir/manual.html#sec50 and middle/ReferenceInterpreter.ml#166 and docstring of [test_default_reduction] *)
        let please_discard =
          match Lr1.default_reduction state' with
          | Some _ when has_red_on_sharp state' ->
              log
                "will not call the lexer next run because we're about to \
                 reduce the start production";
              false
          | _ -> true
        in

        log_action "shifting %s to state %d" (G.Terminal.name t)
          (G.Lr1.to_int state');
        loop_with_positions entrypoint state'
          (push stack (Located.map (fun _ -> (T t, state')) token))
          (Some tok) tokens start_pos end_pos please_discard
          (push derivation (Leaf (Located.map (fun _ -> t) token)))
    | None, Some p -> (
        (* When a default reduction is indicated, there are no possible shift transitions and we continue with a reduction. *)
        let rhs =
          Production.rhs p |> Array.to_list |> L.map (fun (s, _, _) -> s)
        in
        let lhs = Production.lhs p in
        (* Pop the top elements corresponding to the production's RHS from the stack *)
        let items, stack' =
          sub_split
            ~cmp:(fun x1 { v = x2, _; _ } -> Symbol.compare x1 x2)
            (L.rev rhs) stack
        in
        (* The start position of the expression being reduced is in the last item popped item.
           What if we're reducing the empty production? (items = nil) *)
        let first_item_endp =
          L.head_opt items |> O.map_or ~default:end_pos Located.endp
        in
        let last_item_startp =
          L.last_opt items |> O.map_or ~default:start_pos Located.startp
        in
        let terms, derivation' =
          sub_split
            ~cmp:(fun symbol (derivation_tree : token_derivation_tree) ->
              match (symbol, derivation_tree) with
              | T t1, Leaf t2 -> Terminal.compare t1 t2.v
              | N nt1, Branches (nt2, _) -> Nonterminal.compare nt1 nt2.v
              | _ -> 1)
            (L.rev rhs) derivation
        in
        (* Query the transitions of the current top element on the lhs nonterminal to get the next lr1 state *)
        let top_lr1 : lr1 =
          peek stack'
          |> O.map (fun { v } -> snd v)
          |> O.value ~default:start_lr1
        in
        let range = Range.make last_item_startp first_item_endp in
        let data = locate lhs range in
        match get_trans top_lr1 (N lhs) with
        | None when Production.kind p = `START ->
            log_action "reducing start production %a" Print.production p;
            log_action "accepting";
            Ok (Branches (data, derivation))
        | None ->
            log_error "rejected nonterminal %s" (Nonterminal.name lhs);
            Error (`Rejected (N lhs))
        | Some state' ->
            log_action "reducing production %a to state %d" Print.production p
              (G.Lr1.to_int state');
            loop_with_positions entrypoint state'
              (push stack' (locate (N lhs, state') range))
              (Some tok) tokens last_item_startp first_item_endp
              false (* Never discard the lookahead token after a reduction *)
              (push derivation' (Branches (data, L.rev terms)))
        (* These two following cases are silly because Menhir resolves the conflicts when building the tables. todo: confirm and remove *)
        )
    | Some s', Some p when Terminal.equal t end_pseudotoken ->
        log
          "[End Of Stream Conflict] between shifting token %s to state %d and \
           reducing production %a"
          (Terminal.name t) (Lr1.to_int s') Print.production p;
        Error (`EOS_conflict ((T t, s'), p))
    | Some s', Some p ->
        log
          "[Shift/Reduce Conflict] between shifting token %s to state %d and \
           reducing production %a"
          (Terminal.name t) (Lr1.to_int s') Print.production p;
        Error (`ShiftReduce_conflict ((T t, s'), p))
    | None, None ->
        log_error "rejected token %s" (Terminal.name t);
        Error (`Rejected (T t))

  (* Modeling the things a semantic action of a lexer rule can do. *)
  type lexer_behavior = Skip | Fail | Produce of int

  let parse_with_positions (entrypoint : entrypoint)
      (lexer_entry : Tokenizer.entrypoint)
      (lexer_automata : Lex.Lexgen.automata array)
      (lexer_action_map : lexer_behavior ActionMap.t) (lexbuf : Lexing.lexbuf) :
      (token_derivation_tree, _) result =
    let start_nt, _, start_lr1 = entrypoint in
    let rec next_token () =
      let tok : token =
        Tokenizer.next_token lexbuf lexer_entry lexer_automata
      in
      let data = tok.v in
      let open O in
      (* todo: filter out lexer actions that do not produce a token (i.e. they either loop or throw an error) *)
      match ActionMap.find_opt data.action lexer_action_map with
      | None ->
          Error (spr "unbound action: don't know how to interpret %s" data.text)
      | Some terminal -> (
          match terminal with
          | Skip -> next_token ()
          | Fail -> Error "lexer error"
          | Produce term -> Ok (Terminal.of_int term, tok))
    in

    let tokens =
      (* Seq.(memoize (of_dispenser (next_token >> O.of_result))); *)
      Seq.(of_dispenser (next_token >> O.some))
    in
    loop_with_positions entrypoint start_lr1 [] None tokens Position.dummy
      Position.dummy true []

  (** Creates a mapping between this [G]'s tokens and the actions of the given
      lexer's rule by searching for token occurrences within the rule's actions.
  *)
  let connect_lexer_entry (entry : OcamllexSyntax.Syntax.entry) :
      (_ * lexer_behavior option) ActionMap.t =
    let actions =
      let open OcamllexSyntax.Located in
      L.map
        (fun { v = regexp, text; _ } ->
          (* Call menhirformat to stringify the regexp *)
          let fmt = new MF.Ocamllex.formatter MF.Utils.Config.default_config in
          let regexp_text =
            fmt#visit_regular_expression_syntax () regexp.v
            |> spr "%a" (PPrint.ToFormatter.pretty 1. 9999)
          in
          Utils.Range.Located.
            (of_lexpos regexp_text regexp.p, of_lexpos text.v text.p))
        entry.clauses
    in
    let log s = log_src "connect_lexer" s in
    L.foldi
      (* todo: [idx] may not correspond to the actual index of the action in the table of actions generated by ocamllex. Please double check. *)
      (fun (acc : _ ActionMap.t) (idx : int)
           ((regexp, text) : string Located.t * string Located.t) ->
        (* Logic: parse the action text and see if it contains any constructor whose name matches a token known by [G] or calls [Lexing.new_line] or resembles code that raises an exception. We're really oversimplifying stuff here and the following static code analysis may be cursed. *)
        let data =
          try
            log "parsing action #%d with text %S" idx text.v;
            let ocaml_ast =
              Ppxlib.Parse.implementation (Lexing.from_string text.v)
            in

            let vars =
              Menhir_lsp_lib.OcamlSymbols.get_fvars ocaml_ast
              |> L.map (fun (v : string Ppxlib.Loc.t) -> v.txt)
            in
            let does_fail =
              L.exists (fun txt -> txt = "raise" || txt = "failwith") vars
            in
            let does_skip = L.mem entry.name.v vars in
            let constructors =
              Menhir_lsp_lib.OcamlSymbols.get_tags ocaml_ast
              |> L.map Ppxlib.Loc.txt
            in
            log "constructors found: %a"
              (L.pp (fun out t -> pf out "%s" t))
              constructors;
            let does_token =
              Terminal.fold
                (fun term acc' ->
                  if L.exists (String.equal (Terminal.name term)) constructors
                  then Some term
                  else acc')
                None
            in
            match (does_fail, does_skip, does_token) with
            | true, _, _ -> Some Fail
            | _, true, _ -> Some Skip
            | _, _, Some v -> Some (Produce (Terminal.to_int v))
            | _ ->
                (* uninterpreted action, the user should set its behaviour through the UI. *)
                None
          with _ ->
            log "failed to parse action #%d" idx;
            (* We could be more lenient and skip it to read the next token instead of diverging here. *)
            None
        in
        ActionMap.add idx (regexp, data) acc)
      ActionMap.empty actions
end
