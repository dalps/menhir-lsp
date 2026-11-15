open Menhir_lsp_lib.Utils
module MF = Menhirformat_lib
module Located = Utils.Range.Located
open L
open O

let log_action s = log_src "action" s
let log_error s = log_src "error" s
let ( !! ) = Option.get

(** This functor returns a module that extends the [MenhirSdk.Cmly_api.GRAMMAR]
    interface with functions to drive the a parser automaton encoded in the CMLY
    format. The facilities exposed are in the same vein of and based on Menhir's
    Incremental API. *)
module Make (G : MenhirSdk.Cmly_api.GRAMMAR) = struct
  module G = G
  open G

  type 'a located = 'a Utils.Range.Located.t

  type entrypoint = {
    start_nt : nonterminal;
    start_prod : production;
    start_lr1 : lr1;
  }

  let get_entrypoint name : entrypoint option =
    L.find_map
      (fun (start_nt, start_prod, start_lr1) ->
        if_
          (fun _ -> String.equal (Nonterminal.name start_nt) name)
          { start_nt; start_prod; start_lr1 })
      Grammar.entry_points

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
  type token = Tokenizer.token

  type token_derivation_tree =
    ((terminal * token) located, nonterminal located) derivation_tree

  type token_derivation =
    ((terminal * token) located, nonterminal located) derivation

  type json = Yojson.Safe.t

  module T = Terminal
  module NT = Nonterminal
  module P = Production
  module X = Symbol

  let yotagged tag value : json =
    `Assoc [ ("kind", `String tag); ("value", value) ]

  let yodict assoc : json = `Assoc assoc
  let yolist f l : json = `List (L.map f l)
  let yolr1 x : json = `Int (Lr1.to_int x)
  let yosymbol x : json = `String (X.name x)
  let yoterminal t : json = `String (T.name t)
  let yononterminal t : json = `String (NT.name t)

  let yoproduction (p : production) : json =
    `Assoc
      [
        ("lhs", `String (NT.name (P.lhs p)));
        ("rhs", yolist (fun (x, _, _) -> yosymbol x) (P.rhs p |> Array.to_list));
      ]

  let yojson_of_derivation ~visit_t ~visit_nt =
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
    (v#visit_derivation (), v#visit_derivation_tree ())

  let yojson_of_simple_derivation, yojson_of_simple_derivation_tree =
    yojson_of_derivation ~visit_t:yoterminal ~visit_nt:yononterminal

  let yojson_of_token_derivation, yojson_of_token_derivation_tree =
    yojson_of_derivation
      ~visit_t:
        (Located.yojson_of_t (fun (t, tok) ->
             `Assoc
               [
                 ("terminal", yoterminal t);
                 ("token", Tokenizer.Token.yojson_of_t tok);
               ]))
      ~visit_nt:(Located.yojson_of_t yononterminal)

  let rec loop ({ start_nt; start_lr1; _ } as entrypoint) state
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
    let { start_nt; start_lr1 } = entrypoint in
    let tokens = input |> L.map terminal_of_name |> L.to_seq in
    let tokens = Seq.(append tokens (repeat end_pseudotoken)) in
    loop entrypoint start_lr1 [] tokens []

  module ActionMap = struct
    include Map.Make (Int)
  end

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

  let has_defred (s : lr1) = is_some (Lr1.default_reduction s)

  type stack = (symbol * lr1) located list
  type lookahead = terminal * token

  let rec loop_with_positions ({ start_nt; start_lr1; _ } as entrypoint) state
      (stack : stack) (tok : lookahead option)
      (tokens : (lookahead, string) result Seq.t) (please_discard : bool)
      (derivation : token_derivation) : (token_derivation_tree, _) result =
    let open Utils.Range in
    let module Range = Utils.Range in
    let open Utils.Range.Located in
    let attach_derivation res = R.map_err (fun err -> (err, derivation)) res in
    let default_reduction = Lr1.default_reduction state in

    let open R in
    let* tok =
      if please_discard then read tokens |> attach_derivation
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
          (Some tok) tokens please_discard
          (push derivation (Leaf (Located.map (fun _ -> (t, token)) token)))
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
              | T t1, Leaf t2 -> Terminal.compare t1 (fst t2.v)
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
            Error (`Rejected (N lhs)) |> attach_derivation
        | Some state' ->
            log_action "reducing production %a to state %d" Print.production p
              (G.Lr1.to_int state');
            loop_with_positions entrypoint state'
              (push stack' (locate (N lhs, state') range))
              (Some tok) tokens
              false (* Never discard the lookahead token after a reduction *)
              (push derivation' (Branches (data, L.rev terms)))
        (* These two following cases are silly because Menhir resolves the conflicts when building the tables. todo: confirm and remove *)
        )
    | Some s', Some p when Terminal.equal t end_pseudotoken ->
        log
          "[End Of Stream Conflict] between shifting token %s to state %d and \
           reducing production %a"
          (Terminal.name t) (Lr1.to_int s') Print.production p;
        Error (`EOS_conflict ((T t, s'), p)) |> attach_derivation
    | Some s', Some p ->
        log
          "[Shift/Reduce Conflict] between shifting token %s to state %d and \
           reducing production %a"
          (Terminal.name t) (Lr1.to_int s') Print.production p;
        Error (`ShiftReduce_conflict ((T t, s'), p)) |> attach_derivation
    | None, None ->
        log_error "rejected token %s" (Terminal.name t);
        Error (`Rejected (T t)) |> attach_derivation

  (* Modeling the things a semantic action of a lexer rule can do. *)
  type lexer_behavior = Skip | Fail | Produce of int

  let lexer_behavior_map_of_yojson : json list -> lexer_behavior ActionMap.t =
    L.fold_left
      (fun acc obj ->
        match obj with
        | `Assoc
            [
              _;
              ("action", `Int k);
              _;
              ("value", `Assoc (("kind", `String kind) :: data));
            ] -> (
            log "matching action #%d" k;
            match (kind, data) with
            | "skip", _ ->
                log "to Skip";
                ActionMap.add k Skip acc
            | "fail", _ ->
                log "to Fail";
                ActionMap.add k Fail acc
            | "token", ("token", `Assoc (("id", `Int t) :: _)) :: _ ->
                log "to Produce %d" t;
                ActionMap.add k (Produce t) acc
            | _ ->
                log "bad payload";
                acc)
        | _ -> acc)
      ActionMap.empty

  let make_token_supplier (lexer_entry : Tokenizer.entrypoint)
      (lexer_automata : Lex.Lexgen.automata array)
      (lexer_action_map : lexer_behavior ActionMap.t) (lexbuf : Lexing.lexbuf) =
    let rec next_token () =
      let tok : token =
        Tokenizer.next_token lexbuf lexer_entry lexer_automata
      in
      let data = tok.v in
      let open O in
      match ActionMap.find_opt data.action lexer_action_map with
      | None ->
          Error (spr "unbound action: don't know how to interpret %s" data.text)
      | Some terminal -> (
          match terminal with
          | Skip -> next_token ()
          | Fail -> Error "lexer error"
          | Produce term -> Ok (Terminal.of_int term, tok))
    in
    next_token

  let parse_with_positions (entrypoint : entrypoint)
      (lexer_entry : Tokenizer.entrypoint)
      (lexer_automata : Lex.Lexgen.automata array)
      (lexer_action_map : lexer_behavior ActionMap.t) (lexbuf : Lexing.lexbuf) :
      (token_derivation_tree, _) result =
    let { start_nt; start_lr1; _ } = entrypoint in
    let next_token =
      make_token_supplier lexer_entry lexer_automata lexer_action_map lexbuf
    in
    let tokens =
      (* Seq.(memoize (of_dispenser (next_token >> O.of_result))); *)
      Seq.(of_dispenser (next_token >> O.some))
    in
    loop_with_positions entrypoint start_lr1 [] None tokens true []

  open Tokenizer.Persistent

  let make_persistent_token_supplier (lexer_entry : Tokenizer.entrypoint)
      (lexer_automata : Lex.Lexgen.automata array)
      (lexer_action_map : lexer_behavior ActionMap.t) (buffer : bytes) =
    let rec next_token (lexbuf : Tokenizer.Persistent.lexbuf) =
      let lexbuf', tok =
        Tokenizer.Persistent.next_token buffer lexbuf lexer_entry lexer_automata
      in
      let data = tok.v in
      match ActionMap.find_opt data.action lexer_action_map with
      | None ->
          ( lexbuf',
            Error
              (spr "empty action: don't know how to interpret the text %S"
                 data.text) )
      | Some terminal -> (
          match terminal with
          | Skip -> next_token lexbuf'
          | Fail -> (lexbuf', Error "lexer error")
          | Produce term -> (lexbuf', Ok (Terminal.of_int term, tok)))
    in
    next_token

  type env = {
    entrypoint : entrypoint;
    state : lr1;
    lookahead : lookahead option;
    lexbuf : Tokenizer.Persistent.lexbuf;
    stack : stack;
    derivation : token_derivation;
    last : checkpoint;
        (** A chain of checkpoints that led up to the current env. *)
  }

  and checkpoint =
    | InputNeeded of env
    | LexerError of string * env
    | Shifting of terminal * env
    | Reducing of production * env
    | Accepted of token_derivation_tree * env
    | Rejected of symbol * env
  (* [@@deriving visitors { name = "checkpoint_reduce"; variety = "reduce" }] *)

  let env_equal : env -> env -> bool =
   fun e1 e2 ->
    Lr1.equal e1.state e2.state
    && e1.lexbuf = e2.lexbuf
    && CCEqual.option
         (fun (t1, _) (t2, _) -> Terminal.equal t1 t2)
         e1.lookahead e2.lookahead

  let yojson_of_lookahead ((t, tok) : lookahead) =
    `Assoc
      [ ("terminal", yoterminal t); ("token", Tokenizer.Token.yojson_of_t tok) ]

  let yojson_of_stack (stack : stack) : json =
    yolist
      (Located.yojson_of_t (fun (x, lr1) ->
           `Assoc [ ("symbol", yosymbol x); ("state", yolr1 lr1) ]))
      stack

  let yojson_of_env ({ entrypoint; state; lookahead; stack; derivation } : env)
      : json =
    `Assoc
      [
        ("stack", yojson_of_stack stack);
        ("state", yolr1 state);
        ("derivation", yojson_of_token_derivation derivation);
        ("lookahead", yojson_of_option yojson_of_lookahead lookahead);
      ]

  let yojson_of_checkpoint (c : checkpoint) : json =
    let with_env env l = `Assoc (("env", yojson_of_env env) :: l) in
    match c with
    | InputNeeded env -> yotagged "inputNeeded" (with_env env [])
    | LexerError (msg, env) ->
        yotagged "lexerError" (with_env env [ ("message", `String msg) ])
    | Shifting (t, env) ->
        yotagged "shifting" (with_env env [ ("terminal", yoterminal t) ])
    | Reducing (p, env) ->
        yotagged "reducing" (with_env env [ ("production", yoproduction p) ])
    | Accepted (dv, _) ->
        yotagged "accepted" (yojson_of_token_derivation_tree dv)
    | Rejected (x, env) ->
        yotagged "rejected" (with_env env [ ("symbol", yosymbol x) ])

  let should_discard ({ state = s } : env) =
    not (has_defred s && has_red_on_sharp s)

  let step
      ({ entrypoint; state; lookahead = tok; stack; derivation } as env : env)
      (please_discard : bool) : checkpoint =
    let open Utils.Range in
    let module Range = Utils.Range in
    let open Utils.Range.Located in
    let default_reduction = Lr1.default_reduction state in

    if please_discard then InputNeeded env
    else
      let tok =
        (* Since the loop is called with [please_discard] set to [true] and we don't need to worry about recognizing the empty language or the singleton epsilon language (because Menhir rejects them statically), this [Option.get] should be safe and sound. *)
        !!tok
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
          let env' : env =
            {
              env with
              entrypoint;
              state = state';
              stack = push stack (Located.map (fun _ -> (T t, state')) token);
              lookahead = Some tok;
              derivation =
                push derivation (Leaf (Located.map (fun _ -> (t, token)) token));
            }
          in
          log_action "shifting %s to state %d" (G.Terminal.name t)
            (G.Lr1.to_int state');
          Shifting (t, env')
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
                | T t1, Leaf t2 -> Terminal.compare t1 (fst t2.v)
                | N nt1, Branches (nt2, _) -> Nonterminal.compare nt1 nt2.v
                | _ -> 1)
              (L.rev rhs) derivation
          in
          (* Query the transitions of the current top element on the lhs nonterminal to get the next lr1 state *)
          let top_lr1 : lr1 =
            peek stack'
            |> O.map (fun { v } -> snd v)
            |> O.value ~default:entrypoint.start_lr1
          in
          let range = Range.make last_item_startp first_item_endp in
          let data = locate lhs range in
          match get_trans top_lr1 (N lhs) with
          | None when Production.kind p = `START ->
              log_action "reducing start production %a" Print.production p;
              log_action "accepting";
              Accepted (Branches (data, derivation), env)
          | None ->
              log_error "rejected nonterminal %s" (Nonterminal.name lhs);
              Rejected (N lhs, env)
          | Some state' ->
              log_action "reducing production %a to state %d" Print.production p
                (G.Lr1.to_int state');
              let env' : env =
                {
                  env with
                  entrypoint;
                  state = state';
                  lookahead = Some tok;
                  stack = push stack' (locate (N lhs, state') range);
                  derivation = push derivation' (Branches (data, L.rev terms));
                }
              in
              Reducing (p, env'))
      | _ ->
          log_error "rejected token %s" (Terminal.name t);
          Rejected (T t, env)

  type interpreter_state = {
    mutable current : checkpoint;
    step_forward : checkpoint -> checkpoint;
    step_back : checkpoint -> checkpoint;
    play_forward : checkpoint -> checkpoint;
    play_back : checkpoint -> checkpoint;
  }

  let interpreter_state : interpreter_state option ref = ref None

  (** Computes two closures to step forward an backward on an history of
      checkpoints. These functions are not returned but are encapsulated in the
      interpreter's state. The user is supposed to interact with them through
      the imperative API below.

      Invariant: [step_forward (step_back c) = c] for any [c : checkpoint].

      [step_back (step_forward c) = c] also holds as long as [c] is neither
      [Accepted] or [Rejected], because [step_forward] makes no progress on
      them. *)
  let parse_with_checkpoints (entrypoint : entrypoint)
      (lexer_entry : Tokenizer.entrypoint)
      (lexer_automata : Lex.Lexgen.automata array)
      (lexer_action_map : lexer_behavior ActionMap.t) (buffer : bytes) =
    let next_token =
      make_persistent_token_supplier lexer_entry lexer_automata lexer_action_map
        buffer
    in

    let rec bottom : env =
      {
        entrypoint;
        state = entrypoint.start_lr1;
        lookahead = None;
        stack = [];
        derivation = [];
        lexbuf = start_lexbuf;
        last = InputNeeded bottom;
      }
    in

    (* Initializes a checkpoint for [entrypoint] and resets the history. *)
    let init_checkpoint : checkpoint = InputNeeded bottom in

    (* Unwraps a checkpoint and sets up the environment to call [step]. Also updates the history of checkpoints. *)
    let step_forward (c : checkpoint) : checkpoint =
      match c with
      | InputNeeded env -> (
          match next_token env.lexbuf with
          | lexbuf, Ok tok ->
              step { env with last = c; lexbuf; lookahead = Some tok } false
          | lexbuf, Error msg -> LexerError (msg, { env with lexbuf }))
      | Shifting (_, env) -> step { env with last = c } (should_discard env)
      | Reducing (_, env) -> step { env with last = c } false
      | LexerError _ | Accepted _ | Rejected _ -> (* makes no progress *) c
    in

    (* Steps forwards until a terminal point is reached. *)
    let rec play_forward (c : checkpoint) : checkpoint =
      match c with
      | LexerError _ | Accepted _ | Rejected _ ->
          log "stopping at the top";
          c
      | _ -> play_forward (step_forward c)
    in

    (* Forgets a history step and returns it. *)
    let step_back : checkpoint -> checkpoint = function
      | InputNeeded env
      | LexerError (_, env)
      | Shifting (_, env)
      | Reducing (_, env)
      | Accepted (_, env)
      | Rejected (_, env) ->
          env.last
    in

    (* Steps all the way back to the bottom checkpoint. *)
    let rec play_back (c : checkpoint) : checkpoint =
      match c with
      | InputNeeded env when env_equal env bottom ->
          log "stopping at the bottom";
          c
      | _ -> play_back (step_back c)
    in

    interpreter_state :=
      Some
        {
          current = init_checkpoint;
          step_forward;
          step_back;
          play_forward;
          play_back;
        }

  let get_state () =
    match !interpreter_state with
    | None -> failwith "Interpreter not connected"
    | Some i -> i

  let current_checkpoint () =
    let i = get_state () in
    i.current

  let step_forward () =
    let i = get_state () in
    i.current <- i.step_forward i.current

  let step_back () =
    let i = get_state () in
    i.current <- i.step_back i.current

  let play_forward () =
    let i = get_state () in
    i.current <- i.play_forward i.current

  let play_back () =
    let i = get_state () in
    i.current <- i.play_back i.current

  let disconnect () = interpreter_state := None

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

(** The signature of modules returned by the [Make] functor. *)
module type S = sig
  module G : MenhirSdk.Cmly_api.GRAMMAR
  open G
  open Utils.Range

  type entrypoint = {
    start_nt : nonterminal;
    start_prod : production;
    start_lr1 : lr1;
  }

  val get_entrypoint : string -> entrypoint option
  val print_transitions : lr1 -> unit
  val print_reductions : lr1 -> unit
  val pp_stack : (symbol * lr1) list Utils.L.printer
  val print_stack : (symbol * lr1) list -> unit
  val terminal_of_name : string -> terminal
  val position_of_terminal : terminal -> Utils.Range.t option
  val end_pseudotoken : terminal

  exception Rejected of symbol
  exception Conflict of (symbol * lr1) * production

  type ('t, 'nt) derivation_tree =
    | Leaf of 't
    | Branches of 'nt * ('t, 'nt) derivation

  and ('t, 'nt) derivation = ('t, 'nt) derivation_tree list

  type simple_derivation_tree = (terminal, nonterminal) derivation_tree
  type simple_derivation = (terminal, nonterminal) derivation
  type token = Tokenizer.Token.t

  type token_derivation_tree =
    ((terminal * token) located, nonterminal located) derivation_tree

  type token_derivation =
    ((terminal * token) located, nonterminal located) derivation

  type json = Utils.Json.t

  val yotagged : string -> json -> json
  val yodict : (string * json) list -> json
  val yolist : ('a -> json) -> 'a list -> json
  val yolr1 : lr1 -> json
  val yosymbol : symbol -> json
  val yoterminal : terminal -> json
  val yononterminal : nonterminal -> json
  val yoproduction : production -> json

  val yojson_of_derivation :
    visit_t:('t -> json) ->
    visit_nt:('nt -> json) ->
    (('t, 'nt) derivation -> json) * (('t, 'nt) derivation_tree -> json)

  val yojson_of_simple_derivation : (terminal, nonterminal) derivation -> json

  val yojson_of_simple_derivation_tree :
    (terminal, nonterminal) derivation_tree -> json

  val yojson_of_token_derivation :
    ((terminal * Tokenizer.Token.t) Located.t, nonterminal Located.t) derivation ->
    json

  val yojson_of_token_derivation_tree :
    ( (terminal * Tokenizer.Token.t) Located.t,
      nonterminal Located.t )
    derivation_tree ->
    json

  val loop :
    entrypoint ->
    lr1 ->
    (symbol * lr1) list ->
    terminal Seq.t ->
    (terminal, nonterminal) derivation ->
    simple_derivation_tree

  val parse_terminals : entrypoint -> string list -> simple_derivation_tree

  module ActionMap : module type of Map.Make (Int)

  val lexer_error : string -> [> `LexerError of string ]

  val read :
    ('a * 'b, string) result Seq.t ->
    ('a * 'b, [> `LexerError of string ]) result

  type stack = (symbol * lr1) located list
  type lookahead = terminal * token

  val loop_with_positions :
    entrypoint ->
    lr1 ->
    stack ->
    lookahead option ->
    (lookahead, string) result Seq.t ->
    bool ->
    token_derivation ->
    ( token_derivation_tree,
      [> `EOS_conflict of (symbol * lr1) * production
      | `LexerError of string
      | `Rejected of symbol
      | `ShiftReduce_conflict of (symbol * lr1) * production ]
      * token_derivation )
    result

  type lexer_behavior = Skip | Fail | Produce of int

  val lexer_behavior_map_of_yojson : json list -> lexer_behavior ActionMap.t

  val make_token_supplier :
    Tokenizer.entrypoint ->
    Lex.Lexgen.automata array ->
    lexer_behavior ActionMap.t ->
    Lexing.lexbuf ->
    unit ->
    (terminal * token, string) result

  val parse_with_positions :
    entrypoint ->
    Tokenizer.entrypoint ->
    Lex.Lexgen.automata array ->
    lexer_behavior ActionMap.t ->
    Lexing.lexbuf ->
    ( token_derivation_tree,
      [> `EOS_conflict of (symbol * lr1) * production
      | `LexerError of string
      | `Rejected of symbol
      | `ShiftReduce_conflict of (symbol * lr1) * production ]
      * token_derivation )
    result

  type env = {
    entrypoint : entrypoint;
    state : lr1;
    lookahead : lookahead option;
    lexbuf : Tokenizer.Persistent.lexbuf;
    stack : stack;
    derivation : token_derivation;
    last : checkpoint;
  }

  and checkpoint =
    | InputNeeded of env
    | LexerError of string * env
    | Shifting of terminal * env
    | Reducing of production * env
    | Accepted of token_derivation_tree * env
    | Rejected of symbol * env

  val yojson_of_stack : stack -> json
  val yojson_of_env : env -> json
  val yojson_of_checkpoint : checkpoint -> json

  val parse_with_checkpoints :
    entrypoint ->
    Tokenizer.entrypoint ->
    Lex.Lexgen.automata array ->
    lexer_behavior ActionMap.t ->
    bytes ->
    unit

  val current_checkpoint : unit -> checkpoint
  val step_forward : unit -> unit
  val step_back : unit -> unit
  val play_forward : unit -> unit
  val play_back : unit -> unit
  val disconnect : unit -> unit

  val connect_lexer_entry :
    OcamllexSyntax.Syntax.entry ->
    (string Located.t * lexer_behavior option) ActionMap.t
end
