open Menhir_lsp_lib.Utils

let white = 30
let red = 31
let green = 32
let yellow = 33
let blue = 34
let purple = 35
let cyan = 36
let regular = 0
let bold = 1
let dimmed = 2
let underlined = 4

let log ?(debug = false) ?(mode = regular) ?(color = white) s =
  Format.kasprintf
    (fun s -> if debug then pr "\x1b[%d;%dm%s\x1b[0m\n%!" mode color s else ())
    s

open L
open O

let log_action s = log ~mode:underlined ~color:green s
let log_error s = log ~mode:bold ~color:red s

(** This module is responsible for driving the a parser automaton encoded in the
    CMLY format. It takes a compiled CMLY Grammar module as input and returns
    and adapter that allows interactive parsing. The facilities provided are
    similar to those of Menhir's Incremental API. *)
module Make (G : MenhirSdk.Cmly_api.GRAMMAR) = struct
  open G

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
    ( terminal * Tokenizer.Token.t,
      nonterminal * Tokenizer.range )
    derivation_tree

  type token_derivation =
    (terminal * Tokenizer.Token.t, nonterminal * Tokenizer.range) derivation

  let yojson_of_derivation ~visit_t ~visit_nt :
      ('t, 'nt) derivation -> Yojson.Safe.t =
    let v =
      object
        inherit [_] deriv_reduce as super
        method zero = `List []
        method plus = Utils.combine_json_lists
        method! visit_Leaf () = visit_t

        method! visit_Branches () nt dv =
          visit_nt nt (super#visit_derivation () dv)
      end
    in
    v#visit_derivation ()

  let yojson_of_simple_derivation : simple_derivation -> Yojson.Safe.t =
    yojson_of_derivation
      ~visit_t:(fun t -> `String (Terminal.name t))
      ~visit_nt:(fun nt dv ->
        `Assoc [ ("nt", `String (Nonterminal.name nt)); ("children", dv) ])

  let rec loop ((start_nt, _, start_lr1) as entrypoint) state
      (stack : (symbol * lr1) list) tokens derivation : simple_derivation_tree =
    (* We require the stream be persistent as opposed to ephemeral. *)
    let t, tokens' = Seq.uncons tokens |> Option.get in

    log ~color:blue ~mode:bold "## State %d, looking at %s" (Lr1.to_int state)
      (Terminal.name t);
    log ~color:blue "### Available transitions";
    print_transitions state;
    log ~color:blue "### Available reductions";
    print_reductions state;
    let default_reduction = Lr1.default_reduction state in
    log ~color:blue "### Default reduction";
    log "%a" (Format.pp_print_option Print.production) default_reduction;
    log ~color:blue "### Stack";
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
        log ~mode:bold ~color:yellow
          "[End Of Stream Conflict] between shifting token %s to state %d and \
           reducing production %a"
          (Terminal.name t) (Lr1.to_int s') Print.production p;
        raise (Conflict ((T t, s'), p))
    | Some s', Some p ->
        log ~mode:bold ~color:yellow
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

  module ActionMap = Map.Make (Int)

  type token = Tokenizer.Token.t

  (* todo: fix the locations. (check out how menhir code calculates positions) *)
  let rec loop_with_positions ((start_nt, _, start_lr1) as entrypoint) state
      (stack : (symbol * lr1) list) (tokens : (terminal * token) Seq.t)
      start_pos end_pos derivation : (token_derivation_tree, _) result =
    (* The token stream should be persistent to enable peeking without consumption. *)
    let ((t, token) as x), tokens' = Seq.uncons tokens |> Option.get in
    let default_reduction = Lr1.default_reduction state in

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
        loop_with_positions entrypoint state'
          (push stack (T t, state'))
          tokens' start_pos (snd token.loc) (push derivation (Leaf x))
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
              | T t1, Leaf (t2, _) -> Terminal.compare t1 t2
              | N nt1, Branches ((nt2, _), _) -> Nonterminal.compare nt1 nt2
              | _ -> 1)
            (L.rev rhs) derivation
        in
        (* Query the transitions of the current top element on the lhs nonterminal to get the next lr1 state *)
        let top_lr1 : lr1 =
          peek stack' |> O.map snd |> O.value ~default:start_lr1
        in
        let data = (lhs, (start_pos, end_pos)) in
        match get_trans top_lr1 (N lhs) with
        | None when Production.kind p = `START ->
            log_action "reducing start production %a" Print.production p;
            log_action "accepting";
            Ok (Branches (data, derivation))
        | None ->
            log_error "rejected nonterminal %s" (Nonterminal.name lhs);
            raise (Rejected (N lhs))
        | Some state' ->
            log_action "reducing production %a to state %d" Print.production p
              (G.Lr1.to_int state');
            loop_with_positions entrypoint state'
              (push stack' (N lhs, state'))
              tokens (fst token.loc) (fst token.loc)
              (push derivation' (Branches (data, L.rev reductees))))
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

  let parse_string (entrypoint : entrypoint)
      (lexer_entry : Tokenizer.entrypoint)
      (lexer_automata : Lex.Lexgen.automata array)
      (lexer_action_map : Terminal.t ActionMap.t) (input : string)
      (token_derivation_tree, _) result =
    let lexbuf = Lexing.from_string input in
    let start_nt, _, start_lr1 = entrypoint in
    let next_token () =
      let t : token = Tokenizer.next_token lexbuf lexer_entry lexer_automata in
      let open O in
      let+ terminal = ActionMap.find_opt t.action lexer_action_map in
      (terminal, t)
    in
    let tokens = Seq.(memoize (of_dispenser next_token)) in
    loop_with_positions entrypoint start_lr1 [] tokens Lexing.dummy_pos
      Lexing.dummy_pos []

  (** Creates a mapping between this [G]'s tokens and the actions of the given
      lexer's rule by searching for token occurrences within the rule's actions.
  *)
  let connect_lexer_entry (entry : OcamllexSyntax.Syntax.entry) :
      terminal option ActionMap.t =
    let actions =
      L.map
        (fun OcamllexSyntax.Located.{ v = _, { v = text; _ }; _ } -> text)
        entry.clauses
    in
    let log s = log_src "connect_lexer" s in
    L.foldi
      (fun (acc : _ ActionMap.t) (idx : int) (text : string) ->
        (* Logic: parse the actions and see if it contains any constructor whose name matches a token known by [G]. *)
        try
          log "parsing action #%d with text %S" idx text;
          let ocaml_ast =
            Ppxlib.Parse.implementation (Lexing.from_string text)
          in
          let constructors = Menhir_lsp_lib.OcamlSymbols.get_tags ocaml_ast in
          log "constructors found: %a"
            (L.pp (fun out t -> pf out "%s" (Ppxlib.Loc.txt t)))
            constructors;
          let v =
            Terminal.fold
              (fun token acc' ->
                if
                  L.exists
                    (fun loc ->
                      String.equal (Terminal.name token) (Ppxlib.Loc.txt loc))
                    constructors
                then Some token
                else acc')
              None
          in
          ActionMap.add idx v acc
        with _ ->
          log "failed to parse action #%d" idx;
          acc)
      ActionMap.empty actions
end
