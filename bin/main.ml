(* This module is modelled after Linol's Lwt template:
https://github.com/c-cube/linol/blob/main/example/template-lwt/main.ml *)

open Types
open Linol_lsp.Types
open Utils
open Loc

let load_state_from_partial_grammar (grammar : M.Syntax.partial_grammar) :
    (state, Diagnostic.t list) result =
  let symbols = Symbol_visitor.process_symbols grammar in
  try
    let tokens : tokens =
      List.filter_map
        (function
          | ({
               p;
               v = M.Syntax.DToken (ocamltype, terminal, alias, _attributes);
             } :
              M.Syntax.declaration located) ->
              Some
                { p; v = ({ ocamltype; terminal; alias; _attributes } : token) }
          | _ -> None)
        grammar.pg_declarations
    in
    Ok { grammar; tokens; symbols }
  with exn ->
    let diags =
      match exn with
      | M.ParserAux.ParserError { v = msg; p }
      | M.Lexer.LexerError { v = msg; p } ->
          [
            Diagnostic.create ~message:(`String msg)
              ~range:(Range.of_lexical_positions p)
              ();
          ]
      | M.Parser.Error ->
          [
            Diagnostic.create ~message:(`String "There are syntax errors.")
              ~range:Range.first_line ();
          ]
      | _ -> []
    in
    Error diags

let load_state_from_contents (file_name : string) (file_contents : string) :
    (state, Diagnostic.t list) result =
  M.Main.load_grammar_from_contents 0 file_name file_contents
  |> load_state_from_partial_grammar

let standard_lib =
  Standard.menhir_standard_library_grammar |> load_state_from_partial_grammar
  |> R.get_exn

(** If we are inside a semantic action, we shall suggest the binders declared in
    the current branch *)
let completions_for_action (pos : Position.t) ({ grammar; _ } : state) :
    CompletionItem.t list =
  let comps =
    L.(
      let* rule = grammar.pg_rules in
      let* branch = rule.pr_branches in
      let range =
        Range.of_lexical_positions
        @@
        match branch.pb_action.expr with
        | M.IL.ETextual { p; _ } -> p
        | _ -> branch.pb_position
      in
      if Position.compare_inclusion pos range = `Inside then
        Keywords.position_keywords
        @
        let+ binder, par, _ = branch.pb_producers in
        let binder =
          O.(
            CCString.chop_prefix ~pre:"_" binder.v
            >|= ( ^ ) "$" |> get_or ~default:binder.v)
        in
        CompletionItem.create ~kind:Variable
          ~detail:
            (match par with
            | M.Syntax.ParamVar p | M.Syntax.ParamApp (p, _) -> p.v
            | M.Syntax.ParamAnonymous _ -> "")
          ~label:binder ()
      else [])
  in
  comps

let completions ?(docs : (string, string) Hashtbl.t = Hashtbl.create 0)
    ({ tokens; grammar; _ } : state) : CompletionItem.t list =
  let open MenhirSyntax.Syntax in
  CCList.flat_map
    (fun (t : token located) ->
      let comp = CompletionItem.create ~kind:CompletionItemKind.Constant in
      let type_doc =
        O.(
          map
            (fun t ->
              match t with
              | Declared { v; _ } | Inferred v ->
                  `MarkupContent
                    (MarkupContent.create ~kind:Markdown
                       ~value:(Utils.md_fenced v)))
            t.v.ocamltype)
      in

      let ld =
        CompletionItemLabelDetails.create
          ?detail:
            O.(
              t.v.ocamltype >|= fun typ ->
              ": " ^ match typ with Declared { v; _ } | Inferred v -> v)
      in
      comp ~label:t.v.terminal ?documentation:type_doc ~labelDetails:(ld ()) ()
      :: O.(
           t.v.alias
           >|= (fun alias ->
           let description = "alias for " ^ t.v.terminal in
           comp ~label:alias ~detail:description
             ?documentation:type_doc
               (* This is persistent text that sits next to the label.
           The previous ~detail argument won't be shown while scrolling
           if there is ~labelDetails. *)
             ~labelDetails:(ld ~description:t.v.terminal ())
             ())
           |> to_list))
    tokens
  @ List.map
      (fun (rule : parameterized_rule) ->
        let label = rule.pr_nt.v in
        let comp =
          CompletionItem.create ~kind:CompletionItemKind.Function ~label
            ?documentation:
              O.(
                CCHashtbl.get docs label >|= fun doc ->
                `MarkupContent (MarkupContent.create ~kind:Markdown ~value:doc))
            ~labelDetails:
              (CompletionItemLabelDetails.create
                 ~detail:
                   (match rule.pr_parameters with
                   | [] -> ""
                   | _ ->
                       L.to_string ~start:"(" ~stop:")"
                         (fun { p = _; v } -> v)
                         rule.pr_parameters)
                 ())
        in
        comp ())
      grammar.pg_rules

let percent_completions =
  CCHashtbl.map_list
    (fun k doclines ->
      let c =
        CompletionItem.create ~kind:CompletionItemKind.Keyword
          ~documentation:
            (`MarkupContent
               (MarkupContent.create ~kind:Markdown
                  ~value:(CCString.concat "\n\n" doclines)))
      in
      if k = "token_t" then
        c ~label:"%token" ~insertTextFormat:InsertTextFormat.Snippet
          ~insertText:"token <$1> $0"
          ~labelDetails:
            (CompletionItemLabelDetails.create ~detail:" <typexpr>" ())
          ()
      else c ~label:("%" ^ k) ~insertText:k ())
    Keywords.declarations

let standard_lib_completions =
  completions standard_lib ~docs:Standard.menhir_standard_library_doc

let document_symbols ({ grammar = { pg_rules; _ }; tokens; _ } : state) :
    DocumentSymbol.t list =
  (* Here we extract a listing of the defined tokens and grammar rules. *)
  CCList.(
    ( tokens >|= fun t ->
      let range = Range.of_lexical_positions t.p in
      DocumentSymbol.create ~kind:SymbolKind.Constant ~name:t.v.terminal ~range
        ~selectionRange:range
        ~detail:(CCOption.get_or ~default:"" t.v.alias)
        () )
    @ ( pg_rules >|= fun t ->
        let range = Range.of_lexical_positions t.pr_nt.p in
        DocumentSymbol.create ~kind:SymbolKind.Function ~name:t.pr_nt.v ~range
          ~selectionRange:range () ))

let symbol_at_position (state : state) (pos : Position.t) :
    (Range.t * string located) option =
  L.find_map
    (fun (s : string located) ->
      let rng = Range.of_lexical_positions s.p in
      let res = Position.compare_inclusion pos rng = `Inside in
      if res then Some (rng, s) else None)
    state.symbols

(** Produce hover information at a particular position. For:
    - token aliases, we display their full name;
    - standard library rules, their documentation; *)
let hover (state : state) (pos : Position.t) =
  let open O in
  let* rng, sym = symbol_at_position state pos in
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

let diagnostics (_state : state) : Lsp.Types.Diagnostic.t list =
  (* let open MenhirSyntax.Syntax in
  List.map
    (function
      | { p; v = { terminal; _ } } ->
          Diagnostic.create ~severity:DiagnosticSeverity.Information
            ~message:(`String (Printf.sprintf "%s is a nice token" terminal))
            ~range:(lsp_range_of_menhir_range p)
            ())
    state.tokens *)
  []

(** Lsp server class

    This is the main point of interaction beetween the code checking documents
    (parsing, typing, etc...), and the code of linol.

    The [Linol_lwt.Jsonrpc2.server] class defines a method for each of the
    action that the lsp server receives, such as opening of a document, when a
    document changes, etc.. By default, the method predefined does nothing (or
    errors out ?), so that users only need to override methods that they want
    the server to actually meaningfully interpret and respond to. *)
class lsp_server =
  object (self)
    inherit Linol_lwt.Jsonrpc2.server

    (* one env per document *)
    val buffers : (uri, state) Hashtbl.t = Hashtbl.create 32
    method spawn_query_handler f = Linol_lwt.spawn f

    method! config_completion =
      Some
        {
          allCommitCharacters = None;
          completionItem = None;
          resolveProvider = None;
          triggerCharacters = Some [ "%" ];
          workDoneProgress = None;
        }

    method! on_req_completion =
      fun ~notify_back ~id:_ ~uri ~pos ~ctx:_ ~workDoneToken:_
          ~partialResultToken:_ _doc_state ->
        let open O in
        Lwt.return
        @@
        let+ state = Hashtbl.find_opt buffers uri in
        let comps =
          match completions_for_action pos state with
          | [] ->
              completions state @ standard_lib_completions @ percent_completions
          | l -> l
        in
        notify_back#send_log_msg ~type_:MessageType.Info
          (Printf.sprintf "# completions: %d" (List.length comps))
        |> ignore;
        `List comps

    method! config_symbol = Some (`Bool true)

    method! on_req_symbol =
      fun ~notify_back ~id:_ ~uri ~workDoneToken:_ ~partialResultToken:_
          _unit ->
        Lwt.return
        @@
        let open O in
        let+ state = Hashtbl.find_opt buffers uri in
        let syms = document_symbols state in
        notify_back#send_log_msg ~type_:MessageType.Info
          (Printf.sprintf "# symbols: %d" (List.length syms))
        |> ignore;
        `DocumentSymbol syms

    method! config_definition = Some (`Bool true)

    method! config_modify_capabilities (default : ServerCapabilities.t) =
      {
        default with
        referencesProvider = Some (`Bool true);
        renameProvider =
          Some
            (`RenameOptions
               { prepareProvider = Some true; workDoneProgress = None });
      }

    method! on_request_unhandled : type r.
        notify_back:Linol_lwt.Jsonrpc2.notify_back ->
        id:Linol_jsonrpc.Jsonrpc.Id.t ->
        r Lsp.Client_request.t ->
        r Lwt.t =
      fun ~notify_back ~id t ->
        match t with
        | Lsp.Client_request.TextDocumentPrepareRename
            (r : PrepareRenameParams.t) ->
            self#_on_req_prepare_rename ~notify_back ~id ~uri:r.textDocument.uri
              ~pos:r.position
        | Lsp.Client_request.TextDocumentRename (r : RenameParams.t) ->
            notify_back#send_log_msg ~type_:Info
              (spr "Client requested rename at position: %s"
                 (Position.show r.position))
            |> ignore;
            self#_on_req_rename ~notify_back r.newName ~pos:r.position ~id
              ~uri:r.textDocument.uri
        | Lsp.Client_request.TextDocumentReferences (r : ReferenceParams.t) ->
            notify_back#send_log_msg ~type_:Info
              (spr "Client requested references at position: %s"
                 (Position.show r.position))
            |> ignore;
            self#_on_req_references ~notify_back ~id ~pos:r.position
              ~uri:r.textDocument.uri
        | _ -> Lwt.fail_with "unhandled request type"

    method private _on_req_references =
      fun ~notify_back:_ ~id:_ ~uri ~pos : Location.t list option Lwt.t ->
        Lwt.return
        @@
        let open O in
        let* state = Hashtbl.find_opt buffers uri in
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
             (fun { v; p } ->
               epr "Comparing with %s at %s\n" v
                 Range.(show @@ of_lexical_positions p);
               if_
                 (fun _ -> v = sym_name)
                 (Location.create ~uri ~range:(Range.of_lexical_positions p)))
             state.symbols)

    method private _on_req_prepare_rename =
      fun ~notify_back:_ ~id:_ ~uri ~pos : Range.t option Lwt.t ->
        Lwt.return
        @@
        let open O in
        let* state = Hashtbl.find_opt buffers uri in
        let+ sym_range, _ = symbol_at_position state pos in
        epr "Range for rename is valid: %s\n" (Range.show sym_range);
        sym_range

    method private _on_req_rename =
      fun ~notify_back:_ ~id:_ ~uri ~pos newName : WorkspaceEdit.t Lwt.t ->
        Lwt.return
        @@
        (* Gather all the occurrences of the symbol (terminal or non) *)
        let edits : TextEdit.t list =
          O.(
            let* state = Hashtbl.find_opt buffers uri in
            let* _sym_range, sym = symbol_at_position state pos in
            epr "I will rename %s\n" sym.v;
            some
            @@ L.filter_map
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

    method! on_req_definition =
      fun ~notify_back ~id:_ ~uri ~pos ~workDoneToken:_ ~partialResultToken:_
          _doc_state ->
        Lwt.return
        @@
        let open O in
        let* state = Hashtbl.find_opt buffers uri in
        notify_back#send_log_msg ~type_:MessageType.Info
          (spr "Request definition at pos %s" (Position.show pos))
        |> ignore;
        (* Get the symbol under the cursor, if any. *)
        let* _sym_range, sym = symbol_at_position state pos in
        notify_back#send_log_msg ~type_:MessageType.Info
          (spr "Symbol under cursor: %s %s" sym.v (Range.show _sym_range))
        |> ignore;
        (* Search for the symbol in the terminals or in the nonterminals. *)
        (let+ token =
           L.find_opt
             (fun (t : token located) ->
               String.equal t.v.terminal sym.v || t.v.alias = Some sym.v)
             state.tokens
         in
         `Location
           [ Location.create ~range:(Range.of_lexical_positions token.p) ~uri ])
        <+> let+ nt =
              L.find_map
                (fun (r : M.Syntax.parameterized_rule) ->
                  if String.equal r.pr_nt.v sym.v then Some r.pr_nt else None)
                state.grammar.pg_rules
            in
            `Location
              [ Location.create ~range:(Range.of_lexical_positions nt.p) ~uri ]

    method! config_hover = Some (`Bool true)

    method! on_req_hover =
      fun ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_ _doc_state ->
        let open O in
        Lwt.return
        @@
        let* state = Hashtbl.find_opt buffers uri in
        let* contents, range = hover state pos in
        Some
          (Hover.create
             ~contents:
               (`MarkupContent
                  (MarkupContent.create ~kind:MarkupKind.Markdown
                     ~value:contents))
             ~range ())

    method! config_code_action_provider =
      `CodeActionOptions
        {
          codeActionKinds = Some [ Refactor ];
          resolveProvider = None;
          workDoneProgress = None;
        }

    method! on_req_code_action =
      fun ~notify_back:_ ~id:_ t ->
        Lwt.return
        @@
        let open O in
        let uri = t.textDocument.uri in
        let* state = Hashtbl.find_opt buffers uri in
        let* sym_range, sym = symbol_at_position state t.range.start in
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
                         Linol_jsonrpc.Import.(
                           List.
                             [
                               `String terminal;
                               Range.yojson_of_t sym_range;
                               DocumentUri.yojson_of_t uri;
                               (* just send the ranges and build the edit on the client *)
                               WorkspaceEdit.(
                                 yojson_of_t
                                 @@ create
                                      ~changes:
                                        [
                                          ( uri,
                                            L.filter_map
                                              (fun sym' ->
                                                let range =
                                                  Range.of_lexical_positions
                                                    sym'.p
                                                in
                                                if_
                                                  (fun _ ->
                                                    sym.v = sym'.v
                                                    && Range.compare sym_range
                                                         range
                                                       <> Eq)
                                                  (TextEdit.create
                                                     ~newText:""
                                                       (* will be set by the client *)
                                                     ~range))
                                              state.symbols );
                                        ]
                                      ());
                             ])
                       ());
                ]
            | { v = { terminal; alias = Some alias; _ }; _ }
              when terminal = sym.v ->
                [
                  `CodeAction
                    (CodeAction.create ~kind:Refactor
                       ~title:
                         ("Replace all occurrences of " ^ terminal
                        ^ " with alias")
                       ~edit:
                         (WorkspaceEdit.create
                            ~changes:
                              [
                                ( uri,
                                  L.filter_map
                                    (fun sym ->
                                      let range =
                                        Range.of_lexical_positions sym.p
                                      in
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

    (* We define here a helper method that will:
            - process a document
            - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
    method private _on_doc ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (uri : uri) (contents : string) =
      Printf.eprintf "Processing document %s\n" @@ DocumentUri.to_path uri;
      let%lwt new_state, new_diags =
        match load_state_from_contents (DocumentUri.to_path uri) contents with
        | Ok new_state ->
            Hashtbl.replace buffers uri new_state;
            Lwt.return (Some new_state, [])
        | Error diags ->
            (* Reuse the old state *)
            Lwt.return (Hashtbl.find_opt buffers uri, diags)
      in
      let diags = O.map_or ~default:[] diagnostics new_state in
      notify_back#send_diagnostic (diags @ new_diags)

    (* We now override the [on_notify_doc_did_open] method that will be called
      by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_lwt.t =
      prerr_endline "Opened document";
      self#_on_doc ~notify_back d.uri content

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
      by the server each time a new document is opened. *)
    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old
        ~new_content =
      self#_on_doc ~notify_back d.uri new_content

    (* On document closes, we remove the state associated to the file from the global
      hashtable state, to avoid leaking memory. *)
    method on_notif_doc_did_close ~notify_back:_ d : unit Linol_lwt.t =
      Hashtbl.remove buffers d.uri;
      Linol_lwt.return ()
  end

(* Main code
      This is the code that creates an instance of the lsp server class
      and runs it as a task. *)
let run () =
  let s = new lsp_server in
  let server = Linol_lwt.Jsonrpc2.create_stdio ~env:() s in
  let task =
    prerr_endline "Started LSP server";
    let shutdown () = s#get_status = `ReceivedExit in
    Linol_lwt.Jsonrpc2.run ~shutdown server
  in
  match Linol_lwt.run task with
  | () -> ()
  | exception e ->
      let e = Printexc.to_string e in
      Printf.eprintf "error: %s\n%!" e;
      exit 1

(* Finally, we actually run the server *)
let () =
  Printexc.record_backtrace true;
  (* let module Cli = Linol_lsp.Cli in
  let arg = Cli.Arg.create () in
  let spec = Cli.Arg.spec arg in *)
  run ()
