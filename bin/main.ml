(* This file is free software, part of linol. See file "LICENSE" for more information *)

(* Some user code

   The code here is just a placeholder to make this file compile, it is expected
   that users have an implementation of a processing function for input contents.

   Here we expect a few things:
   - a type to represent a state/environment that results from processing an
     input file
   - a function processing an input file (given the file contents as a string),
     which return a state/environment
   - a function to extract a list of diagnostics from a state/environment.
     Diagnostics includes all the warnings, errors and messages that the processing
     of a document are expected to be able to return.
*)

open Types
open Linol_lsp.Types
open Utils
open Loc

let process_input_file (file_name : string) (file_contents : string) :
    (state, Diagnostic.t list) result Lwt.t =
  try%lwt
    let grammar = M.Main.load_grammar_from_contents 0 file_name file_contents in
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
    Lwt.return @@ Ok { grammar; tokens }
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
    Lwt.return @@ Error diags

let completions ({ tokens; grammar } : state) : CompletionItem.t list =
  let open MenhirSyntax.Syntax in
  CCList.flat_map
    (let label_details typ =
       CompletionItemLabelDetails.create
         ~detail:
           (match typ with
           | None -> ""
           | Some t -> (
               ": " ^ match t with Declared { v; _ } | Inferred v -> v))
     in
     function
     | { v = { ocamltype = typ; terminal; alias = Some alias; _ }; _ } ->
         [
           CompletionItem.create ~kind:CompletionItemKind.Constant ~label:alias
             ~detail:terminal
             ~labelDetails:(label_details typ ~description:terminal ())
             ();
           CompletionItem.create ~kind:CompletionItemKind.Constant
             ~label:terminal ();
         ]
     | { v = { ocamltype = typ; terminal; alias = None; _ }; _ } ->
         [
           CompletionItem.create ~kind:CompletionItemKind.Constant
             ~labelDetails:(label_details typ ()) ~label:terminal ();
         ])
    tokens
  @ List.map
      (fun (rule : parameterized_rule) ->
        CompletionItem.create ~kind:CompletionItemKind.Function
          ~label:rule.pr_nt.v
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
          ())
      grammar.pg_rules

let document_symbols ({ grammar = { pg_rules; _ }; tokens } : state) :
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
          triggerCharacters = None;
          workDoneProgress = None;
        }

    method! on_req_completion =
      fun ~notify_back ~id:_ ~uri ~pos:_ ~ctx:_ ~workDoneToken:_
          ~partialResultToken:_ _doc_state ->
        let state = Hashtbl.find buffers uri in
        let comps = completions state in
        notify_back#send_log_msg ~type_:MessageType.Info
          (Printf.sprintf "# completions: %d" (List.length comps))
        |> ignore;
        Lwt.return (Some (`List comps))

    method! config_symbol = Some (`Bool true)

    method! on_req_symbol =
      fun ~notify_back ~id:_ ~uri ~workDoneToken:_ ~partialResultToken:_
          _unit ->
        let state = Hashtbl.find buffers uri in
        let syms = document_symbols state in
        notify_back#send_log_msg ~type_:MessageType.Info
          (Printf.sprintf "# symbols: %d" (List.length syms))
        |> ignore;
        Lwt.return (Some (`DocumentSymbol syms))

    method! config_definition = Some (`Bool true)

    method! on_req_definition =
      fun ~notify_back ~id:_ ~uri ~pos ~workDoneToken:_ ~partialResultToken:_
          _doc_state ->
        let state = Hashtbl.find buffers uri in
        let syms = Symbol_table.process_symbols state.grammar in
        notify_back#send_log_msg ~type_:MessageType.Info
          (spr "Request definition at pos %s" (Position.show pos))
        |> ignore;
        Lwt.return
        @@
        let open CCOption in
        (* Get the symbol under the cursor, if any. *)
        let* _sym_range, sym =
          L.find_map
            (fun (s : string located) ->
              let rng = Range.of_lexical_positions s.p in
              let res = Position.compare_inclusion pos rng = `Inside in
              if res then Some (rng, s) else None)
            syms
        in
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

    (* We define here a helper method that will:
            - process a document
            - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
    method private _on_doc ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (uri : uri) (contents : string) =
      Printf.eprintf "Processing %s for diagnostics.\n"
      @@ DocumentUri.to_path uri;
      let%lwt new_state, diags' =
        match%lwt process_input_file (DocumentUri.to_path uri) contents with
        | Ok new_state ->
            Hashtbl.replace buffers uri new_state;
            Lwt.return (new_state, [])
        | Error diags -> Lwt.return (Hashtbl.find buffers uri, diags)
        (* reuse the old state *)
      in
      let diags = diagnostics new_state in
      notify_back#send_diagnostic (diags @ diags')

    (* We now override the [on_notify_doc_did_open] method that will be called
      by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_lwt.t =
      prerr_endline "opened document";
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
    (* don't print to stdout, it's already being used by the protocol, duh -_- *)
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
