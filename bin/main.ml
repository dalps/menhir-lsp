open Utils
open Menhir_lsp_lib
module Mll = Ocamllex
module Mly = Menhir
module Msg = MenhirMessages
module MF = Menhirformat_lib

type doc_type = Mll | Mly | Messages

(* This spares us having to pattern-match strings and prevents bugs caused by typos.*)
let command_dict =
  [
    ("getAst", `GetAst);
    ("gotoImplementation", `GotoImplementation);
    ("echoErrors", `EchoErrors);
    ("nextMessage", `NextMessage);
    ("nextDummyMessage", `NextDummyMessage);
    ("previousMessage", `PreviousMessage);
    ("previousDummyMessage", `PreviousDummyMessage);
  ]

let doc_type_of_uri uri : doc_type option =
  let filename = Uri.to_path uri in
  match Filename.extension filename with
  | ".mll" -> Some Mll
  | ".mly" -> Some Mly
  | ".messages" -> Some Messages
  | ext ->
      log_error "Unhandled document type: %s" ext;
      None

let formatter_config : MF.Utils.Config.t option ref = ref None

(* Based on Linol's Lwt template: https://github.com/c-cube/linol/blob/main/example/template-lwt/main.ml *)
class lsp_server =
  object (self)
    inherit Linol_lwt.Jsonrpc2.server

    (* one env per document *)
    val msg_buffers : (uri, Msg.state) Hashtbl.t = Hashtbl.create 32
    val mly_buffers : (uri, Mly.state) Hashtbl.t = Hashtbl.create 32
    val mll_buffers : (uri, Mll.state) Hashtbl.t = Hashtbl.create 32
    method spawn_query_handler f = Linol_lwt.spawn f

    method private get_text_document (uri : uri) : Text_document.t option =
      let open O in
      let+ { languageId; version; content = text; _ } = self#find_doc uri in
      TD.create ~position_encoding:positionEncoding ~text ~version ~languageId
        uri
    (** Turns Linol's [find_doc] result into a more useful [Text_document.t] *)

    method private _word_at_position :
        notify_back:Linol_lwt.Jsonrpc2.notify_back ->
        pos:Position.t ->
        uri:uri ->
        word option =
      fun ~notify_back ~pos ~uri ->
        let open O in
        let* td = self#get_text_document uri in
        let text = Text_document.text td in
        let ofs = Text_document.absolute_position td pos in
        let start_ofs, length, word = Utils.find_prefix text ofs in
        let start_pos =
          Position.create ~line:pos.line ~character:(pos.character - length)
        in
        let range = Range.create ~start:start_pos ~end_:pos in
        log_info ~notify_back "Word under cursor: |%s|, range: %a, length: %d"
          word Range.pp range length;
        Some { v = word; p = range; offset = ofs; td }

    method private _dispatch : type r.
        ?msg_handler:(Msg.state -> r) ->
        ?mll_handler:(Mll.state -> r) ->
        ?mly_handler:(Mly.state -> r) ->
        notify_back:Linol_lwt.Jsonrpc2.notify_back ->
        uri ->
        r option =
      fun ?msg_handler ?mll_handler ?mly_handler ~notify_back uri ->
        let open O in
        doc_type_of_uri uri >>= function
        | Mll -> mll_handler <*> Hashtbl.find_opt mll_buffers uri
        | Mly -> mly_handler <*> Hashtbl.find_opt mly_buffers uri
        | Messages -> msg_handler <*> Hashtbl.find_opt msg_buffers uri

    method! config_completion =
      Some
        {
          allCommitCharacters = None;
          completionItem = None;
          resolveProvider = None;
          triggerCharacters = Some [ "%"; "$"; "." ];
          workDoneProgress = None;
        }

    method! on_req_completion =
      fun ~notify_back ~id:_ ~uri ~pos ~ctx:_ ~workDoneToken:_
          ~partialResultToken:_ _doc_state ->
        let open O in
        let word = self#_word_at_position ~notify_back ~uri ~pos in
        let grammar_compls =
          self#_dispatch ~notify_back uri
            ~mll_handler:(Mll.completions ~notify_back ~pos ~uri ~word)
            ~mly_handler:(Mly.completions ~notify_back ~pos ~uri ~word)
          |> get_or ~default:[]
        in
        log_info ~notify_back "# completions: %d" (L.length grammar_compls);
        `List grammar_compls |> some |> Lwt.return

    method! config_symbol = Some (`Bool true)

    method! on_req_symbol =
      fun ~notify_back ~id:_ ~uri ~workDoneToken:_ ~partialResultToken:_
          _unit ->
        Lwt.return
        @@
        let open O in
        let+ syms =
          self#_dispatch uri ~notify_back ~msg_handler:Msg.document_symbols
            ~mll_handler:Mll.document_symbols ~mly_handler:Mly.document_symbols
        in
        log_info ~notify_back "# symbols: %d" (List.length syms);
        `DocumentSymbol syms

    method! config_definition = Some (`Bool true)
    method! config_list_commands = L.map fst command_dict

    method! config_modify_capabilities (default : ServerCapabilities.t) =
      {
        default with
        referencesProvider = Some (`Bool true);
        renameProvider =
          Some
            (`RenameOptions
               { prepareProvider = Some true; workDoneProgress = None });
        selectionRangeProvider = Some (`Bool true);
        documentFormattingProvider = Some (`Bool true);
        foldingRangeProvider = Some (`Bool true);
        diagnosticProvider =
          Some
            (`DiagnosticOptions
               (DiagnosticOptions.create ~interFileDependencies:false
                  ~workspaceDiagnostics:false ()));
      }

    method! on_notification_unhandled ~notify_back =
      function
      | ChangeConfiguration { settings } ->
          (* This case keeps the menhirformat config of the server in sync with the client's.

          Note: according to the LSP spec, this "push" model of configuration synchronization,
          where the client notifies the server every time some setting is changed, is deprecated.

          The server should instead pull the relevant section of the client's config through
          a server request (e.g. inside the [documentFormatting] request handler).
          See example in [on_notif_doc_did_open].
          *)
          Json.Util.(settings |> member "menhir" |> member "format")
          |> MF.Utils.Config.of_yojson
          |> ( function
          | Ok cfg ->
              formatter_config := Some cfg;
              log_info ~notify_back "Updated formatter's settings: %s"
                (Json.to_string settings)
          | Error s ->
              log_error ~notify_back
                "Error while updating formatter's settings %s" s )
          |> Lwt.return
      | DidChangeWatchedFiles params ->
          (* This case syncs the diagnostics with the currently opened grammar's conflicts. *)
          let open R in
          let module P = Stdune.Path in
          let module F = Filename in
          (let+ root, ctx = get_build_dir () in
           L.filter_map
             (fun FileEvent.{ uri; _ } ->
               let s_path = Uri.to_path uri in
               log_info ~notify_back "Watched file changed: %s" s_path;
               let p_path = P.of_string s_path in
               let p_build = P.of_string F.(concat root ctx) in
               let open O in
               let* p_source = P.drop_prefix p_path ~prefix:p_build in
               let uri =
                 Uri.of_path
                   F.(
                     concat root
                       ((P.Local.to_string p_source |> remove_extension)
                       ^ ".mly"))
               in
               log_info ~notify_back "Reconstructed source path: %s"
                 (Uri.to_path uri);
               let+ state = Hashtbl.find_opt mly_buffers uri in
               let diags = Mly.diagnostics ~notify_back ~uri state in
               log_info ~notify_back "# conflicts: %d" (L.length diags);
               notify_back#set_uri uri;
               notify_back#send_diagnostic diags)
             params.changes)
          |> ignore;
          Lwt.return ()
      | _ -> Lwt.return ()

    method! on_request_unhandled : type r.
        notify_back:Linol_lwt.Jsonrpc2.notify_back ->
        id:Linol_jsonrpc.Jsonrpc.Id.t ->
        r Lsp.Client_request.t ->
        r Lwt.t =
      fun ~notify_back ~id t ->
        set_notify_back notify_back;
        let module C = Lsp.Client_request in
        match t with
        | C.TextDocumentDiagnostic (r : DocumentDiagnosticParams.t) ->
            self#_on_req_document_diagnostic ~notify_back r.textDocument.uri
        | C.TextDocumentPrepareRename (r : PrepareRenameParams.t) ->
            self#_on_req_prepare_rename ~notify_back ~id ~uri:r.textDocument.uri
              ~pos:r.position
        | C.TextDocumentFoldingRange (r : FoldingRangeParams.t) ->
            self#_on_req_folding_range ~notify_back r.textDocument.uri
        | C.TextDocumentRename (r : RenameParams.t) ->
            log "Requested rename at position: %a" Position.pp r.position;
            self#_on_req_rename ~notify_back r.newName ~pos:r.position ~id
              ~uri:r.textDocument.uri
        | C.TextDocumentReferences (r : ReferenceParams.t) ->
            log "Requested references at position: %a" Position.pp r.position;
            self#_on_req_references ~notify_back ~id ~pos:r.position
              ~uri:r.textDocument.uri
        | C.SelectionRange (r : SelectionRangeParams.t) ->
            log "Requested selection range at positions: %a"
              (pp_list Position.pp) r.positions;
            self#_on_req_selection_range ~notify_back ~r
        | C.TextDocumentFormatting (r : DocumentFormattingParams.t) ->
            log "Requested document formatting";
            self#_on_req_document_formatting ~notify_back ~r
        | _ -> Lwt.fail_with "Unhandled request type"

    method! on_req_execute_command ~notify_back ~id:_ ~workDoneToken:_
        (command : string) (args : Yojson.Safe.t list option) : Json.t Lwt.t =
      set_notify_back notify_back;
      let open O in
      let showDoc (uri, selection) =
        notify_back#send_request
          (ShowDocumentRequest
             (ShowDocumentParams.create ~uri ?selection ~takeFocus:true ()))
          (function
            | Ok success -> Lwt.return ()
            | Error { code; message; data } ->
                notify_back#send_notification
                  (ShowMessage { message; type_ = Error }))
        |> ignore;
        None
      in
      (* Some commands also take a position, which we always pass as the second argument. *)
      let pos_of_args args =
        match args with
        | Some [ _; pos ] -> Some (Position.t_of_yojson pos)
        | _ ->
            log "Failed to read uri argument";
            None
      in
      log "Command %s invoked with args: %a" command
        (pp_option @@ pp_list Json.pp)
        args;
      Lwt.return @@ O.get_or ~default:`Null
      @@
      (* All the commands we define carry a uri as the first argument, which we extract right away and exit early if not found. *)
      let* uri =
        match args with
        | Some (`String uri :: _) -> Some (Uri.of_string uri)
        | _ ->
            log "Failed to read uri argument";
            None
      in
      (* Helper for handling .messages commands *)
      let focus f =
        let* pos = pos_of_args args in
        let* state = Hashtbl.find_opt msg_buffers uri in
        let selection = f state ~pos in
        log "Next selection: %a" (pp_option Range.pp) selection;
        showDoc (uri, selection)
      in
      match L.assoc ~eq:String.equal command command_dict with
      | `GetAst ->
          self#_dispatch uri ~notify_back
            ~mly_handler:(fun state -> Mly.yojson_of_ast state.grammar)
            ~mll_handler:(fun state -> Mll.yojson_of_ast state.grammar)
      | `GotoImplementation ->
          let pos = pos_of_args args in
          log "Client requested implementation of %a at position %a" pp_uri uri
            (pp_option Position.pp) pos;
          self#_dispatch uri ~notify_back
            ~mly_handler:(Mly.show_impl ?pos >=> showDoc)
            ~mll_handler:(Mll.show_impl ?pos >=> showDoc)
          |> O.flatten
      | `EchoErrors ->
          let+ state = Hashtbl.find_opt msg_buffers uri in
          let stats = Msg.stats state in
          `String stats
      | `NextMessage -> focus Msg.next_message
      | `NextDummyMessage -> focus Msg.next_dummy_message
      | `PreviousMessage -> focus Msg.previous_message
      | `PreviousDummyMessage -> focus Msg.previous_dummy_message
      | exception _ -> None

    method private _on_req_folding_range ~(notify_back : notify_back)
        (uri : uri) : FoldingRange.t list option Lwt.t =
      Lwt.return
      @@
      let open O in
      let* doc = self#get_text_document uri in
      self#_dispatch ~notify_back ~msg_handler:(Msg.folding_ranges ~doc) uri

    method private _on_req_document_formatting ~notify_back
        ~r:
          ({ textDocument = { uri; _ }; options; _ } :
            DocumentFormattingParams.t) : TextEdit.t list option Lwt.t =
      (* The AST is provided by the handler's state parameter. *)
      Lwt.return
      @@
      let open O in
      let* doc = self#get_text_document uri in
      let config =
        let open MF.Utils.Config in
        let cfg = O.get_or ~default:default_config !formatter_config in
        { cfg with tabsize = options.tabSize }
      in
      let filename = doc |> Text_document.documentUri |> Uri.to_path in
      let go _ ~format =
        match format ~config (Text_document.text doc) with
        | Ok newText ->
            [ TextEdit.create ~newText ~range:Range.(whole_document doc) ]
        | Error (msg, range) ->
            let message =
              spr "menhirformat: ignoring \"%s\" (syntax error) %s %s" filename
                (OcamllexSyntax.Range.show range)
                msg
            in
            notify_back#send_log_msg ~type_:Warning message |> ignore;
            notify_back#send_notification
              (ShowMessage { message; type_ = Warning })
            |> ignore;
            (* todo: maybe returning None in this case is more semantically correct. Check the spec. *)
            []
      in
      self#_dispatch uri ~notify_back
        ~mll_handler:(go ~format:MF.Ocamllex.format_string)
        ~mly_handler:(go ~format:MF.Menhir.format_string)

    method private _on_req_references =
      fun ~notify_back ~id:_ ~uri ~pos : Location.t list option Lwt.t ->
        self#_dispatch uri ~notify_back ~mly_handler:(Mly.references ~uri ~pos)
          ~mll_handler:(Mll.references ~uri ~pos)
        |> Lwt.return

    method private _on_req_prepare_rename =
      fun ~notify_back ~id:_ ~uri ~pos : Range.t option Lwt.t ->
        self#_dispatch uri ~notify_back ~mly_handler:(Mly.prepare_rename ~pos)
          ~mll_handler:(Mll.prepare_rename ~pos)
        |> O.flatten |> Lwt.return

    method private _on_req_rename =
      fun ~notify_back ~id:_ ~uri ~pos newName : WorkspaceEdit.t Lwt.t ->
        self#_dispatch uri ~notify_back
          ~mly_handler:(Mly.rename ~uri ~pos ~newName)
          ~mll_handler:(Mll.rename ~uri ~pos ~newName)
        |> O.get_or ~default:(WorkspaceEdit.create ())
        |> Lwt.return

    method! on_req_definition =
      fun ~notify_back ~id:_ ~uri ~pos ~workDoneToken:_ ~partialResultToken:_
          _doc_state ->
        Lwt.return
        @@
        let open O in
        let* doc = self#get_text_document uri in
        log_info ~notify_back "Requested definition at pos %s"
          (Position.show pos);
        self#_dispatch uri ~notify_back
          ~mly_handler:(Mly.definition ~notify_back ~doc ~pos)
          ~mll_handler:(Mll.definition ~notify_back ~doc ~pos)

    method! config_hover = Some (`Bool true)

    method! on_req_hover =
      fun ~notify_back ~id:_ ~uri ~pos ~workDoneToken:_ _doc_state ->
        let open O in
        set_notify_back notify_back;
        Lwt.return
        @@
        let* doc = self#get_text_document uri in
        self#_dispatch uri ~notify_back ~mly_handler:(Mly.hover ~doc ~pos)
          ~mll_handler:(Mll.hover ~doc ~pos)
        |> O.flatten

    method! config_code_action_provider =
      `CodeActionOptions
        {
          codeActionKinds = Some [ Refactor; RefactorExtract ];
          resolveProvider = None;
          workDoneProgress = None;
        }

    method! on_req_code_action =
      fun ~notify_back ~id:_ { textDocument = { uri }; range; _ } ->
        self#_dispatch uri ~notify_back
          ~mly_handler:(Mly.code_actions ~uri ~range) ~mll_handler:(fun state ->
            let open O in
            let* doc = self#get_text_document uri in
            Mll.code_actions ~doc ~range ~notify_back state)
        |> O.flatten |> Lwt.return

    method private _on_req_selection_range ~notify_back ~r :
        SelectionRange.t list Lwt.t =
      let SelectionRangeParams.{ positions; _ } = r in
      self#_dispatch r.textDocument.uri ~notify_back
        ~mll_handler:(Mll.selection_range ~notify_back ~positions)
        ~mly_handler:(Mly.selection_range ~notify_back ~positions)
      |> O.to_list |> L.flatten |> Lwt.return

    method private _on_req_document_diagnostic ~notify_back (uri : uri) :
        DocumentDiagnosticReport.t Lwt.t =
      let%lwt items =
        match self#get_text_document uri with
        | None -> Lwt.return_nil
        | Some doc ->
            let contents = TD.text doc in
            self#_on_doc ~notify_back uri contents
      in
      Lwt.return
        (`RelatedFullDocumentDiagnosticReport
           (RelatedFullDocumentDiagnosticReport.create ~items ()))

    (* We define here a helper method that will:
            - process a document
            - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
    method private _on_doc ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (uri : uri) (contents : string) : Diagnostic.t list Lwt.t =
      let log s = log_src "_on_doc" s in
      log "Processing file %a" pp_uri uri;
      set_notify_back notify_back;

      (* Update or register for the first time the state of a server buffer if [contents] is valid, otherwise notify any problem found within [contents] through diagnostics. *)
      let go buffers loader diagnose =
        match loader uri contents with
        | Ok new_state ->
            Hashtbl.replace buffers uri new_state;
            (* Syntax is OK, compute additional diagnostics regarding the semantics of the document. *)
            diagnose new_state
        | Error syntax_errors -> syntax_errors
      in

      let open O in
      let diags =
        doc_type_of_uri uri >|= function
        | Mll ->
            go mll_buffers Mll.load_state_from_contents
              (Mll.diagnostics ~notify_back ~uri)
        | Mly ->
            go mly_buffers Mly.load_state_from_contents
              (Mly.diagnostics ~notify_back ~uri)
        | Messages ->
            go msg_buffers Msg.load_state_from_contents (fun state -> [])
      in
      let diags = O.get_or_nil diags in
      notify_back#send_diagnostic diags |> ignore;
      Lwt.return diags

    (* We now override the [on_notify_doc_did_open] method that will be called
            by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_lwt.t =
      get_build_dir () |> ignore;
      find_merlin_config ~notify_back ~uri:d.uri |> ignore;
      log_info ~notify_back "Language id: %s" d.languageId;
      let%lwt _ =
        let error _ =
          log_error ~notify_back "Error retrieving formatter settings";
          Lwt.return ()
        in
        notify_back#send_request
          (WorkspaceConfiguration
             (ConfigurationParams.create
                ~items:[ ConfigurationItem.create ~section:"menhir.format" () ]))
          (function
            | Ok (j :: _) -> (
                log_info ~notify_back "Found formatter settings: %s "
                  (Json.to_string j);
                match Menhirformat_lib.Utils.Config.of_yojson j with
                | Ok cfg ->
                    formatter_config := Some cfg;
                    Lwt.return ()
                | Error _ -> error ())
            | _ -> error ())
      in
      let _ = self#_on_doc ~notify_back d.uri content in
      Lwt.return ()

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
      by the server each time a new document is opened. *)
    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old
        ~new_content =
      let _ = self#_on_doc ~notify_back d.uri new_content in
      Lwt.return ()

    (* On document closes, we remove the state associated to the file from the global
      hashtable state, to avoid leaking memory. *)
    method on_notif_doc_did_close ~notify_back:_ d : unit Linol_lwt.t =
      Hashtbl.remove mly_buffers d.uri;
      Hashtbl.remove mll_buffers d.uri;
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

let () =
  let version =
    let open Build_info in
    match V1.version () with None -> "dev" | Some v -> V1.Version.to_string v
  in
  let stdio = ref false in
  let print_version () =
    prerr_endline version;
    exit 0
  in
  Arg.parse
    [
      ("--version", Unit print_version, "Print the version of menhir-lsp");
      ("--stdio", Set stdio, "(the server only communicates on stdio for now)");
    ]
    (fun _ -> ())
    {|A language server for Ocamllex and Menhir.

Usage: menhir-lsp|};
  Printexc.record_backtrace true;
  (* let module Cli = Linol_lsp.Cli in
  let arg = Cli.Arg.create () in
  let spec = Cli.Arg.spec arg in *)
  run ()
