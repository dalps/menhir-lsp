open Utils
module Mll = Ocamllex
module Mly = Menhir

(* Based on Linol's Lwt template: https://github.com/c-cube/linol/blob/main/example/template-lwt/main.ml *)
class lsp_server =
  object (self)
    inherit Linol_lwt.Jsonrpc2.server

    (* one env per document *)
    val mly_buffers : (uri, Mly.state) Hashtbl.t = Hashtbl.create 32
    val mll_buffers : (uri, Mll.state) Hashtbl.t = Hashtbl.create 32
    method spawn_query_handler f = Linol_lwt.spawn f

    method private _word_at_position :
        notify_back:Linol_lwt.Jsonrpc2.notify_back ->
        pos:Position.t ->
        uri:uri ->
        word option =
      fun ~notify_back ~pos ~uri ->
        let open O in
        let* d = self#find_doc uri in
        let text = d.content in
        let td =
          Text_document.make ~position_encoding:positionEncoding
            (DidOpenTextDocumentParams.create
               ~textDocument:
                 { text; version = d.version; languageId = d.languageId; uri })
        in
        let ofs = Text_document.absolute_position td pos in
        (* Limit the search to the previous 500 chars. *)
        let max_reach = min ofs 500 in
        let prefix = String.sub text (ofs - max_reach) max_reach in
        (* log_info ~notify_back "Search prefix at offset %d: %s, max reach: %d"
          ofs prefix max_reach; *)
        (* The offset of the character right before what we want to complete. *)
        let start_ofs =
          try
            Re.Str.(
              search_backward
                (* This should include all trigger characters. *)
                (regexp {|[^a-zA-Z0-9_$%.]|})
                prefix max_reach)
          with _ ->
            (* log_info ~notify_back "Couldn't find start_ofs, defaulting to -1"; *)
            -1
        in
        let length = max_reach - (start_ofs + 1) in
        let start_pos =
          Position.create ~line:pos.line ~character:(pos.character - length)
        in
        let range = Range.create ~start:start_pos ~end_:pos in
        let word = String.sub prefix (start_ofs + 1) length in
        log_info ~notify_back "Word under cursor: |%s|, range: %s, length: %d"
          word (Range.show range) length;
        Some { v = word; p = range; td }

    method private _dispatch : type r.
        uri ->
        notify_back:Linol_lwt.Jsonrpc2.notify_back ->
        mll_handler:(Mll.state -> r) ->
        mly_handler:(Mly.state -> r) ->
        r option =
      fun uri ~notify_back ~mll_handler ~mly_handler ->
        let filename = DocumentUri.to_path uri in
        let open O in
        match Filename.extension filename with
        | ".mll" -> Hashtbl.find_opt mll_buffers uri >|= mll_handler
        | ".mly" -> Hashtbl.find_opt mly_buffers uri >|= mly_handler
        | ext ->
            log_error ~notify_back "Unhandled document type: %s" ext;
            None

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
          self#_dispatch uri ~notify_back ~mll_handler:Mll.document_symbols
            ~mly_handler:Mly.document_symbols
        in
        notify_back#send_log_msg ~type_:MessageType.Info
          (spr "# symbols: %d" (List.length syms))
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
        selectionRangeProvider = Some (`Bool true);
      }

    method! on_notification_unhandled ~notify_back =
      function
      | DidChangeWatchedFiles params ->
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
        match t with
        | Lsp.Client_request.TextDocumentPrepareRename
            (r : PrepareRenameParams.t) ->
            self#_on_req_prepare_rename ~notify_back ~id ~uri:r.textDocument.uri
              ~pos:r.position
        | Lsp.Client_request.TextDocumentRename (r : RenameParams.t) ->
            log_info ~notify_back "Requested rename at position: %s"
              (Position.show r.position);
            self#_on_req_rename ~notify_back r.newName ~pos:r.position ~id
              ~uri:r.textDocument.uri
        | Lsp.Client_request.TextDocumentReferences (r : ReferenceParams.t) ->
            log_info ~notify_back "Requested references at position: %s"
              (Position.show r.position);
            self#_on_req_references ~notify_back ~id ~pos:r.position
              ~uri:r.textDocument.uri
        | Lsp.Client_request.SelectionRange (r : SelectionRangeParams.t) ->
            log_info ~notify_back "Requested selection range at positions: %s"
              (L.to_string Position.show r.positions);
            self#_on_req_selection_range ~notify_back ~r
        | _ -> Lwt.fail_with "Unhandled request type"

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
        log_info ~notify_back "Requested definition at pos %s"
          (Position.show pos);
        self#_dispatch uri ~notify_back ~mly_handler:(Mly.definition ~uri ~pos)
          ~mll_handler:(Mll.definition ~uri ~pos)
        |> Lwt.return

    method! config_hover = Some (`Bool true)

    method! on_req_hover =
      fun ~notify_back ~id:_ ~uri ~pos ~workDoneToken:_ _doc_state ->
        self#_dispatch uri ~notify_back ~mly_handler:(Mly.hover ~pos)
          ~mll_handler:(fun _ -> None)
        |> O.flatten |> Lwt.return

    method! config_code_action_provider =
      `CodeActionOptions
        {
          codeActionKinds = Some [ Refactor ];
          resolveProvider = None;
          workDoneProgress = None;
        }

    method! on_req_code_action =
      fun ~notify_back ~id:_ { textDocument = { uri }; range; _ } ->
        self#_dispatch uri ~notify_back
          ~mly_handler:(Mly.code_actions ~uri ~range) ~mll_handler:(fun _ ->
            None)
        |> O.flatten |> Lwt.return

    method private _on_req_selection_range ~notify_back ~r :
        SelectionRange.t list Lwt.t =
      self#_dispatch r.textDocument.uri ~notify_back
        ~mll_handler:
          (* We don't support multiple positions yet. *)
          (Mll.selection_range ~notify_back ~uri:r.textDocument.uri
             ~pos:(L.hd r.positions))
        ~mly_handler:(fun _ -> [])
      |> O.to_list |> L.flatten |> Lwt.return

    (* We define here a helper method that will:
            - process a document
            - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
    method private _on_doc ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (uri : uri) (contents : string) : unit Lwt.t =
      let filename = DocumentUri.to_path uri in
      log_info ~notify_back "Processing document %s" filename;
      let go buffers loader diagnose =
        let new_state, new_diags =
          match loader filename contents with
          | Ok new_state ->
              Hashtbl.replace buffers uri new_state;
              (Some new_state, [])
          | Error diags -> (Hashtbl.find_opt buffers uri, diags)
        in
        (* diagnoses for the new state (empty) *)
        let diags = O.map_or ~default:[] diagnose new_state in
        notify_back#send_diagnostic (diags @ new_diags)
      in
      (* consider matching on TextDocumentItem.languageId *)
      match Filename.extension filename with
      | ".mll" -> go mll_buffers Mll.load_state_from_contents Mll.diagnostics
      | ".mly" ->
          go mly_buffers Mly.load_state_from_contents
            (Mly.diagnostics ~notify_back ~uri)
      | ext ->
          notify_back#send_log_msg ~type_:Error
          @@ spr "Unhandled document type: %s" ext

    (* We now override the [on_notify_doc_did_open] method that will be called
            by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_lwt.t =
      get_build_dir () |> ignore;
      find_merlin_config ~notify_back ~uri:d.uri |> ignore;
      log_info ~notify_back "Language id: %s" d.languageId;
      self#_on_doc ~notify_back d.uri content

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
      by the server each time a new document is opened. *)
    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old
        ~new_content =
      self#_on_doc ~notify_back d.uri new_content

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

(* Finally, we actually run the server *)
let () =
  Printexc.record_backtrace true;
  (* let module Cli = Linol_lsp.Cli in
  let arg = Cli.Arg.create () in
  let spec = Cli.Arg.spec arg in *)
  run ()
