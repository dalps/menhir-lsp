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
            notify_back#send_log_msg ~type_:Error
              (spr "Unhandled document type: %s" ext)
            |> ignore;
            None

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
        Lwt.return
        @@
        let open O in
        let+ comps =
          self#_dispatch ~notify_back uri ~mll_handler:(Mll.completions ~pos)
            ~mly_handler:(Mly.completions ~pos)
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
        let+ syms =
          self#_dispatch uri ~notify_back ~mll_handler:Mll.document_symbols
            ~mly_handler:Mly.document_symbols
        in
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
              (spr "Requested rename at position: %s" (Position.show r.position));%lwt
            self#_on_req_rename ~notify_back r.newName ~pos:r.position ~id
              ~uri:r.textDocument.uri
        | Lsp.Client_request.TextDocumentReferences (r : ReferenceParams.t) ->
            notify_back#send_log_msg ~type_:Info
              (spr "Requested references at position: %s"
                 (Position.show r.position));%lwt
            self#_on_req_references ~notify_back ~id ~pos:r.position
              ~uri:r.textDocument.uri
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
        notify_back#send_log_msg ~type_:Info
          (spr "Requested definition at pos %s" (Position.show pos));%lwt
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

    (* We define here a helper method that will:
            - process a document
            - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
    method private _on_doc ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (uri : uri) (contents : string) =
      let filename = DocumentUri.to_path uri in
      notify_back#send_log_msg ~type_:Info
        (spr "Processing document %s" filename);%lwt
      let go buffers loader diagnose =
        let%lwt new_state, new_diags =
          match loader filename contents with
          | Ok new_state ->
              Hashtbl.replace buffers uri new_state;
              Lwt.return (Some new_state, [])
          | Error diags -> Lwt.return (Hashtbl.find_opt buffers uri, diags)
        in
        (* diagnoses for the new state (empty) *)
        let diags = O.map_or ~default:[] diagnose new_state in
        notify_back#send_diagnostic (diags @ new_diags)
      in
      match Filename.extension filename with
      | ".mll" -> go mll_buffers Mll.load_state_from_contents Mll.diagnostics
      | ".mly" -> go mly_buffers Mly.load_state_from_contents Mly.diagnostics
      | ext ->
          notify_back#send_log_msg ~type_:Error
          @@ spr "Unhandled document type: %s" ext

    (* We now override the [on_notify_doc_did_open] method that will be called
          by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_lwt.t =
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
