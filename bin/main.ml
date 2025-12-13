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

module Lsp = Linol.Lsp
open Lsp.Types

type state = MenhirSyntax.Syntax.partial_grammar
(** for now it's just the AST computed by Menhir *)

type uri = Lsp.Types.DocumentUri.t

let process_input_file (file_name : string) (file_contents : string) :
    (state, string) result Lwt.t =
  try%lwt
    Lwt.return
    @@ Ok
         (MenhirSyntax.Main.load_grammar_from_contents 0 file_name file_contents)
  with _ ->
    let msg = "could not parse file" in
    prerr_endline msg;
    Lwt.return @@ Error msg

let completions (state : state) : CompletionItem.t list =
  let open MenhirSyntax.Syntax in
  List.filter_map
    (fun (decl : declaration located) ->
      match decl.v with
      | DToken (_, terminal, Some alias, _) ->
          Some
            (CompletionItem.create ~kind:CompletionItemKind.Constant
               ~label:alias ~detail:terminal ())
      | DToken (_, terminal, None, _) ->
          Some
            (CompletionItem.create ~kind:CompletionItemKind.Constant
               ~label:terminal ())
      | _ -> None)
    state.pg_declarations
  @ List.map
      (fun (rule : parameterized_rule) ->
        CompletionItem.create ~kind:CompletionItemKind.Function
          ~label:rule.pr_nt ())
      state.pg_rules

module MR = MenhirSyntax.Range

let _dummy_pos = Position.create ~character:0 ~line:0

let lsp_pos_of_lexing_pos (p : Lexing.position) =
  Position.create ~character:(p.pos_cnum - p.pos_bol) ~line:(p.pos_lnum - 1)

let lsp_range_of_menhir_range (r : MR.range) =
  Range.create
    ~start:(lsp_pos_of_lexing_pos @@ MR.startp r)
    ~end_:(lsp_pos_of_lexing_pos @@ MR.endp r)

let diagnostics (state : state) : Lsp.Types.Diagnostic.t list =
  let open MenhirSyntax.Syntax in
  List.filter_map
    (fun (d : declaration located) ->
      match d.v with
      | DToken (_, terminal, _, _) ->
          Some
            (Diagnostic.create ~severity:DiagnosticSeverity.Information
               ~message:(`String (Printf.sprintf "%s is a nice token" terminal))
               ~range:(lsp_range_of_menhir_range d.p)
               ())
      | _ -> None)
    state.pg_declarations

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

    method! on_req_completion =
      fun ~notify_back ~id:_ ~uri ~pos:_ ~ctx:_ ~workDoneToken:_
          ~partialResultToken:_ _doc_state ->
        let state = Hashtbl.find buffers uri in
        let comps = completions state in
        Printf.eprintf "# completions: %d" (List.length comps);
        let _ =
          notify_back#send_log_msg ~type_:MessageType.Info
            (Printf.sprintf "# completions: %d" (List.length comps))
        in
        Lwt.return (Some (`List comps))

    (* We define here a helper method that will:
       - process a document
       - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
    method private _on_doc ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (uri : uri) (contents : string) =
      match%lwt process_input_file (DocumentUri.to_path uri) contents with
      | Ok new_state ->
          Hashtbl.replace buffers uri new_state;
          let diags = diagnostics new_state in
          prerr_endline "processing document: diagnostics";
          notify_back#send_diagnostic diags
      | Error _ -> Lwt.return ()

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
