open Menhir_lsp_lib
module M = MenhirSyntax
module MR = MenhirSyntax.Range
module Json = Yojson.Safe
include Menhir_lsp_lib.Utils
module Ivl_map = Interval_map.Make (Int)
module Ivl = Ivl_map.Interval

type document = Text_document.t

let server_name = "menhir-lsp"

type uri = Lsp.Types.DocumentUri.t
type notify_back = Linol_lwt.Jsonrpc2.notify_back
type word = { v : string; p : Range.t; offset : int; td : Text_document.t }

let notify_back_ref : notify_back option ref = ref None
let guard flag f s = if flag then f s
let guard' flag f s = if%lwt flag then f s

(** [log] prints to the first available output channel. It will use caller's
    [notify_back] argument if provided, falling back to the optional value
    stored in the global variable [notify_back_ref] and ultimately default to
    [prerr_endline]. *)
let log ?(debug = true) ?(notify_back : notify_back option)
    ?(kind = MessageType.Info) s =
  match (notify_back, !notify_back_ref) with
  | None, None -> Format.kasprintf (guard debug prerr_endline) s
  | None, Some notify_back | Some notify_back, _ ->
      Format.kasprintf
        (guard debug @@ (notify_back#send_log_msg ~type_:kind >> ignore))
        s

(** Identical to [log] but returns a unit promise. *)
let log' ?(debug = true) ?(notify_back : notify_back option)
    ?(kind = MessageType.Info) s =
  match (notify_back, !notify_back_ref) with
  | None, None -> Format.kasprintf (guard debug prerr_endline >> Lwt.return) s
  | None, Some notify_back | Some notify_back, _ ->
      Format.kasprintf
        (guard' (Lwt.return debug) (notify_back#send_log_msg ~type_:kind))
        s

(** Logging helper that allows to specify a message source that will be
    prepended to every log message.

    Override with a concrete [src] argument like this:
    [let log s = log_src "my_source" s in ..].

    Set the [debug] flag to false to mute all messages from this source. *)
let log_src ?(debug = true) ?notify_back ?kind src s =
  Format.kasprintf (log ~debug ?notify_back ?kind "[%s] %s" src) s

let log_info = log ~kind:Info
let log_error = log ~kind:Error
let log_info' = log' ~kind:Info
let log_error' = log' ~kind:Error

let compile_completions ?(range : Range.t option) ~(kind : CompletionItemKind.t)
    :
    (string * string option * string option * string list) list ->
    CompletionItem.t list =
  L.map (fun (label, detail, snippet, docs) ->
      CompletionItem.create ~kind ~label ~filterText:label
        ?labelDetails:
          (O.map
             (fun detail -> CompletionItemLabelDetails.create ~detail ())
             detail)
        ?textEdit:
          O.(
            let+ r = range in
            `TextEdit TextEdit.{ newText = label; range = r })
          (* note: snippets aren't allowed in text edit mode :( *)
        ?insertText:snippet
        ?insertTextFormat:(O.map (fun _ -> InsertTextFormat.Snippet) snippet)
        ?documentation:
          (match docs with
          | [] -> None
          | _ ->
              Some
                (`MarkupContent
                   (MarkupContent.create ~kind:Markdown
                      ~value:(String.concat "\n\n" docs))))
        ())

let _build_dir = ref (Error "")

let get_build_dir _ =
  let f () =
    let err = Error "Failure: dune describe" in
    try
      let inp = Unix.open_process_in "dune describe workspace" in
      let s = CCSexp.parse_chan inp in
      In_channel.close inp;
      match s with
      | Ok
          (`List
             (`List [ `Atom "root"; `Atom root_dir ]
             :: `List [ `Atom "build_context"; `Atom context ]
             :: _)) ->
          _build_dir := Ok (root_dir, context);
          !_build_dir
      | _ -> err
    with _ ->
      (* May fail due to 'A running dune (pid: ..) instance has locked the build directory.') *)
      err
  in
  match !_build_dir with Ok _ -> !_build_dir | Error _ -> f ()

(** e.g. if [uri] is

    [/home/foo/menhir-lsp/test/calc.mly]

    then [fetch_build_dir ~ext:".conflicts" uri] is

    [/home/foo/menhir-lsp/_build/default/test/calc.conflicts] *)
let fetch_build_dir ?(ext : string option) uri =
  let module P = Stdune.Path in
  let module F = Filename in
  let s_path = DocumentUri.to_path uri in
  let s_name = F.basename s_path in
  let s_slug = F.remove_extension s_name in
  let s_ext = O.get_or ~default:(F.extension s_name) ext in
  let open R in
  let* root, ctx = get_build_dir () in
  let p_root = P.of_string root in
  let p_dir = P.of_string (F.dirname s_path) in
  let error =
    Error
      (spr
         "No implementation found for %s. Make sure your module is declared \
          included in the library's dune file. e.g. (menhir (modules .. %s \
          ..)) or (ocamllex .. %s ..) "
         s_name s_slug s_slug)
  in
  match P.drop_prefix p_dir ~prefix:p_root with
  | None -> error
  | Some p_rel ->
      let p_ctx = P.of_string (F.concat root ctx) in
      let p_res = P.append_local p_ctx p_rel in
      let res = F.concat (P.to_string p_res) (s_slug ^ s_ext) in
      if Sys.file_exists res then Ok res else error

module MK = Merlin_kernel
module QP = Query_protocol

let merlin_configs : (uri, MK.Mconfig.t) Hashtbl.t = Hashtbl.create 42

let get_merlin_config (uri : uri) =
  let path = DocumentUri.to_path uri in
  let dir = Filename.dirname path in
  let open O in
  let+ ctx, config_path = MK.Mconfig_dot.find_project_context dir in
  let dot, failures = MK.Mconfig_dot.get_config ctx path in
  log_info {|Search result for Merlin config of %s:
  Errors: %a|} path
    (pp_list pp_string) failures;
  let merlin =
    MK.Mconfig.merge_merlin_config dot MK.Mconfig.initial.merlin ~failures
      ~config_path
  in
  MK.Mconfig.normalize { MK.Mconfig.initial with merlin }

let find_merlin_config ~notify_back ~uri =
  let open O in
  match Hashtbl.find_opt merlin_configs uri with
  | None ->
      log_info ~notify_back
        "couldn't find merlin config for %s, generating new one"
        (DocumentUri.to_path uri);
      let+ config = get_merlin_config uri in
      Hashtbl.add merlin_configs uri config;
      config
  | o -> o

(** https://github.com/ocaml/ocaml-lsp/blob/master/ocaml-lsp-server/src/compl.ml
*)
let completion_kind kind : CompletionItemKind.t option =
  match kind with
  | `Value -> Some Value
  | `Variant -> Some EnumMember
  | `Label -> Some Field
  | `Module -> Some Module
  | `Modtype -> Some Interface
  | `MethodCall -> Some Method
  | `Keyword -> Some Keyword
  | `Constructor -> Some Constructor
  | `Type -> Some TypeParameter

(* Consider switching return type from options to more descriptive results *)
let with_merlin ~(doc : Text_document.t)
    (f : MK.Msource.t -> MK.Mpipeline.t -> 'a) =
  let open O in
  let+ config = get_merlin_config (TD.documentUri doc) in
  let source = MK.Msource.make (TD.text doc) in
  let pipeline = MK.Mpipeline.make config source in
  MK.Mpipeline.with_pipeline pipeline (fun () -> f source pipeline)

let get_merlin_completions ~(uri : uri) ~(pos : Position.t) (prefix : word)
    source pipeline =
  let open O in
  let logical_pos = Position.logical pos in
  let query =
    Query_protocol.Complete_prefix (prefix.v, logical_pos, [], false, true)
  in
  let compls = Query_commands.dispatch pipeline query in
  let sortText_of_index idx = Printf.sprintf "%04d" idx in
  (* Merlin wants the completion prefix to include the fully qualified module path,
      but the TextEdit range must not extend before the cursor position, otherwise the completion won't show.
      Reference: https://github.com/ocaml/ocaml-lsp/blob/master/ocaml-lsp-server/src/compl.ml *)
  let range =
    (let+ prefix = String.split_on_char '.' prefix.v |> L.last_opt in
     let len = String.length prefix in
     let character = pos.character - len in
     let start = { pos with character } in
     { Range.start; end_ = pos })
    |> get_or ~default:prefix.p
  in
  let compls =
    L.mapi
      (fun idx QP.Compl.{ name; kind; desc; _ } ->
        CompletionItem.create ~label:name ?kind:(completion_kind kind)
          ~sortText:(sortText_of_index idx) ~detail:desc
          ~textEdit:(`TextEdit { newText = name; range })
          ())
      compls.entries
  in
  log_info "# merlin completions: %d" (L.length compls);
  compls

let get_merlin_docs ~expression ~(doc : Text_document.t) ~(pos : Position.t) =
  let logical_pos = Position.logical pos in
  with_merlin ~doc (fun _source pipeline ->
      let query = Query_protocol.Document (expression, logical_pos) in
      let typ = Query_commands.dispatch pipeline query in
      match typ with
      | `Found s | `Builtin s -> s
      | `No_documentation | `Invalid_context | `Not_found _ | `Not_in_env _
      | `File_not_found _ ->
          "")

let odoc_to_md text =
  match Doc_to_md.translate text with
  | Doc_to_md.Raw d | Doc_to_md.Markdown d -> d

let make_md_hover ?range sections =
  let concat_sections = String.concat "\n***\n" in
  Hover.create
    ~contents:
      (`MarkupContent
         (MarkupContent.create ~kind:Markdown ~value:(concat_sections sections)))
    ?range ()

let get_merlin_type ~(doc : Text_document.t) ~(pos : Position.t) expression =
  let open O in
  let logical_pos = Position.logical pos in
  let expr =
    with_merlin ~doc (fun _source pipeline ->
        let query = Query_protocol.Type_expr (expression, logical_pos) in
        let typ = Query_commands.dispatch pipeline query in
        typ)
  in
  let _search () =
    with_merlin ~doc (fun _source pipeline ->
        let query =
          Query_protocol.Type_search (expression, logical_pos, 0, true)
        in
        let typs = Query_commands.dispatch pipeline query in
        spr "%a"
          (Format.pp_print_list (fun out typ ->
               pf out "%s, %s" typ.Query_protocol.typ typ.Query_protocol.name))
          typs)
  in
  let enclosing =
    with_merlin ~doc (fun _source pipeline ->
        let query = Query_protocol.Type_enclosing (None, logical_pos, Some 0) in
        let typs = Query_commands.dispatch pipeline query in
        spr "%a"
          (Format.pp_print_list (fun out typ ->
               pf out "%s"
               @@ match typ with _, `Index _, _ -> "" | _, `String s, _ -> s))
          typs)
  in
  let _locate () =
    with_merlin ~doc (fun _source pipeline ->
        let query = Query_protocol.Locate_type logical_pos in
        let typ = Query_commands.dispatch pipeline query in
        match typ with
        | `Builtin t | `Found (Some t, _) -> t
        | `Invalid_context | `Not_found _ | `At_origin | `Not_in_env _
        | `File_not_found _
        | `Found (None, _) ->
            "")
  in
  log "Type_expr: %a" pp_stropt expr;
  log "Type_enclosing: %a" pp_stropt enclosing;
  expr <+> enclosing

let get_merlin_compls ~uri ~pos word =
  with_merlin ~doc:word.td (get_merlin_completions ~uri ~pos word)

let parse_ocaml_impl s =
  let lexbuf = Lexing.from_string s in
  Ppxlib.Parse.implementation lexbuf

let parse_ocaml_type s =
  let lexbuf = Lexing.from_string s in
  Ppxlib.Parse.core_type lexbuf

let add_range ~parent_ref range =
  let add () =
    parent_ref := O.some @@ SelectionRange.create ?parent:!parent_ref ~range ()
  in
  (* Add [range] only if it preserves the invariant. *)
  match !parent_ref with
  | None -> add ()
  | Some p when Range.contains p.range range -> add ()
  | Some _ -> epr "Skipping bad selection range: %a." Range.pp range

let query_position (intervals : 'zone Ivl_map.t) offset : 'zone option =
  let open O in
  let query = Ivl.create (Included offset) (Included offset) in
  let res = Ivl_map.query_interval ~order:Desc query intervals in
  let* (ivl, zones), gen = Ivl_map.Gen.next res in
  let+ innermost_zone = L.head_opt zones in
  innermost_zone

let pp_interval out ({ low; high } : Ivl.t) =
  let open Ivl_map.Bound in
  let pp_low out = function
    | Included v -> pf out "[%d" v
    | Excluded v -> pf out "(%d" v
    | Unbounded -> pf out "(∞"
  in
  let pp_high out = function
    | Included v -> pf out "%d]" v
    | Excluded v -> pf out "%d)" v
    | Unbounded -> pf out "∞)"
  in
  pf out "%a, %a" pp_low low pp_high high

let show_interval = spr "%a" pp_interval

let pp_uri_debug out (uri : uri) =
  pf out "%s{hash = %d; query = %a}" (Uri.to_path uri) (Uri.hash uri)
    (pp_option pp_string) (Uri.query uri)

let pp_uri out (uri : uri) = pf out "%s" (Uri.to_path uri)
let pp_short_uri = pp_uri
let pp_short_state = fun out _ -> pf out "<state>"

let rec pp_selection_range (out : Format.formatter) (sr : SelectionRange.t) =
  pf out "%a%a" Range.pp sr.range
    (Format.pp_print_option (fun out p -> pf out " --> %a" pp_selection_range p))
    sr.parent

let get_ocaml_impl = fetch_build_dir ~ext:".ml"
let get_ocaml_intf = fetch_build_dir ~ext:".mli"

let read_file_contents filename =
  let ic = open_in filename in
  let contents = really_input_string ic (in_channel_length ic) in
  close_in ic;
  contents

let get_source_map uri =
  let log s = log_src "get_source_map" s in
  match get_ocaml_impl uri with
  | Ok path ->
      let text = read_file_contents path in
      let doc = TD.create ~text (Uri.of_path path) in
      let sourcemap =
        text |> Lexing.from_string
        |> Menhir_lsp_lib.Line_directives.read_line_directives []
      in
      (Some doc, sourcemap)
  | Error msg ->
      log "No source map for %a: %s" pp_short_uri uri msg;
      (None, [])

(** Maps a location in a lexer / parser's source code to a symbol in the
    corresponding implementation generated by either Menhir or ocamllex.

    A symbol [v] at line [l] and column [c] in the source code is mapped to a
    symbol [v' = v] at line [m < m'] and column [d] of the implementation code
    that is characterized by a section with the following format:

    {v
# l "path/to/source.(mly|mll)"
                             v'
\_______ c columns _________/
# m' "path/to/source.ml"
    v} *)
let lookup_source (sourcemap : Line_directives.source_mapping list)
    ((startp, endp) as src_pos : lexing_range) (src_text : string) :
    lexing_range option =
  let open Line_directives in
  let open L in
  let log s = log_src "lookup_source" s in
  let start_col = startp.pos_cnum - endp.pos_bol in
  let end_col = endp.pos_cnum - endp.pos_bol in
  let@*? i, m = sourcemap in
  (* log "Comparing %a with mapping #%d %a " Range.pp_lexing src_pos i
    pp_source_mapping m; *)
  (* The text in the implementation has the same number of lines as the original action source text, plus a blank line we omit. *)
  let lines = String.split_on_char '\n' m.text in
  let lines = L.(take (length lines - 1) lines) in
  (* Scan the lines of this implementation section for the symbol we're looking for *)
  let@*? j, line = lines in
  let open O in
  let* _ = guard (m.src_line + j = startp.pos_lnum) in
  try
    let sub = String.sub line start_col (String.length src_text) in
    let* _ = guard (String.equal sub src_text) in
    (* log "At line #%d |%s| ('%s' = '%s')"
      (m.impl_line - L.length lines + j - 1)
      (String.escaped line) sub src_text; *)
    let m_startp, _m_endp = m.range in
    (* Reconstruct the range of the query symbol in the implementation. *)
    let res_startp =
      { m_startp with pos_cnum = m_startp.pos_cnum + start_col }
    in
    let res_endp = { m_startp with pos_cnum = m_startp.pos_cnum + end_col } in
    let res = (res_startp, res_endp) in
    log "Success: %a --> %a" Range.pp_lexing src_pos Range.pp_lexing res;
    Some res
  with Invalid_argument _ -> None
