module M = MenhirSyntax
module MR = MenhirSyntax.Range
module Json = Yojson.Safe
include Menhir_lsp_lib.Utils
module Ivl_map = Interval_map.Make (Int)
module Ivl = Ivl_map.Interval

let server_name = "menhir-lsp"

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
  match P.drop_prefix p_dir ~prefix:p_root with
  | None ->
      Error
        (spr
           "No config found for %s. Make sure (menhir (modules .. %s ..)) is \
            included in the stanza's dune file."
           s_name s_slug)
  | Some p_rel ->
      let p_ctx = P.of_string (F.concat root ctx) in
      let p_res = P.append_local p_ctx p_rel in
      let res = F.concat (P.to_string p_res) (s_slug ^ s_ext) in
      Ok res

module MK = Merlin_kernel
module QP = Query_protocol

let merlin_configs : (uri, MK.Mconfig.t) Hashtbl.t = Hashtbl.create 42

let get_merlin_config ~(notify_back : notify_back) ~(uri : uri) =
  let path = DocumentUri.to_path uri in
  let dir = Filename.dirname path in
  let open O in
  let+ ctx, config_path = MK.Mconfig_dot.find_project_context dir in
  let dot, failures = MK.Mconfig_dot.get_config ctx path in
  let concat = String.concat ", " in
  log_info ~notify_back
    {|Search result for Merlin config of %s:
  Errors: %s
  Source path: %s
  Build path: %s|}
    path (concat failures) (concat dot.source_path) (concat dot.build_path);
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
      let+ config = get_merlin_config ~notify_back ~uri in
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

let get_merlin_compls ~(notify_back : notify_back) ~(uri : uri)
    ~(pos : Position.t) (prefix : word) =
  let open O in
  let+ config = get_merlin_config ~notify_back ~uri in
  let source = MK.Msource.make (Text_document.text prefix.td) in
  let pipeline = MK.Mpipeline.make config source in
  MK.Mpipeline.with_pipeline pipeline (fun _ ->
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
      log_info ~notify_back "# merlin completions: %d" (L.length compls);
      compls)

let parse_ocaml_impl s =
  let lexbuf = Lexing.from_string s in
  Ppxlib.Parse.implementation lexbuf

let parse_ocaml_type s =
  let lexbuf = Lexing.from_string s in
  Ppxlib.Parse.core_type lexbuf
