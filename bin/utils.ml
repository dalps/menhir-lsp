module M = MenhirSyntax
module MR = MenhirSyntax.Range
module F = CCFun

module L = struct
  include CCList

  (** Like [let*] but also supplies the index. *)
  let ( let@+ ) (x : 'a t) (f : int * 'a -> 'b) : 'b t = mapi (F.curry f) x

  (** Like [let+] but also supplies the index. *)
  let ( let@* ) (x : 'a t) (f : int * 'a -> 'b t) : 'b t =
    flat_map_i (F.curry f) x

  (** Iterate on the list until [Some] is produced. Transition into option
      monad. *)
  let ( let*? ) (x : 'a t) (f : 'a -> 'b option) : 'b option = find_map f x

  (** Like [let*?] but also supplies the index. *)
  let ( let@*? ) (x : 'a t) (f : int * 'a -> 'b option) : 'b option =
    find_mapi (F.curry f) x

  (** Analogous to [CCOption.if_]. *)
  let if_ (p : 'a -> bool) (x : 'a) : 'a t = if p x then [ x ] else []
end

module P = CCParse
module LA = L.Assoc

module O = struct
  include CCOption

  let ( <|> ) (a : 'a option) (b : unit -> 'a option) =
    match (a, b) with Some a, _ -> Some a | None, f -> f ()

  let get_or_nil (t : 'a list t) : 'a list = get_or ~default:[] t
  let get_string = get_or ~default:""
end

module R = struct
  include CCResult

  let get_or_nil (t : ('a list, 'err) t) : 'a list = get_or ~default:[] t
end

module A = CCArray
module C = CCChar
module Pr = Printf
module U = CCParse.U
module Lsp = Linol_lsp.Lsp
module Loc = M.Located
module Log = (val Logs.src_log Linol.logs_src)
include Lsp.Types
module Uri = DocumentUri
module Text_document = Lsp.Text_document
module TD = Text_document

type notify_back = Linol_lwt.Jsonrpc2.notify_back
type uri = Lsp.Types.DocumentUri.t
type word = { v : string; p : Range.t; td : Text_document.t }

let server_name = "menhir-lsp"
let pr = Pr.printf
let spr = Pr.sprintf
let epr = Pr.eprintf
let ( >> ) = CCFun.( %> )

let log ~(notify_back : notify_back) ~type_ =
  Printf.ksprintf (fun s -> notify_back#send_log_msg ~type_ s |> ignore)

let log_info ~(notify_back : notify_back) = log ~notify_back ~type_:Info
let log_error ~(notify_back : notify_back) = log ~notify_back ~type_:Error

(** Adapted from
    https://github.com/ocaml/ocaml-lsp/blob/master/ocaml-lsp-server/src/position.ml
*)
module Position = struct
  include Lsp.Types.Position

  let start = { line = 0; character = 0 }
  let show ({ character; line } : t) = spr "%d:%d" line character

  let is_dummy (lp : Lexing.position) =
    lp.pos_lnum = Lexing.dummy_pos.pos_lnum
    && lp.pos_cnum = Lexing.dummy_pos.pos_cnum

  let of_lexical_position_opt (lex_position : Lexing.position) : t option =
    if is_dummy lex_position then None
    else
      let line = lex_position.pos_lnum - 1 in
      let character = lex_position.pos_cnum - lex_position.pos_bol in
      if not (line >= 0 && character >= 0) then epr "converting dummy position";
      let line = max line 0 in
      let character = max character 0 in
      Some { line; character }

  let of_lexical_position (lex_position : Lexing.position) : t =
    of_lexical_position_opt lex_position |> O.get_or ~default:start

  let ( - ) ({ line; character } : t) (t : t) : t =
    { line = line - t.line; character = character - t.character }

  let abs ({ line; character } : t) : t =
    { line = abs line; character = abs character }

  let compare ({ line; character } : t) (t : t) : Ordering.t =
    CCOrd.(pair int int) (line, character) (t.line, t.character)
    |> Ordering.of_int

  let compare_inclusion (t : t) (r : Lsp.Types.Range.t) =
    match (compare t r.start, compare t r.end_) with
    | Lt, Lt -> `Outside (abs (r.start - t))
    | Gt, Gt -> `Outside (abs (r.end_ - t))
    | Eq, Lt | Gt, Eq | Eq, Eq | Gt, Lt -> `Inside
    | Eq, Gt | Lt, Eq | Lt, Gt -> assert false (* uncanny *)

  let is_inside (t : t) r = compare_inclusion t r = `Inside

  let logical position =
    let line = position.line + 1 in
    let col = position.character in
    `Logical (line, col)
end

(** Adapted from
    https://github.com/ocaml/ocaml-lsp/blob/master/ocaml-lsp-server/src/range.ml
*)
module Range = struct
  include Lsp.Types.Range

  let create ~(end_ : Position.t) ~(start : Position.t) : t =
    assert (Position.(compare start end_) <> Gt);
    Range.create ~end_ ~start

  let end_ t = t.end_
  let start t = t.start

  let show ({ end_; start } : t) =
    spr "[ %s, %s ]" (Position.show start) (Position.show end_)

  let of_lexical_positions ((start, end_) : Lexing.position * Lexing.position) =
    Range.create
      ~start:(Position.of_lexical_position start)
      ~end_:(Position.of_lexical_position end_)

  let compare (x : t) (y : t) =
    match Position.compare x.start y.start with
    | (Lt | Gt) as r -> r
    | Ordering.Eq -> Position.compare x.end_ y.end_

  let compare_inclusion (x : t) (y : t) =
    match
      Position.
        ( compare x.start y.start,
          compare x.start y.end_,
          compare x.end_ y.start,
          compare x.end_ y.end_ )
    with
    | Eq, Eq, Eq, Eq -> `Empty
    | Eq, _, _, Eq -> `Equal
    (* | _, _, _, Eq -> `RaggedLeft
    | Eq, _, _, _ -> `RaggedRight *)
    | (Lt | Eq), _, _, (Gt | Eq) -> `Contain
    | (Gt | Eq), _, _, (Lt | Eq) -> `Contained
    (* Or you could provide an integer distance

    | Lt, _, Gt, Lt -> `OverlapBefore
    | Gt, Lt, _, Gt -> `OverlapAfter
    | Lt, _, Eq, Lt -> `AdjacentBefore
    | Gt, Eq, _, Gt -> `AdjacentAfter
    *)
    | Lt, _, _, Lt -> `Before
    | Gt, _, _, Gt -> `After

  let contains (x : t) (y : t) =
    let open Ordering in
    match
      (Position.compare x.start y.start, Position.compare x.end_ y.end_)
    with
    | (Lt | Eq), (Gt | Eq) -> true
    | _ -> false

  (* Compares ranges by their lengths *)
  let compare_size (x : t) (y : t) =
    let dx = Position.(x.end_ - x.start) in
    let dy = Position.(y.end_ - y.start) in
    CCOrd.(pair int int) (dx.line, dy.line) (dx.character, dy.character)

  let first_line =
    let start = { Position.line = 0; character = 0 } in
    let end_ = { Position.line = 1; character = 0 } in
    { start; end_ }

  let resize_for_edit { TextEdit.range; newText } =
    let lines = CCString.lines newText in
    match lines with
    | [] -> { range with end_ = range.start }
    | several_lines ->
        let end_ =
          let start = range.start in
          let line = start.line + List.length several_lines - 1 in
          let character =
            let last_line_len =
              L.last 1 several_lines |> O.of_list |> Option.get |> String.length
            in
            start.character + last_line_len
          in
          { Position.line; character }
        in
        { range with end_ }

  let whole_document (td : Text_document.t) : Range.t =
    let dummy_edit = TextEdit.create ~range:first_line ~newText:(TD.text td) in
    resize_for_edit dummy_edit

  let overlaps x y =
    let open Ordering in
    match
      (Position.compare x.start y.end_, Position.compare x.end_ y.start)
    with
    | (Lt | Eq), (Gt | Eq) | (Gt | Eq), (Lt | Eq) -> true
    | _ -> false
end

(** Surround the given string in Markdown code block fences. *)
let md_fenced ?(flavor = "") s = spr "```%s\n%s\n```" flavor s

(* module LspTypes = struct
  include Lsp.Types
  module Position = Position
  module Range = Position
end *)

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

(** From ocaml-lsp/ocaml-lsp-server/src/document.ml *)
let substring doc range =
  let start, end_ = TD.absolute_range doc range in
  let text = TD.text doc in
  if start < 0 || start > end_ || end_ > String.length text then None
  else Some (CCStringLabels.sub text ~pos:start ~len:(end_ - start))

module PPrint = struct
  include PPrint

  let text = string

  let between sep d1 d2 =
    match (is_empty d1, is_empty d2) with
    | false, false -> d1 ^^ sep ^^ d2
    | false, true -> d1
    | _ -> d2

  let ( // ) = between hardline
  let ( //// ) = between (twice hardline)
  let ( ^-^ ) = between space
  let ( <|> ) d e = if is_empty d then e else d
  let ( <!> ) d e = if is_empty d then empty else e

  (** Prefix [sep] to [d] if [d] is nonempty. *)
  let ( ^! ) sep d = d <!> sep ^^ d

  (** Append [sep] to [d] if [d] is nonempty. *)
  let ( !^ ) d sep = d <!> d ^^ sep

  (** A smarter [flow_map] that doesn't prepend [sep] to empty documents. *)
  let flow_map sep f docs =
    L.foldi
      (fun accu i doc -> if i = 0 then f doc else accu ^^ group (sep ^! f doc))
      empty docs

  let flow sep = flow_map sep (fun x -> x)
  let if_ ?(else_ = empty) ~then_ b = if b then then_ else else_
  let barspace = text "| "
end
