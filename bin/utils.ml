module M = MenhirSyntax
module MR = MenhirSyntax.Range
module L = CCList
module P = CCParse
module LA = L.Assoc
module O = CCOption
module R = CCResult
module A = CCArray
module F = CCFun
module C = CCChar
module Pr = Printf
module U = CCParse.U
module Lsp = Linol_lsp.Lsp
module Loc = M.Located
module Log = (val Logs.src_log Linol.logs_src)
include Lsp.Types

type uri = Lsp.Types.DocumentUri.t

let pr = Pr.printf
let spr = Pr.sprintf
let epr = Pr.eprintf

(** Adapted from ocaml-lsp/ocaml-lsp-server/src/position.ml *)
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

  let logical position =
    let line = position.line + 1 in
    let col = position.character in
    `Logical (line, col)
end

(** Adapted from ocaml-lsp/ocaml-lsp-server/src/range.ml *)
module Range = struct
  include Lsp.Types.Range

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

  let contains (x : t) (y : t) =
    let open Ordering in
    match
      (Position.compare x.start y.start, Position.compare x.end_ y.end_)
    with
    | (Lt | Eq), (Gt | Eq) -> true
    | _ -> false

  (* Compares ranges by their lengths*)
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

let compile_completions ~(kind : CompletionItemKind.t) :
    (string * string option * string option * string list) list ->
    CompletionItem.t list =
  L.map (fun (label, detail, snippet, docs) ->
      CompletionItem.create ~kind ~label
        ?labelDetails:
          (O.map
             (fun detail -> CompletionItemLabelDetails.create ~detail ())
             detail)
        ?insertText:snippet
        ?insertTextFormat:(O.map (fun _ -> InsertTextFormat.Snippet) snippet)
        ~documentation:
          (`MarkupContent
             (MarkupContent.create ~kind:Markdown
                ~value:(String.concat "\n\n" docs)))
        ())
