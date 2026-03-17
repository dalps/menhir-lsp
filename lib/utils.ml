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
(* module Loc = M.Located *)
module Log = (val Logs.src_log Linol.logs_src)
include Lsp.Types
module Uri = DocumentUri
module Text_document = Lsp.Text_document
module TD = Text_document

type notify_back = Linol_lwt.Jsonrpc2.notify_back
type uri = Lsp.Types.DocumentUri.t
type word = { v : string; p : Range.t; td : Text_document.t }

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

(** From ocaml-lsp/ocaml-lsp-server/src/document.ml *)
let substring doc range =
  let start, end_ = TD.absolute_range doc range in
  let text = TD.text doc in
  if start < 0 || start > end_ || end_ > String.length text then None
  else Some (CCStringLabels.sub text ~pos:start ~len:(end_ - start))
