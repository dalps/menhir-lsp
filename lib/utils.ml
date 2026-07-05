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
module LSP = Lsp.Types
include Lsp.Types
module Uri = DocumentUri
module Text_document = Lsp.Text_document

module TD = struct
  include Text_document

  let create ?(position_encoding = `UTF8) ?(version = 0) ?(languageId = "")
      ~text uri =
    make ~position_encoding
      (DidOpenTextDocumentParams.create
         ~textDocument:{ text; version; languageId; uri })
end

let pr = Format.printf
let spr = Format.asprintf
let epr = Format.eprintf
let pf = Format.fprintf
let ( >> ) = CCFun.( %> )
let log s = Format.kasprintf prerr_endline s

(** Logging helper that allows to specify a message source that will be
    prepended to every log message.

    Override with a concrete [src] argument like this:
    [let log s = log_src "my_source" s in ..].

    Set the [debug] flag to false to mute the messages from this source. *)
let log_src ?(debug = true) src s =
  Format.kasprintf (fun s -> if debug then log "[%s] %s" src s) s

let pp_position out
    ({ pos_fname; pos_lnum; pos_bol; pos_cnum } : Lexing.position) =
  pf out "{lnum = %d; bol = %d; cnum = %d}" pos_lnum pos_bol pos_cnum

(** Adapted from
    https://github.com/ocaml/ocaml-lsp/blob/master/ocaml-lsp-server/src/position.ml
*)
module Position = struct
  include LSP.Position

  let start = { line = 0; character = 0 }

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

  let pp out ({ character; line } : t) = pf out "%d:%d" line character
  let show = spr "%a" pp

  let of_lexical_position (lex_position : Lexing.position) : t =
    of_lexical_position_opt lex_position |> O.get_or ~default:start

  let to_lexical_position ~(doc : Text_document.t) (pos : Position.t) :
      Lexing.position =
    let log s = log_src "to_lexical_position" s in
    let pos_fname = TD.documentUri doc |> Uri.to_path in
    let pos_cnum = TD.absolute_position doc pos in
    let pos_bol = pos_cnum - pos.character in
    let res =
      Lexing.{ pos_fname; pos_lnum = pos.line + 1; pos_bol; pos_cnum }
    in
    log "%a -> %a" pp pos pp_position res;
    res

  let pp_lexing out = of_lexical_position >> pf out "%a" pp
  let show_lexing = spr "%a" pp_lexing

  let ( - ) ({ line; character } : t) (t : t) : t =
    { line = line - t.line; character = max 0 (character - t.character) }

  let abs ({ line; character } : t) : t =
    { line = abs line; character = abs character }

  let compare ({ line; character } : t) (t : t) : Ordering.t =
    CCOrd.(pair int int) (line, character) (t.line, t.character)
    |> Ordering.of_int

  let compare_inclusion (t : t) (r : LSP.Range.t) =
    match (compare t r.start, compare t r.end_) with
    | Lt, Lt -> `Outside (abs (r.start - t))
    | Gt, Gt -> `Outside (abs (r.end_ - t))
    | Eq, Lt | Gt, Eq | Eq, Eq | Gt, Lt -> `Inside
    | Eq, Gt | Lt, Eq | Lt, Gt -> assert false (* uncanny *)

  let is_inside (t : t) r = compare_inclusion t r = `Inside

  let logical (position : t) =
    let line = position.line + 1 in
    let col = position.character in
    `Logical (line, col)

  let ( + ) ({ line; character } : t) (t : t) : t =
    { line = line + t.line; character = character + t.character }
end

(** Adapted from
    https://github.com/ocaml/ocaml-lsp/blob/master/ocaml-lsp-server/src/range.ml
*)
module Range = struct
  include LSP.Range

  let create ~(end_ : Position.t) ~(start : Position.t) : t =
    assert (Position.(compare start end_) <> Gt);
    create ~end_ ~start

  let end_ t = t.end_
  let start t = t.start

  let of_lexical_positions ((start, end_) : Lexing.position * Lexing.position) =
    create
      ~start:(Position.of_lexical_position start)
      ~end_:(Position.of_lexical_position end_)

  let to_lexical_positions ~doc ({ end_; start } : t) =
    ( Position.to_lexical_position ~doc start,
      Position.to_lexical_position ~doc end_ )

  let pp out ({ end_; start } : t) =
    pf out "[ %a, %a ]" Position.pp start Position.pp end_

  let pp_lexing out = of_lexical_positions >> pf out "%a" pp
  let show = spr "%a" pp
  let show_lexing = spr "%a" pp_lexing

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
    | Lt, _, _, Lt -> `Before Position.(y.start - x.end_)
    | Gt, _, _, Gt -> `After Position.(x.start - y.end_)

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

  (* Enlarges the given range with one column on both ends. *)
  let parenthesize (t : t) : t =
    create
      ~start:{ t.start with character = max 0 (t.start.character - 1) }
      ~end_:{ t.end_ with character = t.end_.character + 1 }

  let resize_for_edit { LSP.TextEdit.range; newText } =
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

  let whole_document (td : Text_document.t) : t =
    let dummy_edit =
      LSP.TextEdit.create ~range:first_line ~newText:(TD.text td)
    in
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

type distance = Position.t = { character : int; line : int }
type relpos = [ `Before of distance | `After of distance ]

let compare_relpos (r1 : relpos) (r2 : relpos) =
  match (r1, r2) with
  | `After d1, `After d2
  | `Before d1, `After d2
  | `After d1, `Before d2
  | `Before d1, `Before d2 ->
      CCOrd.int (abs d1.line) (abs d2.line)

(* module Relpos = struct
  type t = { rel : [ `Before | `After ]; distance : Position.t }

  let compare (r1 : t) (r2 : t) =
    CCOrd.int (abs r1.distance.line) (abs r2.distance.line)
end *)

let find_prefix text ofs =
  (* Don't look back beyond 500 chars. *)
  let max_reach = min ofs 500 in
  (* The subtext to search, [max_reach] long. *)
  let sub = String.sub text (ofs - max_reach) max_reach in
  (* The start index of the prefix. *)
  let start_ofs =
    try
      Re.Str.(
        search_backward
          (* This should include all trigger characters. *)
          (regexp {|[^a-zA-Z0-9_$%.]|})
          sub max_reach)
      + 1
    with Not_found -> 0 (* empty *)
  in
  let length = max_reach - start_ofs in
  let prefix = String.sub sub start_ofs length in

  (start_ofs, length, prefix)

type lexing_range = Lexing.position * Lexing.position

module Lexing = struct
  include Lexing

  let pp_position out = pp_position
end

let pp_option pp_v out (o : 'a option) =
  Format.pp_print_option
    ~none:(fun out _ -> pf out "None")
    (fun out v -> pf out "Some (%a)" pp_v v)
    out o

let pp_stropt out (o : string option) =
  Format.pp_print_option
    ~none:(fun out _ -> pf out "<none>")
    (fun out v -> pf out "%s" v)
    out o

let pp_string = Format.pp_print_string
