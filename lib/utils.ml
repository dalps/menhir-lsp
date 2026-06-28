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
type word = { v : string; p : Range.t; offset : int; td : Text_document.t }

let pr = Format.printf
let spr = Format.asprintf
let epr = Format.eprintf
let pf = Format.fprintf
let ( >> ) = CCFun.( %> )
let notify_back_ref : notify_back option ref = ref None

(** [log] prints to the first available output channel. It will use caller's
    [notify_back] argument if provided, falling back to the optional value
    stored in the global variable [notify_back_ref] and ultimately default to
    [prerr_endline]. *)
let log ?(notify_back : notify_back option) ?(kind = MessageType.Info) s =
  match (notify_back, !notify_back_ref) with
  | None, None -> Format.kasprintf prerr_endline s
  | None, Some notify_back | Some notify_back, _ ->
      Format.kasprintf
        (fun s -> notify_back#send_log_msg ~type_:kind s |> ignore)
        s

(** Identical to [log] but returns a unit promise. *)
let log' ?(notify_back : notify_back option) ?(kind = MessageType.Info) s =
  match (notify_back, !notify_back_ref) with
  | None, None -> Format.kasprintf (fun s -> prerr_endline s |> Lwt.return) s
  | None, Some notify_back | Some notify_back, _ ->
      Format.kasprintf (fun s -> notify_back#send_log_msg ~type_:kind s) s

(** Logging helper that allows to specify a message source that will be
    prepended to every log message.

    Override with a concrete [src] argument like this:
    [let log s = log_src "my_source" s in ..].

    Set the [debug] flag to false to mute the messages from this source. *)
let log_src ?(debug = true) ?notify_back ?kind src s =
  Format.kasprintf
    (fun s -> if debug then log ?notify_back ?kind "[%s] %s" src s)
    s

let log_info = log ~kind:Info
let log_error = log ~kind:Error
let log_info' = log' ~kind:Info
let log_error' = log' ~kind:Error

(** Adapted from
    https://github.com/ocaml/ocaml-lsp/blob/master/ocaml-lsp-server/src/position.ml
*)
module Position = struct
  include Lsp.Types.Position

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

  let of_lexical_position (lex_position : Lexing.position) : t =
    of_lexical_position_opt lex_position |> O.get_or ~default:start

  let pp out ({ character; line } : t) = pf out "%d:%d" line character
  let pp_lexing out = of_lexical_position >> pf out "%a" pp
  let show = spr "%a" pp
  let show_lexing = spr "%a" pp_lexing

  let ( - ) ({ line; character } : t) (t : t) : t =
    { line = line - t.line; character = max 0 (character - t.character) }

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

  let ( + ) ({ line; character } : t) (t : t) : t =
    { line = line + t.line; character = character + t.character }
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

  let of_lexical_positions ((start, end_) : Lexing.position * Lexing.position) =
    create
      ~start:(Position.of_lexical_position start)
      ~end_:(Position.of_lexical_position end_)

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
    Range.create
      ~start:{ t.start with character = max 0 (t.start.character - 1) }
      ~end_:{ t.end_ with character = t.end_.character + 1 }

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

let pp_position out
    ({ pos_fname; pos_lnum; pos_bol; pos_cnum } : Lexing.position) =
  pf out "{lnum = %d; bol = %d; cnum = %d}" pos_lnum pos_bol pos_cnum

module Lexing = struct
  include Lexing

  let pp_position out = pp_position
end

let pp_option pp_v out (o : 'a option) =
  Format.pp_print_option
    ~none:(fun out _ -> pf out "None")
    (fun out v -> pf out "Some (%a)" pp_v v)
    out o

let pp_string = Format.pp_print_string
