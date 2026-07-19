(* This file contains LSP handlers that help navigate the Menhir .messages file format. *)

open Utils
open MenhirSyntax
open Syntax
open Located

(* --------------------------------------------------------------------------- *)
(** [menhir-lsp] Start of code grabbed from menhir/middle/Messages.ml. We don't
    get into the LR1 thingies. *)

(* A located sentence is a sentence
   together with its start and end positions. *)

(* [menhir-lsp] changed [ranges] to [range] *)
type located_sentence = Range.range * RawSentence.raw_sentence

let ranges ((ranges, _) : located_sentence) = ranges

(* A targeted sentence is a located sentence
   together with the target to which it leads.  *)

type targeted_sentence = located_sentence

(* An error message. *)

type message = string

(* A run is a series of targeted sentences or comments, followed with a
   delimiter (composed at least one blank line and possibly comments),
   followed with an error message. *)

type tso = targeted_sentence OrComment.t

type run = {
  (* A list of sentences. *)
  elements : tso list;
  (* A delimiter. *)
  delimiter : string located;
  (* A message. *)
  message : message located; (* [menhir-lsp] made located. *)
}

type orun = run OrComment.t

(* A [.messages] file is a list of runs or comments. *)
type oruns = orun list

(* The name of a [.messages] file. *)
type filename = string

let default_message = "<YOUR SYNTAX ERROR MESSAGE HERE>\n"

(* --------------------------------------------------------------------------- *)
(* Display an informational message about the contents of a [.messages] file.  *)

let count_input_sentences oruns : int =
  List.fold_left
    (OrComment.fold (fun s run -> s + OrComment.count run.elements))
    0 oruns

let count_error_messages oruns : int = OrComment.count oruns

let stats (oruns : oruns) : string =
  (* spr "Read %d sample input sentences and %d error messages.\n%!" *)
  spr "%d sample input sentences, %d error messages\n%!"
    (count_input_sentences oruns)
    (count_error_messages oruns)

(* --------------------------------------------------------------------------- *)

let mkcomment c accu =
  if String.length c = 0 then accu else OrComment.Comment c :: accu

let read_messages filename : (oruns, Range.ranges * string) result =
  let open MenhirSyntax in
  let open Segment in
  (* Read and segment the file. *)
  let segments : (tag * string located * Lexing.lexbuf) list =
    segment filename
  in
  let error ranges msg = Error (ranges, msg) in
  (* Process the segments, two by two. We expect one segment to contain
                a non-empty series of sentences, and the next segment to contain
                free-form text. *)
  let rec loop accu segments =
    match segments with
    | [] -> Ok (List.rev accu)
    | (Whitespace, comments, _) :: segments ->
        loop (mkcomment comments.v accu) segments
    | (Segment, _, lexbuf) :: segments -> (
        (* Read a series of raw sentences. *)
        match RawSentenceParser.entry RawSentenceLexer.lex lexbuf with
        | (exception RawSentenceLexer.Error) | (exception Parsing.Parse_error)
          ->
            error [ Range.current lexbuf ] "ill-formed sentence."
        | elements -> (
            (* [elements] is a list of raw sentences or comments. Validate it.
                Any sentences that do not pass validation are removed (and
                error messages are emitted). In an effort to be robust, we
                continue. If there remain zero sentences, then this entry is
                removed entirely. *)
            (* let elements = validate_entry c elements in *)
            let elements : tso list =
              L.map
                (OrComment.map (fun raw_sentence ->
                     (RawSentence.range raw_sentence, raw_sentence)))
                elements
            in
            (* In principle, we should now find a segment of whitespace
                followed with a segment of text. By construction, the two
                kinds of segments alternate. *)
            match segments with
            | (Whitespace, delimiter, _) :: (Segment, message, _) :: segments ->
                if OrComment.count elements = 0 then
                  (* There remain zero sentences. Skip this entry. *)
                  loop accu segments
                else
                  (* Accumulate this entry. *)
                  let run = { elements; delimiter; message } in
                  loop (Thing run :: accu) segments
            | [] | [ _ ] ->
                error
                  [ Range.current lexbuf ]
                  "missing a final message. I may be desynchronized."
            | (Segment, _, _) :: _
            | (Whitespace, _, _) :: (Whitespace, _, _) :: _ ->
                (* Should not happen, thanks to the alternation between the
                two kinds of segments. *)
                assert false))
  in
  loop [] segments

(** End of code grabbed from menhir/middle/Messages.ml *)
(* --------------------------------------------------------------------------- *)

type state = { segments : oruns; entries : run list }
(** [segments] includes comments inbetween entries, [entries] does not. *)

let entry_range ({ elements; delimiter; message } : run) =
  let sentences = OrComment.things elements in
  let startp =
    O.(L.head_opt sentences >|= fst >|= fst <+> Some (Located.startp message))
    |> O.get_exn_or "startp"
  in
  let endp = Located.endp message in
  (startp, endp)

let load_state_from_contents (uri : uri) contents :
    (state, Diagnostic.t list) result =
  let open MenhirSyntax in
  let input_file = Uri.to_path uri in
  match read_messages input_file with
  | Ok segments ->
      log "%s" (stats segments);
      Ok { segments; entries = OrComment.things segments }
  | Error (ranges, message) ->
      let diags =
        L.map
          (fun range ->
            Diagnostic.create ~message:(`String message)
              ~range:(Utils.Range.of_lexical_positions range)
              ())
          ranges
      in
      Error diags

(* A few commands that make up the message-editing UI *)
let stats state = stats state.segments

let fold_entries (f : int -> 'acc -> 'a -> 'acc) (accu : 'acc)
    (elems : 'a OrComment.t list) : 'acc =
  L.foldi (fun accu idx elem -> OrComment.fold (f idx) accu elem) accu elems

let map_entries (f : 'a -> 'b) (elems : 'a OrComment.t list) : 'b list =
  L.filter_map
    (fun (elem : 'a OrComment.t) ->
      match elem with
      | OrComment.Thing x -> Some (f x)
      | OrComment.Comment _ -> None)
    elems

let document_symbols (state : state) : DocumentSymbol.t list =
  let open L in
  let@+ idx, ({ elements; delimiter; message } as run) = state.entries in
  (* We use the first line of the entry's message as the symbol name. *)
  let name =
    let first_line =
      CCString.lines message.v |> L.head_opt |> O.get_or ~default:message.v
    in
    spr "%d. %s" (idx + 1) first_line
  in
  let pp_sentence out ((nonterminal, terminals) : RawSentence.raw_sentence) =
    let pp_option = Format.pp_print_option in
    let pp_raw_symbol out (text, _, _) = pp_string out text in
    pf out "%a: %a" (pp_option pp_raw_symbol) nonterminal
      (pp_list ~pp_sep:(fun out () -> pf out " ") pp_raw_symbol)
      terminals
  in
  (* We display a summary of the sentences that trigger the message next to it. *)
  let detail =
    let open O in
    match OrComment.things elements with
    | [] -> None
    | [ (_, s) ] -> Some (spr "%a" pp_sentence s)
    | (_, s) :: ss -> Some (spr "%a and %d other" pp_sentence s (L.length ss))
  in
  let open Utils.Range in
  (*
    - [entry_range] is used to determine if the cursor is inside a database entry.
    - [msg_range] is used to place the cursor on the message when the user selects an entry from the outline.
    *)
  let entry_range = of_lexical_positions (entry_range run) in
  let msg_range = of_lexical_positions message.p in
  (* assert (compare_inclusion entry_range msg_range = `Contain); *)
  DocumentSymbol.create ?detail ~name ~kind:Constant ~range:entry_range
    ~selectionRange:msg_range ()

let folding_ranges ~(doc : document) (state : state) : FoldingRange.t list =
  map_entries
    (fun run ->
      let startp, endp = entry_range run in
      let startLine, startCharacter =
        (startp.pos_lnum - 1, startp.pos_cnum - startp.pos_bol + 1)
      in
      let endLine, endCharacter =
        (endp.pos_lnum - 1, endp.pos_cnum - endp.pos_bol + 1)
      in
      FoldingRange.create ~startLine ~startCharacter ~endLine ~endCharacter ())
    state.segments

let search_forward filter state = L.find_predi filter state.entries

let search_backward filter state =
  let last_idx = L.length state.entries - 1 in
  L.find_predi (fun idx -> filter (last_idx - idx)) (L.rev state.entries)

let focus_message search_fun state ~pos : Utils.Range.t option =
  let open L in
  let open Utils in
  (* 1. Search for the entry we're in. *)
  let@*? idx, run = state.entries in
  let module Range = Utils.Range in
  let msg_range = entry_range run |> Range.of_lexical_positions in
  let open O in
  let* _ = guard (Position.is_inside pos msg_range) in
  (* 2. Search for the entry that satisfies [search_fun], which is given the index of the current entry.
    Return the message range of the target entry if found. *)
  log ~debug:false "We're inside %s %a" run.message.v Range.pp msg_range;
  let+ target = search_fun idx state in
  Range.of_lexical_positions target.message.p

let next_message =
  focus_message (fun current -> search_forward (fun i _ -> i = current + 1))

let previous_message =
  focus_message (fun current -> search_forward (fun i _ -> i = current - 1))

let next_dummy_message =
  focus_message (fun current ->
      search_forward (fun i { message; _ } ->
          i > current && message.v = default_message))

let previous_dummy_message =
  focus_message (fun current ->
      search_backward (fun i { message; _ } ->
          i < current && message.v = default_message))
