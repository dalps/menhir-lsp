(* This file contains LSP handlers that help navigate the Menhir .messages file format. *)

open Utils
open MenhirSyntax
open Syntax
open Located

(* --------------------------------------------------------------------------- *)
(** [menhir-lsp] Start of code grabbed from menhir/middle/Messages.ml. We don't
    get into the LR1 thingies. *)

(* [menhir-lsp] Our dumbed-down version of sentences. *)
type sentence = nonterminal located option * terminal located list

(* A located sentence is a sentence
   together with its start and end positions. *)

type located_sentence = Range.ranges * sentence

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
  delimiter : string;
  (* A message. *)
  message : message;
}

type orun = run OrComment.t

(* A [.messages] file is a list of runs or comments. *)
type oruns = orun list

(* The name of a [.messages] file. *)
type filename = string

(* --------------------------------------------------------------------------- *)
(* Display an informational message about the contents of a [.messages] file.  *)

let count_input_sentences oruns : int =
  List.fold_left
    (OrComment.fold (fun s run -> s + OrComment.count run.elements))
    0 oruns

let count_error_messages oruns : int = OrComment.count oruns

let stats (oruns : oruns) : unit =
  epr "Read %d sample input sentences and %d error messages.\n%!"
    (count_input_sentences oruns)
    (count_error_messages oruns)

(* --------------------------------------------------------------------------- *)

let read_messages mode filename : oruns =
  Report.monitor mode @@ fun c ->
  let open MenhirSyntax in
  let open Segment in
  (* Read and segment the file. *)
  let segments : (tag * string * Lexing.lexbuf) list = segment filename in
  (* Process the segments, two by two. We expect one segment to contain
                a non-empty series of sentences, and the next segment to contain
                free-form text. *)
  let rec loop accu segments =
    match segments with
    | [] -> List.rev accu
    | (Whitespace, comments, _) :: segments ->
        loop (mkcomment comments accu) segments
    | (Segment, _, lexbuf) :: segments -> (
        (* Read a series of raw sentences. *)
        match RawSentenceParser.entry RawSentenceLexer.lex lexbuf with
        | exception Parsing.Parse_error ->
            Report.error c [ Range.current lexbuf ] "ill-formed sentence."
        | elements -> (
            (* [elements] is a list of raw sentences or comments. Validate it.
                Any sentences that do not pass validation are removed (and
                error messages are emitted). In an effort to be robust, we
                continue. If there remain zero sentences, then this entry is
                removed entirely. *)
            (* let elements = validate_entry c elements in *)
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
                Report.error c
                  [ Range.current lexbuf ]
                  "missing a final message. I may be desynchronized."
            | (Segment, _, _) :: _
            | (Whitespace, _, _) :: (Whitespace, _, _) :: _ ->
                (* Should not happen, thanks to the alternation between the
                two kinds of segments. *)
                assert false))
  in
  let oruns = loop [] segments in
  stats oruns;
  oruns

(** End of code grabbed from menhir/middle/Messages.ml *)
(* --------------------------------------------------------------------------- *)

type state = { segments : _ }

let load_state_from_contents input_file contents =
  let open MenhirSyntax in
  let segments = Segment.segment input_file in
  RawSentenceParser.entry RawSentenceLexer.lex
