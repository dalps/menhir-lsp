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

(* --------------------------------------------------------------------------- *)
(* Display an informational message about the contents of a [.messages] file.  *)

let count_input_sentences oruns : int =
  List.fold_left
    (OrComment.fold (fun s run -> s + OrComment.count run.elements))
    0 oruns

let count_error_messages oruns : int = OrComment.count oruns

let stats (oruns : oruns) : string =
  spr "Read %d sample input sentences and %d error messages.\n%!"
    (count_input_sentences oruns)
    (count_error_messages oruns)

(* --------------------------------------------------------------------------- *)

let mkcomment c accu =
  if String.length c = 0 then accu else OrComment.Comment c :: accu

let read_messages mode filename : oruns =
  Report.monitor mode @@ fun c ->
  let open MenhirSyntax in
  let open Segment in
  (* Read and segment the file. *)
  let segments : (tag * string located * Lexing.lexbuf) list =
    segment filename
  in
  (* Process the segments, two by two. We expect one segment to contain
                a non-empty series of sentences, and the next segment to contain
                free-form text. *)
  let rec loop accu segments =
    match segments with
    | [] -> List.rev accu
    | (Whitespace, comments, _) :: segments ->
        loop (mkcomment comments.v accu) segments
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
  log "%s" @@ stats oruns;
  oruns

(** End of code grabbed from menhir/middle/Messages.ml *)
(* --------------------------------------------------------------------------- *)

type state = { segments : oruns }

let load_state_from_contents (uri : uri) contents :
    (state, Diagnostic.t list) result =
  let open MenhirSyntax in
  let input_file = Uri.to_path uri in
  try
    let segments = read_messages Report.(`SignalIsWarning) input_file in
    Ok { segments }
  with _ -> Error []

(* A few commands that make up the message-editing UI *)
let stats state = stats state.segments

(*

let next_message state : Range.t = _
let next_unhandled_message state : Range.t = _
let previous_message state : Range.t = _

*)

let fold_entries (f : 'a -> 'b) (elems : 'a OrComment.t list) : 'b list =
  L.filter_map
    (fun (elem : 'a OrComment.t) ->
      match elem with
      | OrComment.Thing x -> Some (f x)
      | OrComment.Comment _ -> None)
    elems

let document_symbols (state : state) : DocumentSymbol.t list =
  fold_entries
    (fun { elements; delimiter; message } ->
      (* We use the first line of the entry's message as the symbol name. *)
      let name =
        CCString.lines message.v |> L.head_opt |> O.get_or ~default:message.v
      in
      (* We use the first sentence of the entry as the symbol detail. *)
      let detail =
        let open O in
        elements |> L.head_opt
        >>= OrComment.fold
              (fun _ (_, (nonterminal, terminals)) ->
                let pp_option = Format.pp_print_option in
                let pp_raw_symbol out (text, _, _) = pp_string out text in
                some
                @@ spr "%a: %a" (pp_option pp_raw_symbol) nonterminal
                     (pp_list pp_raw_symbol) terminals)
              None
      in
      let range = Utils.Range.of_lexical_positions message.p in
      DocumentSymbol.create ?detail ~name ~kind:Constant ~range
        ~selectionRange:range ())
    state.segments

let folding_ranges ~(doc : document) (state : state) : FoldingRange.t list =
  fold_entries
    (fun { elements; delimiter; message } ->
      let sentences = OrComment.things elements in
      let startp =
        O.(
          L.head_opt sentences >|= fst >|= fst <+> Some (Located.startp message))
        |> O.get_exn_or "startp"
      in
      let endp = Located.endp message in
      let startLine, startCharacter =
        (startp.pos_lnum - 1, startp.pos_cnum - startp.pos_bol + 1)
      in
      let endLine, endCharacter =
        (endp.pos_lnum - 1, endp.pos_cnum - endp.pos_bol + 1)
      in
      FoldingRange.create ~startLine ~startCharacter ~endLine ~endCharacter ())
    state.segments
