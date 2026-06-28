include Menhir_lsp_lib.Utils

module Config = struct
  type t = {
    tabsize : int; [@default 2]
    maxWidth : int; [@default 80]
    noLeadingBar : bool; [@default false]
    indentOnce : bool; [@default false]
    semiAfterProducer : bool; [@default false]
    breakLongRegexps : bool; [@default false]
    breakRegexpGroups : bool; [@default false]
  }
  [@@deriving make, yojson]
  (** The formatting options to customize the output of [menhirformat]. *)

  let default_config : t = make ()
end

module PPrint = struct
  include PPrint

  let text = string
  let escaped = String.escaped >> arbitrary_string

  let between sep d1 d2 =
    match (is_empty d1, is_empty d2) with
    | false, false -> d1 ^^ sep ^^ d2
    | false, true -> d1
    | _ -> d2

  let ( // ) = between hardline
  let ( //// ) = between (twice hardline)
  let ( ^-^ ) = between (blank 1)
  let ( ^/^ ) = between (break 1)
  let ( <|> ) d e = if is_empty d then e else d
  let ( <!> ) d e = if is_empty d then empty else e

  (** [sep ^| d] prepends [sep] to [d] if [d] is nonempty. *)
  let ( ^| ) sep d = d <!> sep ^^ d

  (** [d |^ sep] appends [sep] to [d] if [d] is nonempty. *)
  let ( |^ ) d sep = d <!> d ^^ sep

  (** A smarter [separate_map] that doesn't insert [sep] if either side is
      empty. *)
  let separate_map sep f docs =
    L.fold_left (fun accu -> f >> between sep accu) empty docs

  (** Indexed [separate_map]. *)
  let separate_mapi sep f docs =
    L.foldi (fun accu idx -> f idx >> between sep accu) empty docs

  let separate sep = separate_map sep (fun x -> x)

  (** A smarter [flow_map] that doesn't insert [sep] if either side is empty. *)
  let flow_map sep f docs =
    L.foldi
      (fun accu i doc ->
        let doc' = f doc in
        if i = 0 then doc'
        else if is_empty accu then doc'
        else accu ^^ group (sep ^| doc'))
      empty docs

  let flow_right_map sep f docs =
    L.foldi
      (fun accu i doc ->
        let doc' = f doc in
        if i = 0 then doc'
        else if is_empty accu then doc'
        else group (accu |^ sep) ^^ doc')
      empty docs

  let flow sep = flow_map sep (fun x -> x)
  let flow_right sep = flow_right_map sep (fun x -> x)
  let if_ ?(else_ = empty) ~then_ b = if b then then_ else else_
  let barspace = text "| "
  let enclose l x r = enclose l r x
end

let doc_of_string ?(input_file = "") text : TD.t =
  TD.make ~position_encoding:`UTF8
    {
      textDocument =
        { languageId = ""; text; uri = Uri.of_path input_file; version = 0 };
    }

let get_test_helpers parse format =
  let format ?(config = Config.default_config) text =
    text |> parse
    |> Result.fold
         ~ok:(fun partial_grammar ->
           format ~config ~ast:partial_grammar ~doc:(doc_of_string text))
         ~error:(fun (msg, range) -> spr "%s at %a" msg Range.pp_lexing range)
  in
  let format_and_print ?(config = Config.default_config) text : unit =
    text |> format ~config |> print_endline
  in
  (format, format_and_print)
