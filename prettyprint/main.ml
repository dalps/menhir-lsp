open Menhir_lsp_lib.Utils
open Menhirformat_lib.Utils

let version = "0.0.0"
let log s = Format.ksprintf prerr_endline s

let read_file_contents filename =
  try
    let ic = open_in filename in
    let contents = really_input_string ic (in_channel_length ic) in
    close_in ic;
    contents
  with _ ->
    log "File doesn't exist: %s" filename;
    exit 1

open Cmdliner
open Cmdliner.Term.Syntax

let tabsize =
  let doc = "Whitespace unit for indentation." in
  Arg.(value & opt int 2 & info [ "t"; "tabsize" ] ~docv:"COLS" ~doc)

let maxColumns =
  let doc = "Maximum number of characters per line." in
  Arg.(value & opt int 80 & info [ "w"; "width" ] ~docv:"COLS" ~doc)

let noLeadingBar =
  let doc = "Omit the optional leading bar `|` in the first case of a rule." in
  Arg.(value & flag & info [ "no-leading-bar" ] ~doc)

let indentOnce =
  let doc =
    "Add a level of indentation to the cases of a rule or (default) keep them \
     flush with the start of the definition."
  in
  Arg.(value & flag & info [ "indent-rule" ] ~doc)

let semiAfterProducer =
  let doc =
    "Terminate every producer with a semicolon in Menhir rules written in the \
     traditional syntax."
  in
  Arg.(value & flag & info [ "semi-after-producer" ] ~doc)

let breakLongRegexps =
  let doc =
    "Break long ocamllex regexps onto multiple lines when they don't fit in a \
     single line. Breaking may happen only at concatenations or alternations \
     outside groups."
  in
  Arg.(value & flag & info [ "break-long-regexps" ] ~doc)

let breakRegexpsGroups =
  let doc =
    "Allow breaking to happen inside regexp groups. Effective only when \
     $(b,--break-long-regexps) is set."
  in
  Arg.(value & flag & info [ "break-regexp-groups" ] ~doc)

let input_file =
  let doc =
    "The file to format, whose name must end with the `.mly' or `.mll' file \
     extension."
  in
  Arg.(value & pos 0 file "" & info [] ~docv:"FILE" ~doc)

let main ~config (input_file : string) =
  let open R in
  let text = read_file_contents input_file in
  let doc = doc_of_string ~input_file text in
  match Filename.extension input_file with
  | ".mly" -> (
      match MenhirSyntax.Main.load_grammar_from_file input_file with
      | Ok ast ->
          Menhirformat_lib.Menhir.main ~config ~doc ~ast |> print_endline
      | Error _ -> ())
  | ".mll" -> (
      match OcamllexSyntax.Main.parse_string text with
      | Ok ast ->
          Menhirformat_lib.Ocamllex.main ~config ~doc ~ast |> print_endline
      | Error _ -> ())
  | _ ->
      log "%s" "Unrecognized file extension: must be either '.mll' or '.mly'.";
      exit 2

let cmd =
  Cmd.v
    (Cmd.info "menhirformat" ~version
       ~doc:"A formatter for Ocamllex .mll and Menhir .mly files.")
  @@ let+ input_file = input_file
     and+ tabsize = tabsize
     and+ indentOnce = indentOnce
     and+ noLeadingBar = noLeadingBar
     and+ maxWidth = maxColumns
     and+ breakLongRegexps = breakLongRegexps
     and+ _breakRegexpsGroups = breakRegexpsGroups
     and+ semiAfterProducer = semiAfterProducer in
     let config =
       Config.make ~tabsize ~indentOnce ~noLeadingBar ~semiAfterProducer
         ~maxWidth ~breakLongRegexps ()
     in
     main ~config input_file

let () = if !Sys.interactive then () else exit (Cmd.eval cmd)
