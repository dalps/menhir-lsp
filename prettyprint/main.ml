open Menhir_lsp_lib.Utils
open Menhirformat_lib.Utils

let version = "0.0.0"

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
    "The file to format, whose name must end with a `.mly' or `.mll' file \
     extension."
  in
  Arg.(value & pos 0 file "" & info [] ~docv:"FILE" ~doc)

let main ~config (input_file : string) =
  let open R in
  let open Menhirformat_lib in
  let res =
    match Filename.extension input_file with
    | ".mly" -> Menhir.format_file ~config input_file
    | ".mll" -> Ocamllex.format_file ~config input_file
    | _ ->
        log "%s: Unrecognized file extension: must be either '.mll' or '.mly'."
          input_file;
        exit 2
  in
  res
  |> R.map_err (fun (msg, rng) ->
      log "%s: Failed to format: at %a: %s" input_file Range.pp_lexing rng msg;
      exit 1)
  |> R.iter print_endline

let cmd =
  Cmd.v
    (Cmd.info "menhirformat" ~version
       ~doc:"A formatter for Menhir ocamllex .mll and .mly files.")
  @@ let+ input_file = input_file
     and+ tabsize = tabsize
     and+ indentOnce = indentOnce
     and+ noLeadingBar = noLeadingBar
     and+ maxWidth = maxColumns
     (* and+ breakLongRegexps = breakLongRegexps
     and+ breakRegexpsGroups = breakRegexpsGroups *)
     and+ semiAfterProducer = semiAfterProducer in
     let config =
       Config.make ~tabsize ~indentOnce ~noLeadingBar ~semiAfterProducer
         ~maxWidth ~breakLongRegexps:true ()
     in
     main ~config input_file

let () = if !Sys.interactive then () else exit (Cmd.eval cmd)
