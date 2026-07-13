open Menhir_lsp_lib.Utils
open Menhirformat_lib.Utils

let version = "0.1.0"

open Cmdliner
open Cmdliner.Term.Syntax

let tabsize =
  let doc = "Whitespace unit for indentation." in
  Arg.(value & opt int 2 & info [ "t"; "tabsize" ] ~docv:"COLS" ~doc)

let maxColumns =
  let doc = "Maximum number of characters per line." in
  Arg.(value & opt int 80 & info [ "w"; "width" ] ~docv:"COLS" ~doc)

let noLeadingBar =
  let doc =
    "Omit the optional leading bar `|` in the first case of each rule."
  in
  Arg.(value & flag & info [ "no-leading-bar" ] ~doc)

let indentOnce =
  let doc =
    "Add a level of indentation to the cases of a rule or (default) keep them \
     flush with the start of the definition."
  in
  Arg.(value & flag & info [ "indent-once" ] ~doc)

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
    "The path to the file to format, preferably ending with the `.mly` or \
     `.mll` file extension, or `-` to read from stdin until an end-of-file is \
     reached."
  in
  Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc)

let lang =
  let doc =
    "Specify the syntax of the document. Useful only when FILE has no \
     extension or when reading from stdin."
  in
  (* Arg.(value & vflag `Mly [ (`Mly, info [ "mly" ]); (`Mll, info [ "mll" ])  ]) *)
  Arg.(
    value & opt string ""
    & info [ "lang" ] ~docv:"mll|mly" ~doc
        ~absent:"inferred from file extension")

let log s = Format.kasprintf (log "menhirformat: %s") s

(* Borrowed from menhir/lib/LexerUtil.ml *)
let pp_range out (startp, endp) =
  let open Lexing in
  if startp == dummy_pos || endp == dummy_pos then
    pf out "At an unknown location:\n"
  else
    let file = startp.pos_fname in
    let line = startp.pos_lnum in
    let char1 = startp.pos_cnum - startp.pos_bol + 1 in
    let char2 = endp.pos_cnum - startp.pos_bol + 1 in
    pf out "File %s, line %d, characters %d-%d" file line char1 char2

let main ?(lang = "") ~config (input_file : string) =
  let open R in
  let open Menhirformat_lib in
  if String.length input_file = 0 then (
    log "Please provide an input file or `-` to read input from stdin.";
    exit 2);
  let lang : [> `Mll | `Mly ] =
    match (Filename.extension input_file, lang) with
    | ".mll", _ | _, "mll" -> `Mll
    | ".mly", _ | _, "mly" -> `Mly
    | _ ->
        log "Unrecognized file extension: must be either `.mll` or `.mly`.";
        exit 2
  in
  let res =
    match (input_file, lang) with
    | "-", `Mll -> heredoc () |> Ocamllex.format_string ~config
    | "-", `Mly -> heredoc () |> Menhir.format_string ~config
    | _, `Mll -> Ocamllex.format_file ~config input_file
    | _, `Mly -> Menhir.format_file ~config input_file
  in
  res
  |> R.map_err (fun (msg, rng) ->
      log "Failed to format\n%a: %s" pp_range rng msg;
      exit 1)
  |> R.iter print_endline

let cmd =
  let help_secs =
    [
      `S Manpage.s_bugs;
      `P
        "Report bugs or request new features at \
         https://github.com/dalps/menhir-lsp/issues";
    ]
  in
  let man : Manpage.block list = [ `Blocks help_secs ] in
  Cmd.v
    (Cmd.info "menhirformat" ~version ~man
       ~doc:"A formatter for Menhir and ocamllex code.")
  @@ let+ input_file = input_file
     and+ tabsize = tabsize
     and+ indentOnce = indentOnce
     and+ noLeadingBar = noLeadingBar
     and+ maxWidth = maxColumns
     (* and+ breakLongRegexps = breakLongRegexps
     and+ breakRegexpsGroups = breakRegexpsGroups *)
     and+ semiAfterProducer = semiAfterProducer
     and+ lang = lang in
     let input_file =
       if input_file = "-" then "<standard input>" else input_file
     in
     let config =
       Config.make ~tabsize ~indentOnce ~noLeadingBar ~semiAfterProducer
         ~maxWidth ~breakLongRegexps:true ()
     in
     main ~lang ~config input_file

let () = if !Sys.interactive then () else exit (Cmd.eval cmd)
