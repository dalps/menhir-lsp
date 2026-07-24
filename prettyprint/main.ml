module OcamlLocation = Location
open Menhir_lsp_lib.Utils
open Menhirformat_lib.Utils

let version = "0.1.0"
let standard_input = "<standard input>"

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
     `.mll` file extension, or `-` to read from stdin until end-of-file is \
     reached."
  in
  Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc)

let lang =
  let doc =
    "Specify the language of the source code when FILE has no extension or \
     when reading from stdin."
  in
  (* Arg.(value & vflag `Mly [ (`Mly, info [ "mly" ]); (`Mll, info [ "mll" ])  ]) *)
  Arg.(
    value
    & opt
        (Arg.Conv.make ~docv:"LANG"
           ~pp:(fun out lang ->
             pf out "%s"
               (match lang with
               | Some `Mll -> "mll"
               | Some `Mly -> "mly"
               | None -> ""))
           ~parser:(function
             | "mll" -> Ok (Some `Mll)
             | "mly" -> Ok (Some `Mly)
             | _ -> Error "Must be 'mll' or 'mly'")
           ())
        None
    & info [ "lang" ] ~docv:"mll|mly" ~doc
        ~absent:"inferred from file extension")

let error s = Format.eprintf ("menhirformat: " ^^ s ^^ "\n%!")

let main ~config lang (input_file : [> `File of string | `Stdin ]) =
  let open R in
  let open Menhirformat_lib in
  let lang : [ `Mll | `Mly ] =
    match input_file with
    | `Stdin -> (
        (* OcamlLocation.input_name := standard_input; *)
        match lang with
        | None ->
            error
              "The language mode must be specified via the \
               '--lang' option when reading from standard input.";
            exit 2
        | Some lang -> lang)
    | `File input_file -> (
        match (Filename.extension input_file, lang) with
        | ".mll", _ -> `Mll
        | ".mly", _ -> `Mly
        | _, Some lang -> lang
        | _ ->
            error
              "Could not determine the language mode from the file extension. \
               Please specify it via the '--lang' option.";
            exit 2)
  in
  let filename, res =
    match (input_file, lang) with
    | `Stdin, `Mll ->
        (standard_input, heredoc () |> Ocamllex.format_string ~config)
    | `Stdin, `Mly ->
        (standard_input, heredoc () |> Menhir.format_string ~config)
    | `File name, `Mll -> (name, Ocamllex.format_file ~config name)
    | `File name, `Mly -> (name, Menhir.format_file ~config name)
  in
  res
  |> R.map_err (fun (msg, (loc_start, loc_end)) ->
      let loc = Warnings.{ loc_start; loc_end; loc_ghost = false } in
      let report : OcamlLocation.report = OcamlLocation.errorf ~loc "%s" msg in
      error "ignoring %S (syntax error)" filename;
      epr "%a" OcamlLocation.print_report report;
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
     if String.length input_file = 0 then (
       error
         "Please specify the input file, or alternatively '-' to read from the \
          standard input.";
       exit 2);
     let input_file = if input_file = "-" then `Stdin else `File input_file in
     let config =
       Config.make ~tabsize ~indentOnce ~noLeadingBar ~semiAfterProducer
         ~maxWidth ~breakLongRegexps:true ()
     in
     main ~config lang input_file

let () = if !Sys.interactive then () else exit (Cmd.eval cmd)
