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
  let doc = "Whitespace unit for indentation" in
  Arg.(value & opt int 2 & info [ "t"; "tabsize" ] ~docv:"COLS" ~doc)

let input_file =
  let doc = "The file to format, whose name must end with the `.mly' or `.mll' file extension." in
  Arg.(value & pos 0 file "" & info [] ~docv:"FILE" ~doc)

let main ~tabsize (input_file : string) =
  let open R in
  let text = read_file_contents input_file in
  let doc =
    TD.make ~position_encoding:`UTF8
      {
        textDocument =
          { languageId = ""; text; uri = Uri.of_path input_file; version = 0 };
      }
  in
  let config = Config.make ~tabsize in
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
       ~doc:"A formatter for Ocamllex and Menhir specifications.")
  @@ let+ input_file = input_file and+ tabsize = tabsize in
     main ~tabsize input_file

let () = if !Sys.interactive then () else exit (Cmd.eval cmd)
