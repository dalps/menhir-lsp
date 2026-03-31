open Menhir_lsp_lib.Utils

let version = "0.0.0"
let input_file = ref ""

let log s = Format.ksprintf (prerr_endline) s

let print_version () =
  log "%s" version;
  exit 0

let read_file_contents filename =
  try
    let ic = open_in filename in
    let contents = really_input_string ic (in_channel_length ic) in
    close_in ic;
    contents
  with _ ->
    log "File doesn't exist: %s" filename;
    exit 1

let () =
  Arg.parse
    [ ("-v", Unit print_version, "Print the version of menhirformat") ]
    (fun anon -> input_file := anon)
    {|A formatter for Ocamllex and Menhir specifications.

Usage: menhirformat [INPUT_FILE]|};

  let text = read_file_contents !input_file in
  let doc =
    TD.make ~position_encoding:`UTF8
      {
        textDocument =
          { languageId = ""; text; uri = Uri.of_path !input_file; version = 0 };
      }
  in

  let open R in
  match Filename.extension !input_file with
  | ".mly" -> (
      match MenhirSyntax.Main.load_grammar_from_file !input_file with
      | Ok ast -> Menhirformat_lib.Menhir.main ~doc ~ast |> print_endline
      | Error _ -> ())
  | ".mll" -> (
      match OcamllexSyntax.Main.parse_string text with
      | Ok ast -> Menhirformat_lib.Ocamllex.main ~doc ~ast |> print_endline
      | Error _ -> ())
  | _ ->
      log "%s"
        "Unrecognized file extension: must be '.mll' or '.mly'.";
      exit 2
