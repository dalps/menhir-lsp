open Menhir_lsp_lib.Utils

let version = "0.0.0"
let input_file = ref ""

let print_version () =
  prerr_endline version;
  exit 0

let read_file_contents filename =
  let ic = open_in filename in
  let contents = really_input_string ic (in_channel_length ic) in
  close_in ic;
  contents

let () =
  Arg.parse
    [ ("-v", Unit print_version, "Print the version of menhirformat") ]
    (fun anon -> input_file := anon)
    "Usage: menhirformat [INPUT_FILE]";

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
      let open Menhirformat_lib.Menhir in
      let open MenhirSyntax in
      match MenhirSyntax.Main.load_grammar_from_file !input_file with
      | Ok ast ->
          let buf = Buffer.create 80 in
          let bag_of_comments = init_bag (Lexer.get_comments ()) in
          let attach_vtor =
            object
              inherit [_] Syntax.ast_endo
              method! visit_located = visit_attach ~bag_of_comments ~doc
            end
          in
          attach_comments ast (attach_vtor#visit_main ()) ~bag_of_comments ~doc
          |> (new formatter)#visit_main ()
          |> PPrint.ToBuffer.pretty 0.8 80 buf;
          print_endline @@ Buffer.contents buf
      | Error _ -> ())
  | ".mll" -> (
      let open Menhirformat_lib.Ocamllex in
      let open OcamllexSyntax in
      match OcamllexSyntax.Main.parse_string text with
      | Ok ast ->
          let buf = Buffer.create 80 in
          let bag_of_comments = Lexer.get_comments () |> init_bag in
          let attach_vtor =
            object
              inherit [_] Syntax.syntax_endo
              method! visit_located = visit_attach ~bag_of_comments ~doc
            end
          in
          attach_comments ast (attach_vtor#visit_main ()) ~bag_of_comments ~doc
          |> (new formatter)#visit_main ()
          |> PPrint.ToBuffer.pretty 0.8 80 buf;
          print_endline @@ Buffer.contents buf
      | Error _ -> ())
  | _ -> prerr_endline "Gimme something to work with now :("
