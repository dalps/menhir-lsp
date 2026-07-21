(* The entry point. *)
Printexc.record_backtrace true;;

let parse_string string =
  let lexbuf = Lexing.from_string string in
  let lexer = Lexer.main in

  Parser.grammar lexer lexbuf

let parse_file filename =
  let inchan = open_in filename in
  (* let lexbuf = Lexing.from_channel inchan in *)
  let lexer = Lexer.main in

  let content = really_input_string inchan (in_channel_length inchan) in
  Printf.printf "%s\n" content;

  InputFile.with_file_content filename content (fun lexbuf ->
      try
        let ast = Parser.grammar lexer lexbuf in
        close_in inchan;
        Some ast
      with _ ->
        Printexc.print_backtrace stdout;
        None)

(* ---------------------- *)

(* The entry point. *)

let parse priority lexbuf :
    (Syntax.partial_grammar, string * Range.range) result =
  Lexer.priority := priority;
  let module E = MenhirLib.ErrorReports in
  let buffer, lexer = E.wrap Lexer.main in
  try Ok (Parser.grammar lexer lexbuf)
  with exn ->
    let range =
      match exn with
      | Parser.Error state ->
          Range.make Lexing.(lexeme_start_p lexbuf, lexeme_end_p lexbuf)
      | ParserAux.ParserError loc | Lexer.LexerError loc -> Located.position loc
      | _ -> Range.make Lexing.(dummy_pos, dummy_pos)
    in
    let message =
      match exn with
      | Parser.Error state ->
          Printf.sprintf "Syntax error %s: %s"
            (* Responsible for `after '%s' and before '%s'` message *)
            (E.show InputFile.chunk buffer)
            (ParserMessages.message state)
      | ParserAux.ParserError loc | Lexer.LexerError loc ->
          Printf.sprintf "Error: %s" (Located.value loc)
      | _ -> "???"
    in
    Error (message, range)
