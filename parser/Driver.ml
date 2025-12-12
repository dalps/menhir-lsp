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

let parse priority lexbuf =
  Lexer.priority := priority;
  let lexer = Lexer.main in

  Parser.grammar lexer lexbuf
