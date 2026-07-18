let load_grammar_from_contents priority filename content =
  InputFile.with_file_content filename content @@ fun lexbuf ->
  Location.input_name := filename;
  Location.input_lexbuf := Some lexbuf;
  Lexer.init ();
  Driver.parse priority lexbuf
  |> Result.map (fun grammar -> Syntax.{ grammar with pg_filename = filename })

let load_grammar_from_file filename =
  try
    let priority = 1 and contents = IO.read_whole_file filename in
    load_grammar_from_contents priority filename contents
  with Sys_error msg -> Report.Just.error [] "%s" msg
