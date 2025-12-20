let load_grammar_from_contents priority filename content =
  InputFile.with_file_content filename content @@ fun lexbuf ->
  let grammar = Driver.parse priority lexbuf in
  { grammar with pg_filename = filename }

let load_grammar_from_file filename : Syntax.partial_grammar =
  try
    let priority = 1 and contents = IO.read_whole_file filename in
    load_grammar_from_contents priority filename contents
  with Sys_error msg -> Report.Just.error [] "%s" msg
