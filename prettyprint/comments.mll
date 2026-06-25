rule main allow_newlines = parse
  | [' ' '\r' '\t' '\012']+ { main allow_newlines lexbuf }
  | '\n'
    {
      match allow_newlines with
      | `AllowNewlines ->
          Lexing.new_line lexbuf;
          main allow_newlines lexbuf
      | _ -> failwith "forbidden newline"
    }
  | "#" [' ' '\t']* (['0'-'9']+) [' ' '\t']* ('"' ([^'\n' '\r' '"']*) '"')? [^'\n' '\r']* '\r'* '\n'
    { main allow_newlines lexbuf }
  | "(*" { comment 0 lexbuf; main allow_newlines lexbuf }
  | eof { () }
  | _ as c { failwith (Printf.sprintf "non-blank character '%c'" c) }

and comment depth = parse
  | "(*" { comment (depth + 1) lexbuf }
  | "*)" { if depth > 0 then comment (depth - 1) lexbuf }
  | eof { failwith "unterminated comment" }
  | '\n' { Lexing.new_line lexbuf; comment depth lexbuf }
  | _ { comment depth lexbuf }
