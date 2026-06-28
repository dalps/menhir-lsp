rule main whitelist = parse
  | [' ' '\r' '\t' '\012']+ { main whitelist lexbuf }
  | '\n'
    {
      if List.mem '\n' whitelist then (
        Lexing.new_line lexbuf;
        main whitelist lexbuf)
      else failwith "forbidden newline"
    }
  | "#" [' ' '\t']* (['0'-'9']+) [' ' '\t']* ('"' ([^'\n' '\r' '"']*) '"')? [^'\n' '\r']* '\r'* '\n'
    { main whitelist lexbuf }
  | "(*" { comment 0 lexbuf; main whitelist lexbuf }
  | eof { () }
  | _ as c
    {
      if List.mem c whitelist then main whitelist lexbuf
      else failwith (Printf.sprintf "non-blank character '%c'" c)
    }

and comment depth = parse
  | "(*" { comment (depth + 1) lexbuf }
  | "*)" { if depth > 0 then comment (depth - 1) lexbuf }
  | eof { failwith "unterminated comment" }
  | '\n' { Lexing.new_line lexbuf; comment depth lexbuf }
  | _ { comment depth lexbuf }
