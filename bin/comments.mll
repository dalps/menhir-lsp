rule main = parse
    [' ' '\013' '\009' '\012' ] +
    { main lexbuf }
  | '\010'
    { Lexing.new_line lexbuf;
      main lexbuf }
  | "#" [' ' '\t']* (['0'-'9']+) [' ' '\t']*
    ('\"' ([^ '\010' '\013' '\"']*) '\"')?
    [^ '\010' '\013']* '\013'* '\010'
    { main lexbuf }
  | "(*"
    { comment 0 lexbuf;
      main lexbuf }
  | eof { () }
  | _ { failwith "not a comment" }

and comment depth = parse
    "(*" { comment (depth + 1) lexbuf }
  | "*)" { if depth > 0 then comment (depth - 1) lexbuf }
  | eof
    { failwith "unterminated comment" }
  | '\010'
    { Lexing.new_line lexbuf;
      comment depth lexbuf }
  | _
    { comment depth lexbuf }
