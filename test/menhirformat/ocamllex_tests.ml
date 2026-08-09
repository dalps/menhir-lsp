open Menhirformat_lib
open Utils
open Config
module Mll = OcamllexSyntax
module MF = Ocamllex

let format, helper = get_test_helpers MF.format_string

let%expect_test "It handles escape sequences correctly" =
  helper
    {|rule next_token = parse
  | "\t" | "\ " {next_token lexbuf}
  | "\n" | '\n' | "\r"  {Lexing.new_line ()}
  | '\\'
  | "\\"                      { lexer_logger "\\"; Parser.LDIVIDE }
  | "\\" ("[a-z]" as c) { Printf.printf "got rare backslash sequence: \"\\%c\"\n" c}
|};
  [%expect
    {|
    rule next_token = parse
    | "\t" | " " { next_token lexbuf }
    | "\n" | '\n' | "\r" { Lexing.new_line () }
    | '\\' | "\\" { lexer_logger "\\"; Parser.LDIVIDE }
    | "\\" ("[a-z]" as c)
      { Printf.printf "got rare backslash sequence: \"\\%c\"\n" c }
    |}]

let items_demo =
  {|let green_series_item =
  "Green" as series (("Bed" | "Bench" | "Chair" | "Counter" | "Desk" | "Dresser" | "Lamp" | "Pantry" | "Shell" | "Table" | "Wall Clock" | "Wardrobe") as item)

let bedroom_item =
  ("Gorgeous" | "Sea-Anemone" | "Polka-Dot") as series ("Bed" as item)
  | ("Card" | "Gorgeous" | "Jingle" | "Polka-Dot" | "Regal" | "Pear") as series ("Dresser" as item)
  | ("Regal" | "Full-Moon") as series ("Vanity" as item)

rule scan_feng_shui_item = parse
| ("Alpinist" | "Blossoming" | "Dapper" | "Festivale" | "Festive-Tree"
   | "Chevron" | "Green Lace-Up" | "Lime") as line (("Dress" | "Hat"
                                                    | "Pants" | "Tank") as kind)
  { CLOTHING (line, typ) }
| green_series_item | ("Zodiac" as series) (("Goat" | "Snake" | "Tiger"
                                             | "Horse" | "Ox" | "Rabbit" | "Dragon") as item)
  | ("Golden" as series) (("Bed" | "Bench" | "Chair" | "Clock" | "Closet"
                           | "Dresser" | "Man" | "Screen" | "Table") as item)
  { FURNITURE (series, item) }
| ("Squat" as size) ("Nebuloid" as name) | (("Mega" | "Mini" | "Tall")? as size) (("Brewstoid" | "Buzzoid" | "Clankoid" | "Croakoid" | "Plinkoid" | "Quazoid" | "Sputnoid" | "Squelchoid") as name)
  { GYROID (name, size) }
| eof { EOF }
| _ { failwith "not a feng shui item" }

and green_item = parse green_series_item | "Leaf Bed" as g { g }|}

let%expect_test "Test breaking of alternations" =
  helper ~config:(Config.make ~indentOnce:true ()) items_demo;
  [%expect
    {|
    let green_series_item =
      "Green" as series (("Bed" | "Bench" | "Chair" | "Counter"
                          | "Desk" | "Dresser" | "Lamp" | "Pantry" | "Shell"
                          | "Table" | "Wall Clock" | "Wardrobe") as item)

    let bedroom_item =
      ("Gorgeous" | "Sea-Anemone" | "Polka-Dot") as series ("Bed" as item)
      | ("Card" | "Gorgeous" | "Jingle" | "Polka-Dot" | "Regal"
         | "Pear") as series ("Dresser" as item)
      | ("Regal" | "Full-Moon") as series ("Vanity" as item)

    rule scan_feng_shui_item = parse
      | ("Alpinist" | "Blossoming" | "Dapper" | "Festivale"
         | "Festive-Tree" | "Chevron" | "Green Lace-Up" | "Lime") as line (("Dress"
                                                                            | "Hat"
                                                                            | "Pants"
                                                                            | "Tank") as kind)
        { CLOTHING (line, typ) }
      | green_series_item
      | ("Zodiac" as series) (("Goat" | "Snake" | "Tiger" | "Horse"
                               | "Ox" | "Rabbit" | "Dragon") as item)
      | ("Golden" as series) (("Bed" | "Bench" | "Chair" | "Clock"
                               | "Closet" | "Dresser" | "Man" | "Screen" | "Table") as item)
        { FURNITURE (series, item) }
      | ("Squat" as size) ("Nebuloid" as name)
      | (("Mega" | "Mini" | "Tall")? as size) (("Brewstoid"
                                                | "Buzzoid" | "Clankoid"
                                                | "Croakoid" | "Plinkoid"
                                                | "Quazoid" | "Sputnoid"
                                                | "Squelchoid") as name)
        { GYROID (name, size) }
      | eof { EOF }
      | _ { failwith "not a feng shui item" }

    and green_item = parse
      | green_series_item | "Leaf Bed" as g { g }
    |}]

let long_regexp_demo =
  {|rule main allow_newlines = parse
      | [' ' '\r' '\t' '\012']+ { main allow_newlines lexbuf }
      | '\n'
      {
        match allow_newlines with
        | `AllowNewlines ->
            Lexing.new_line lexbuf;
            main allow_newlines lexbuf
        | _ -> failwith "newline"
      }
      | "#" [' ' '\t']* (['0'-'9']+) [' ' '\t']* ('"' ([^'\n' '\r' '"']*) '"')? [^'\n' '\r']* '\r'* '\n'
      { main allow_newlines lexbuf }
      | "(*" { comment 0 lexbuf; main allow_newlines lexbuf }
      | eof { () }
      | _ { failwith "not a comment" }

      and comment depth = parse
      | "(*" { comment (depth + 1) lexbuf }
      | "*)" { if depth > 0 then comment (depth - 1) lexbuf }
      | eof { failwith "unterminated comment" }
      | '\n' { Lexing.new_line lexbuf; comment depth lexbuf }
      | _ { comment depth lexbuf }
      |}

let%expect_test "Option [breakLongRegexps] works" =
  helper
    ~config:{ default_config with breakLongRegexps = true }
    long_regexp_demo;
  [%expect
    {|
    rule main allow_newlines = parse
    | [' ' '\r' '\t' '\012']+ { main allow_newlines lexbuf }
    | '\n'
      {
        match allow_newlines with
        | `AllowNewlines ->
            Lexing.new_line lexbuf;
            main allow_newlines lexbuf
        | _ -> failwith "newline"
      }
    | "#" [' ' '\t']* (['0'-'9']+) [' ' '\t']* ('"' ([^'\n' '\r' '"']*) '"')?
      [^'\n' '\r']* '\r'* '\n'
      { main allow_newlines lexbuf }
    | "(*" { comment 0 lexbuf; main allow_newlines lexbuf }
    | eof { () }
    | _ { failwith "not a comment" }

    and comment depth = parse
    | "(*" { comment (depth + 1) lexbuf }
    | "*)" { if depth > 0 then comment (depth - 1) lexbuf }
    | eof { failwith "unterminated comment" }
    | '\n' { Lexing.new_line lexbuf; comment depth lexbuf }
    | _ { comment depth lexbuf }
    |}]

let%expect_test "Option [breakRegexpGroups] works" =
  helper
    ~config:
      { default_config with breakLongRegexps = true; breakRegexpGroups = true }
    long_regexp_demo;
  [%expect
    {|
    rule main allow_newlines = parse
    | [' ' '\r' '\t' '\012']+ { main allow_newlines lexbuf }
    | '\n'
      {
        match allow_newlines with
        | `AllowNewlines ->
            Lexing.new_line lexbuf;
            main allow_newlines lexbuf
        | _ -> failwith "newline"
      }
    | "#" [' ' '\t']* (['0'-'9']+) [' ' '\t']* ('"' ([^'\n' '\r' '"']*)
      '"')? [^'\n' '\r']* '\r'* '\n'
      { main allow_newlines lexbuf }
    | "(*" { comment 0 lexbuf; main allow_newlines lexbuf }
    | eof { () }
    | _ { failwith "not a comment" }

    and comment depth = parse
    | "(*" { comment (depth + 1) lexbuf }
    | "*)" { if depth > 0 then comment (depth - 1) lexbuf }
    | eof { failwith "unterminated comment" }
    | '\n' { Lexing.new_line lexbuf; comment depth lexbuf }
    | _ { comment depth lexbuf }
    |}];
  helper
    ~config:
      { default_config with breakLongRegexps = false; breakRegexpGroups = true }
    long_regexp_demo;
  [%expect
    {|
    rule main allow_newlines = parse
    | [' ' '\r' '\t' '\012']+ { main allow_newlines lexbuf }
    | '\n'
      {
        match allow_newlines with
        | `AllowNewlines ->
            Lexing.new_line lexbuf;
            main allow_newlines lexbuf
        | _ -> failwith "newline"
      }
    | "#" [' ' '\t']* (['0'-'9']+) [' ' '\t']* ('"' ([^'\n' '\r' '"']*) '"')? [^'\n' '\r']* '\r'* '\n'
      { main allow_newlines lexbuf }
    | "(*" { comment 0 lexbuf; main allow_newlines lexbuf }
    | eof { () }
    | _ { failwith "not a comment" }

    and comment depth = parse
    | "(*" { comment (depth + 1) lexbuf }
    | "*)" { if depth > 0 then comment (depth - 1) lexbuf }
    | eof { failwith "unterminated comment" }
    | '\n' { Lexing.new_line lexbuf; comment depth lexbuf }
    | _ { comment depth lexbuf }
    |}]

let calc_lexer =
  {|{ open Calc  exception Error of string }

(* This rule looks for a single line, terminated with '\n' or eof.
   It returns a pair of an optional string (the line that was found)
   and a Boolean flag (false if eof was reached). *)

rule line = parse
  | ([^'\n']* '\n') as line
    (* Normal case: one line, no eof. *)
    { Some line, true }
  | eof
    (* Normal case: no data, eof. *)
    { None, false }
  | ([^'\n']+ as line) eof
    (* Special case: some data but missing '\n', then eof.
       Consider this as the last line, and add the missing '\n'. *)
    { Some (line ^ "\n"), false }

(* This rule analyzes a single line and turns it into a stream of tokens. *)

and token = parse
  | [' ' '\t'] { token lexbuf }
  | '\n' { EOL }
  | ['0'-'9']+ as i { INT (int_of_string i) }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | _
    {
      raise
        (Error
           (Printf.sprintf "At offset %d: unexpected character.\n"
              (Lexing.lexeme_start lexbuf)))
    }
  | eof
    {
      raise
        (Error
           (Printf.sprintf "At offset %d: unexpected end of input.\n"
              (Lexing.lexeme_start lexbuf)))
    }
|}

let%expect_test "Comments can sit on top of action blocks" =
  helper calc_lexer;
  [%expect
    {|
    { open Calc  exception Error of string }

    (* This rule looks for a single line, terminated with '\n' or eof.
       It returns a pair of an optional string (the line that was found)
       and a Boolean flag (false if eof was reached). *)

    rule line = parse
    | ([^'\n']* '\n') as line
      (* Normal case: one line, no eof. *)
      { Some line, true }
    | eof
      (* Normal case: no data, eof. *)
      { None, false }
    | ([^'\n']+ as line) eof
      (* Special case: some data but missing '\n', then eof.
           Consider this as the last line, and add the missing '\n'. *)
      { Some (line ^ "\n"), false }

    (* This rule analyzes a single line and turns it into a stream of tokens. *)

    and token = parse
    | [' ' '\t'] { token lexbuf }
    | '\n' { EOL }
    | ['0'-'9']+ as i { INT (int_of_string i) }
    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { TIMES }
    | '/' { DIV }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | _
      {
        raise
          (Error
             (Printf.sprintf "At offset %d: unexpected character.\n"
                (Lexing.lexeme_start lexbuf)))
      }
    | eof
      {
        raise
          (Error
             (Printf.sprintf "At offset %d: unexpected end of input.\n"
                (Lexing.lexeme_start lexbuf)))
      }
    |}]

let%test "Formatting of OCaml fragments is idempotent" =
  let input =
    {|{
  (* Takes a sized_basic_type and a list of sizes and repeatedly applies then
     SArray constructor, taking sizes off the list *)
  let reducearray (sbt, l) =
    List.fold_right l ~f:(fun z y -> SizedType.SArray (y, z)) ~init:sbt
}

rule foo = parse
| (* Program blocks *)
"functions"
  { (* A comment that says hi *)
    lexer_logger "functions"; Parser.FUNCTIONBLOCK }
| "foonctions"
  { (* A comment that says hello *)
    let pattern = () in
    let pattern = () in
    let pattern = () in ()
  }
|}
  in
  String.equal
    (input |> format |> format |> format |> format |> format)
    (format input)

let%expect_test "Comments can sit on top of rule cases, before the leading bar"
    =
  let input =
    {|rule foo = parse
(* Program blocks *)
  | "functions"               { lexer_logger "functions" ;
                                Parser.FUNCTIONBLOCK }|}
  in
  helper input;
  [%expect
    {|
    rule foo = parse
    (* Program blocks *)
    | "functions" { lexer_logger "functions"; Parser.FUNCTIONBLOCK }
    |}];
  helper ~config:{ default_config with indentOnce = true } input;
  [%expect
    {|
    rule foo = parse
      (* Program blocks *)
      | "functions" { lexer_logger "functions"; Parser.FUNCTIONBLOCK }
    |}]

let%expect_test
    "Gracefully fails on invalid OCaml code (`let = ref 0`, `$loc`) and skips \
     the whole block." =
  {|{
    open Parser
        let = ref 0
}

    rule foo = parse
(* Program blocks *)
  | "functions"               { lexer_logger "functions" ;
                                Parser.FUNCTIONBLOCK }
| "foonction"
  {
    let pattern =() in
                let pattern =() in
                let pattern =() in
                     ("functions", $loc, false)
  }|}
  |> format |> format |> format |> format |> helper;
  [%expect
    {|
    { open Parser
            let = ref 0 }

    rule foo = parse
    (* Program blocks *)
    | "functions" { lexer_logger "functions"; Parser.FUNCTIONBLOCK }
    | "foonction"
      {
        let pattern =() in
                    let pattern =() in
                    let pattern =() in
                         ("functions", $loc, false)
      }
    |}]

let%expect_test "Comments can sit on top of regexp alternations (sibling cases)"
    =
  {|rule skip_char = parse
  | '\\'? ('\013'* '\010') "'"
     { incr_loc lexbuf 1 }
  | [^ '\\' '\'' '\010' '\013'] "'" (* regular character *)
(* one character and numeric escape sequences *)
  | '\\' _ "'"
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
  | '\\' 'o' ['0'-'7'] ['0'-'7'] ['0'-'7'] "'"
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
     { () }
(* Perilous *)
  | "" { () }|}
  |> helper;
  [%expect
    {|
    rule skip_char = parse
    | '\\'? ('\013'* '\010') "'" { incr_loc lexbuf 1 }
    | [^'\\' '\'' '\010' '\013'] "'" (* regular character *)
    (* one character and numeric escape sequences *)
    | '\\' _ "'" | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
    | '\\' 'o' ['0'-'7'] ['0'-'7'] ['0'-'7'] "'"
    | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      { () }
    (* Perilous *)
    | "" { () }
    |}]

let%expect_test "It preserves byte escape sequences" =
  {|let crlf = "\r\n"

let whitespace = [' ' '\t']

(* Visual characters. *)
let vchar = _ # whitespace # ['\r' '\n']

let request_regexp =
  ['G' 'g'] ['E' 'e'] ['T' 't'] whitespace+ (_* vchar as url) whitespace* '\r'

let space = ' '

let digit = ['0'-'9']

let status_code = digit digit digit

let arbitrary_text = (space | '\t' | vchar)+

let http_version = "HTTP/1.1"

let status_line =
  http_version space (status_code as status_code) space (arbitrary_text as reason_phrase)? crlf

let field_line =
  ((vchar # ':')+ as field_name) whitespace* ':' whitespace* (arbitrary_text as field_value) crlf

rule read_response_token = parse
| crlf { Lexing.new_line lexbuf; log_res "CRLF"; CRLF }
| status_line
  {
    let reason_phrase = Option.value ~default:"" reason_phrase in
    log_res "STATUS_LINE \x1b[1;35m%s\x1b[0m \x1b[1;31m%s\x1b[0m" status_code
      reason_phrase;
    STATUS_LINE (int_of_string status_code, reason_phrase)
  }
| field_line
  {
    log_res "FIELD_LINE \x1b[1;34m%s:\x1b[0m \x1b[2m%S\x1b[0m" field_name field_value;
    FIELD_LINE (field_name, field_value)
  }

and read_response = parse
| status_line
  {
    let reason_phrase = Option.value ~default:"" reason_phrase in
    log_res "STATUS_LINE \x1b[1;35m%s\x1b[0m \x1b[1;31m%s\x1b[0m" status_code
      reason_phrase;
    Lexing.new_line lexbuf;
    let header = read_field_line lexbuf |> List.rev in
    let content_length = List.assoc "Content-Length" header |> int_of_string in
    let body = read_body_chars content_length lexbuf in
      ({
         status = int_of_string status_code;
         message = reason_phrase;
         header;
         body = !body_acc;
       }
        : Http_V1_types.response)
  }
| _ { failwith "A response should start with a status line." }

and read_bodyless_response = parse
| status_line
  {
    let reason_phrase = Option.value ~default:"" reason_phrase in
    log_res "STATUS_LINE \x1b[1;35m%s\x1b[0m \x1b[1;31m%s\x1b[0m" status_code
      reason_phrase;
    Lexing.new_line lexbuf;
    let header = read_field_line lexbuf |> List.rev in
    let content_length = List.assoc "Content-Length" header in
    (int_of_string status_code, reason_phrase,header)
  }
| _ { failwith "A response should start with a status line." }

and read_field_line = parse
| field_line
  {
    Lexing.new_line lexbuf;
    log_res "FIELD_LINE \x1b[1;34m%s:\x1b[0m \x1b[2m%S\x1b[0m" field_name field_value;
    (field_name, field_value) :: read_field_line lexbuf
  }
| crlf { Lexing.new_line lexbuf; log_res "CRLF"; [] }

and read_body_chars len = parse
| _ as s {
  (* log_res "Character #%-3d: %c \x1b[2;37m%3d\x1b[0m" len s (Char.code s); *)
  body_acc := !body_acc ^ Char.escaped s;
  if len > 1 then read_body_chars (len - 1) lexbuf
  else log_res "BODY <..>" }
  | eof { () }|}
  |> format |> format |> helper;
  [%expect
    {|
    let crlf = "\r\n"

    let whitespace = [' ' '\t']

    (* Visual characters. *)
    let vchar = _ # whitespace # ['\r' '\n']

    let request_regexp =
      ['G' 'g'] ['E' 'e'] ['T' 't'] whitespace+ (_* vchar as url) whitespace* '\r'

    let space = ' '

    let digit = ['0'-'9']

    let status_code = digit digit digit

    let arbitrary_text = (space | '\t' | vchar)+

    let http_version = "HTTP/1.1"

    let status_line =
      http_version space (status_code as status_code) space (arbitrary_text as reason_phrase)? crlf

    let field_line =
      ((vchar # ':')+ as field_name) whitespace* ':' whitespace* (arbitrary_text as field_value) crlf

    rule read_response_token = parse
    | crlf { Lexing.new_line lexbuf; log_res "CRLF"; CRLF }
    | status_line
      {
        let reason_phrase = Option.value ~default:"" reason_phrase in
        log_res "STATUS_LINE \x1b[1;35m%s\x1b[0m \x1b[1;31m%s\x1b[0m" status_code
          reason_phrase;
        STATUS_LINE (int_of_string status_code, reason_phrase)
      }
    | field_line
      {
        log_res "FIELD_LINE \x1b[1;34m%s:\x1b[0m \x1b[2m%S\x1b[0m" field_name
          field_value;
        FIELD_LINE (field_name, field_value)
      }

    and read_response = parse
    | status_line
      {
        let reason_phrase = Option.value ~default:"" reason_phrase in
        log_res "STATUS_LINE \x1b[1;35m%s\x1b[0m \x1b[1;31m%s\x1b[0m" status_code
          reason_phrase;
        Lexing.new_line lexbuf;
        let header = read_field_line lexbuf |> List.rev in
        let content_length = List.assoc "Content-Length" header |> int_of_string in
        let body = read_body_chars content_length lexbuf in
        ({
           status = int_of_string status_code;
           message = reason_phrase;
           header;
           body = !body_acc;
         }
          : Http_V1_types.response)
      }
    | _ { failwith "A response should start with a status line." }

    and read_bodyless_response = parse
    | status_line
      {
        let reason_phrase = Option.value ~default:"" reason_phrase in
        log_res "STATUS_LINE \x1b[1;35m%s\x1b[0m \x1b[1;31m%s\x1b[0m" status_code
          reason_phrase;
        Lexing.new_line lexbuf;
        let header = read_field_line lexbuf |> List.rev in
        let content_length = List.assoc "Content-Length" header in
        (int_of_string status_code, reason_phrase, header)
      }
    | _ { failwith "A response should start with a status line." }

    and read_field_line = parse
    | field_line
      {
        Lexing.new_line lexbuf;
        log_res "FIELD_LINE \x1b[1;34m%s:\x1b[0m \x1b[2m%S\x1b[0m" field_name
          field_value;
        (field_name, field_value) :: read_field_line lexbuf
      }
    | crlf { Lexing.new_line lexbuf; log_res "CRLF"; [] }

    and read_body_chars len = parse
    | _ as s
      {
        (* log_res Character #%-3d: %c \x1b[2;37m%3d\x1b[0m len s (Char.code s); *)
        body_acc := !body_acc ^ Char.escaped s;
        if len > 1 then read_body_chars (len - 1) lexbuf else log_res "BODY <..>"
      }
    | eof { () }
    |}]

let%expect_test "It preserves escapes codes used in various contexts" =
  {|let ident = 'x' | 'y'

rule pattern = parse '\x1b' { "escape \x1b = \027 \h", '\x1b', '\h' (* \x1b *) } | ident { "ident 🤔" } | 'k' { "\u{0138}" }|}
  |> helper;
  [%expect
    {|
    let ident = 'x' | 'y'

    rule pattern = parse
    | '\x1b' { "escape \x1b = \027 \h", '\x1b', '\h' (* \x1b *) }
    | ident { "ident 🤔" }
    | 'k' { "\u{0138}" }
    |}]

let%expect_test "It correctly separates cases in one-liner rules" =
  {|let ident = 'x' | 'y'

rule pattern = parse '\x1b' { ESCAPE } | ident { IDENT } | 'k' { K }|}
  |> helper ~config:{ default_config with noLeadingBar = true };
  [%expect
    {|
    let ident = 'x' | 'y'

    rule pattern = parse
      '\x1b' { ESCAPE }
    | ident { IDENT }
    | 'k' { K }
    |}]
