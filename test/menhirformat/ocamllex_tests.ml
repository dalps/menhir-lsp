open Menhirformat_lib
open Utils
open Config
module Mll = OcamllexSyntax
module MF = Ocamllex

let helper ?(config : Config.t = Config.default_config) text : unit =
  text |> Mll.Main.parse_string
  |> Result.fold
       ~ok:(fun partial_grammar ->
         MF.main ~config ~ast:partial_grammar ~doc:(doc_of_string text))
       ~error:(fun (msg, range) -> spr "%s at %s" msg (Mll.Range.show range))
  |> print_endline

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
  { CLOTHING (line, type) }
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
      "Green" as series (("Bed" | "Bench" | "Chair" | "Counter" | "Desk"
                          | "Dresser" | "Lamp" | "Pantry" | "Shell" | "Table" | "Wall Clock"
                          | "Wardrobe") as item)

    let bedroom_item =
      ("Gorgeous" | "Sea-Anemone" | "Polka-Dot") as series ("Bed" as item)
      | ("Card" | "Gorgeous" | "Jingle" | "Polka-Dot" | "Regal" | "Pear") as series ("Dresser" as item)
      | ("Regal" | "Full-Moon") as series ("Vanity" as item)

    rule scan_feng_shui_item = parse
      | ("Alpinist" | "Blossoming" | "Dapper" | "Festivale" | "Festive-Tree"
         | "Chevron" | "Green Lace-Up" | "Lime") as line (("Dress" | "Hat"
                                                           | "Pants" | "Tank") as kind)
        { CLOTHING (line, type) }
      | green_series_item | ("Zodiac" as series) (("Goat" | "Snake" | "Tiger"
                                                   | "Horse" | "Ox" | "Rabbit" | "Dragon") as item)
      | ("Golden" as series) (("Bed" | "Bench" | "Chair" | "Clock" | "Closet"
                               | "Dresser" | "Man" | "Screen" | "Table") as item)
        { FURNITURE (series, item) }
      | ("Squat" as size) ("Nebuloid" as name) | (("Mega" | "Mini" | "Tall")? as size) (("Brewstoid"
                                                                                         | "Buzzoid"
                                                                                         | "Clankoid"
                                                                                         | "Croakoid"
                                                                                         | "Plinkoid"
                                                                                         | "Quazoid"
                                                                                         | "Sputnoid"
                                                                                         | "Squelchoid") as name)
        { GYROID (name, size) }
      | eof { EOF }
      | _ { failwith "not a feng shui item" }

    and green_item = parse green_series_item | "Leaf Bed" as g { g }
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
