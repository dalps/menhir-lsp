open Menhirformat_lib
open Utils
open Config
module Mll = OcamllexSyntax
module MF = Ocamllex

let format, helper = get_test_helpers Mll.Main.parse_string MF.main

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
  [%expect]

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

let%expect_test "Comments behave well around lexer cases" =
  helper
    {|rule foo = parse
(* Program blocks *)
  | "functions"               { lexer_logger "functions" ;
                                Parser.FUNCTIONBLOCK }|};
  [%expect]
