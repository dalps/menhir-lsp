(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The lexical analyzer for lexer definitions. Bootstrapped! *)

{
open Parser
open Located

(* Auxiliaries for the lexical analyzer *)

exception Lexical_error of string Located.located

let get_range lexbuf =
  Range.make Lexing.(lexeme_start_p lexbuf, lexeme_end_p lexbuf)

let char_for_backslash = function
    'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let raise exn =
  Hashtbl.reset Syntax.named_regexps; (* [menhir-lsp] *)
  Stdlib.raise exn

let raise_lexical_error lexbuf msg =
  raise (Lexical_error (Located.locate (get_range lexbuf) msg))

let handle_lexical_error fn arg lexbuf = fn arg lexbuf

let warning lexbuf msg =
  let p = Lexing.lexeme_start_p lexbuf in
  Printf.eprintf "ocamllex warning:\nFile \"%s\", line %d, character %d: %s.\n"
    p.Lexing.pos_fname p.Lexing.pos_lnum
    (p.Lexing.pos_cnum - p.Lexing.pos_bol + 1) msg;
  flush stderr

let hex_digit_value d =
  let d = Char.code d in
  if d >= 97 then d - 87 else
  if d >= 65 then d - 55 else
  d - 48

let decimal_code c d u =
  100 * (Char.code c - 48) + 10 * (Char.code d - 48) + (Char.code u - 48)

let hexadecimal_code s =
  let rec loop acc i =
    if i < String.length s then
      let value = hex_digit_value s.[i] in
      loop (16 * acc + value) (i + 1)
    else acc in
  loop 0 0

let char_for_octal_code c d u =
  let c = 64 * (Char.code c - 48) +
           8 * (Char.code d - 48) +
               (Char.code u - 48) in
  Char.chr c

let char_for_hexadecimal_code d u =
  Char.chr (16 * (hex_digit_value d) + (hex_digit_value u))

let incr_loc lexbuf delta =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum - delta;
  }

let update_loc lexbuf opt_file line =
  let pos = lexbuf.Lexing.lex_curr_p in
  let new_file = match opt_file with
                 | None -> pos.Lexing.pos_fname
                 | Some f -> f
  in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_fname = new_file;
    Lexing.pos_lnum = line;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  }

type string_context = Pattern | Action | Comment

let spr = Printf.sprintf

(* ---------------------------------------------------------- 
  Object to buffer string literals, comments and actions.
  Adapted from ocamlformat.0.28.1/vendor/parser-extended/lexer.mll 
*)

class lexeme_buffer = object (self)
  val string_buffer = Buffer.create 256

  (* To store the position of the beginning of a string and comment *)
  val string_start_loc = ref Lexing.dummy_pos

  method set_start_loc lexbuf = string_start_loc := Lexing.lexeme_start_p lexbuf

  method reset_string_buffer () = Buffer.reset string_buffer
  method get_stored_string () = Buffer.contents string_buffer

  method store_string_char c = Buffer.add_char string_buffer c
  method store_string_utf_8_uchar u = Buffer.add_utf_8_uchar string_buffer u
  method store_string s = Buffer.add_string string_buffer s
  method store_substring s ~pos ~len = Buffer.add_substring string_buffer s pos len

  method store_lexeme lexbuf = self#store_string (Lexing.lexeme lexbuf)

  method store_normalized_newline newline =
    (* OCamlformat: We preserve the line endings in string literals. *)
    self#store_string newline
end

let string_buffer = new lexeme_buffer
let action_buffer = new lexeme_buffer
let comment_buffer = new lexeme_buffer

(* [menhir-lsp] We want to collect and expose the comments recorded during lexing. 

In the Document printer, we'll use [PPrint.range] combinator to
locate each comment in the formatted document. We'll search for the
smallest CST node that contains the comment's range and map it to a
TextEdit that inserts the comment text above the node's.
*)
type comment = string located

let last_comment = ref None

let _get_last_comment = let c = !last_comment in last_comment := None; c

let comments : comment list ref = ref []

let store_comment c = 
  last_comment := Some c;
  comments := c :: !comments
  
let get_comments () = List.rev !comments

let init () =
  comments := [];
  string_buffer#reset_string_buffer ();
  action_buffer#reset_string_buffer ();
  comment_buffer#reset_string_buffer ()

let tchar lexbuf code =
  let repr = Lexing.lexeme lexbuf in
  Tchar {repr; code}
}

let identstart =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255']
let identbody =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let backslash_escapes =
  ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']

let lowercase = ['a'-'z' '_']
let ident = identstart identbody*
let extattrident = ident ('.' ident)*
let blank = [' ' '\009' '\012']

let uppercase = ['A'-'Z']
let ocaml_identstart = lowercase | uppercase
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let utf8 = ['\192'-'\255'] ['\128'-'\191']*
let identstart_ext = ocaml_identstart | utf8
let identchar_ext = identchar | utf8
let ocaml_ident = identstart_ext identchar_ext*

rule main = parse
    [' ' '\013' '\009' '\012' ] +
    { main lexbuf }
  | '\010'
    { incr_loc lexbuf 0;
      main lexbuf }
  | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
    ('\"' ([^ '\010' '\013' '\"']* as name) '\"')?
    [^ '\010' '\013']* '\013'* '\010'
    { update_loc lexbuf name (int_of_string num);
      main lexbuf
    }
  | "(*"
    { comment_buffer#reset_string_buffer ();
      comment_buffer#store_lexeme lexbuf;
      let startp = Lexing.lexeme_start_p lexbuf in
      let endp = handle_lexical_error comment 0 lexbuf in
      let c = comment_buffer#get_stored_string () in
      store_comment (locate (startp, endp) c);
      comment_buffer#reset_string_buffer ();
      main lexbuf }
  | '_' { Tunderscore }
  | ident
    { match Lexing.lexeme lexbuf with
        "rule" -> Trule
      | "parse" -> Tparse
      | "shortest" -> Tparse_shortest
      | "and" -> Tand
      | "eof" -> Teof
      | "let" -> Tlet
      | "as"  -> Tas
      | "refill" -> Trefill
      | s -> Tident s }
  | '"'
    { string_buffer#reset_string_buffer ();
      let startp = Lexing.lexeme_start_p lexbuf in
      let endp = handle_lexical_error string Pattern lexbuf in
      let content = string_buffer#get_stored_string () in
      Tstring (Located.locate (startp, endp) content) } (* [menhir-lsp] located. *)
(* note: ''' is a valid character literal (by contrast with the compiler) *)
  | "'" [^ '\\'] "'"
    { tchar lexbuf (Char.code(Lexing.lexeme_char lexbuf 1)) }
  | "'" '\\' backslash_escapes "'"
    { tchar lexbuf (Char.code(char_for_backslash (Lexing.lexeme_char lexbuf 2))) }
  | "'" '\\' (['0'-'9'] as c) (['0'-'9'] as d) (['0'-'9'] as u)"'"
    { let v = decimal_code c d u in
      if v > 255 then
        raise_lexical_error lexbuf
          (Printf.sprintf "illegal escape sequence \\%c%c%c" c d u)
      else
        tchar lexbuf v }
  | "'" '\\' 'o' (['0'-'3'] as c) (['0'-'7'] as d) (['0'-'7'] as u) "'"
    { tchar lexbuf (Char.code(char_for_octal_code c d u)) }
  | "'" '\\' 'x'
       (['0'-'9' 'a'-'f' 'A'-'F'] as d) (['0'-'9' 'a'-'f' 'A'-'F'] as u) "'"
       { tchar lexbuf (Char.code(char_for_hexadecimal_code d u)) }
  | "'" '\\' (_ as c)
    { raise_lexical_error lexbuf
        (Printf.sprintf "illegal escape sequence \\%c" c)
    }
  | '{'
    { action_buffer#reset_string_buffer ();
      let startp = Lexing.lexeme_end_p lexbuf in (* [menhir-lsp] The content starts right after the brace. *)
      let endp = handle_lexical_error action [] lexbuf in
      let content = action_buffer#get_stored_string () in
      action_buffer#reset_string_buffer ();
      Taction (locate (startp, endp) content) }
  | '='  { Tequal }
  | '|'  { Tor }
  | '['  { Tlbracket }
  | ']'  { Trbracket }
  | '*'  { Tstar }
  | '?'  { Tmaybe }
  | '+'  { Tplus }
  | '('  { Tlparen }
  | ')'  { Trparen }
  | '^'  { Tcaret }
  | '-'  { Tdash }
  | '#'  { Thash }
  | eof  { Tend }
  | _
    { raise_lexical_error lexbuf
        ("illegal character " ^ String.escaped(Lexing.lexeme lexbuf))
    }


(* String parsing comes from the compiler lexer *)
and string in_pattern = parse
    '"'
    { Lexing.lexeme_end_p lexbuf } (* [menhir-lsp] return pos instead of () *)
  | '\\' ('\013'* '\010') ([' ' '\009'] * as spaces)
    { incr_loc lexbuf (String.length spaces);
      string in_pattern lexbuf }
  | '\\' (backslash_escapes as c)
    { (* [menhir-lsp] spaces can but don't need to be escaped. *)
      if c = ' ' then
        string_buffer#store_string_char (char_for_backslash c)
      else
        string_buffer#store_lexeme lexbuf;
      string in_pattern lexbuf }
  | '\\' (['0'-'9'] as c) (['0'-'9'] as d) (['0'-'9']  as u)
    { let v = decimal_code c d u in
      if in_pattern = Pattern then
        if v > 255 then
          raise_lexical_error lexbuf
            (Printf.sprintf
              "illegal backslash escape in string: '\\%c%c%c'" c d u);
        (* else
          string_buffer#store_string_char (Char.chr v); *)
      string_buffer#store_lexeme lexbuf;
      string in_pattern lexbuf }
  | '\\' 'o' (['0'-'3'] as _c) (['0'-'7'] as _d) (['0'-'7'] as _u)
    { string_buffer#store_lexeme lexbuf;
      string in_pattern lexbuf }
  | '\\' 'x' (['0'-'9' 'a'-'f' 'A'-'F'] as _d) (['0'-'9' 'a'-'f' 'A'-'F'] as _u)
    { 
      (* string_buffer#store_string_char (char_for_hexadecimal_code d u) ; *)
      string_buffer#store_lexeme lexbuf;
      string in_pattern lexbuf }
  | '\\' 'u' '{' (['0'-'9' 'a'-'f' 'A'-'F'] + as s) '}'
    { let v = hexadecimal_code s in
      if in_pattern = Pattern then
        if not (Uchar.is_valid v) then
          raise_lexical_error lexbuf
            (Printf.sprintf
              "illegal uchar escape in string: '\\u{%s}'" s);
        (* else
          string_buffer#store_string_utf_8_uchar (Uchar.unsafe_of_int v); *)
      string_buffer#store_lexeme lexbuf;
      string in_pattern lexbuf }
  | '\\' (_ as c)
    { if in_pattern = Pattern then
        warning lexbuf
          (Printf.sprintf "illegal backslash escape in string: '\\%c'" c) ;
      (* string_buffer#store_string_char '\\' ;
      string_buffer#store_string_char c ; *)
      string_buffer#store_lexeme lexbuf;
      string in_pattern lexbuf }
  | eof
    { raise_lexical_error lexbuf "unterminated string" }
  | '\013'* '\010' as s
    { if in_pattern <> Comment then
        warning lexbuf (Printf.sprintf "unescaped newline in string") ;
      string_buffer#store_string s;
      incr_loc lexbuf 0;
      string in_pattern lexbuf }
  | _ as c
    { string_buffer#store_string_char c;
      string in_pattern lexbuf }

and quoted_string delim buffer = parse
  | '\013'* '\010'
    { incr_loc lexbuf 0;
      buffer#store_lexeme lexbuf;
      quoted_string delim buffer lexbuf }
  | eof
    { raise_lexical_error lexbuf "unterminated string" }
  | '|' (lowercase* as delim') '}'
    { buffer#store_lexeme lexbuf;
      if delim <> delim' then
      quoted_string delim buffer lexbuf }
  | _
    { buffer#store_lexeme lexbuf;
      quoted_string delim buffer lexbuf }

(*
   Lexers comment and action are quite similar.
   They should lex strings, quoted strings and characters,
   in order not to be confused by what is inside them.
*)

and comment depth = parse
    "(*" { comment_buffer#store_lexeme lexbuf;
           comment (depth + 1) lexbuf }
  | "*)" { comment_buffer#store_lexeme lexbuf;
           if depth > 0 then comment (depth - 1) lexbuf 
           else Lexing.lexeme_end_p lexbuf }
  | '"'
    { string_buffer#reset_string_buffer();
      string Comment lexbuf |> ignore;
      comment_buffer#store_string (string_buffer#get_stored_string ());
      string_buffer#reset_string_buffer();
      comment depth lexbuf }
  | '{' ('%' '%'? extattrident blank*)? (lowercase* as delim) "|"
    { comment_buffer#store_lexeme lexbuf;
      quoted_string delim comment_buffer lexbuf;
      comment depth lexbuf }
  | "'"
    { comment_buffer#store_lexeme lexbuf; (* The left quote. *)
      skip_char lexbuf ;
      comment_buffer#store_lexeme lexbuf; (* The char and right quote. *)
      comment depth lexbuf }
  | eof
    { raise_lexical_error lexbuf "unterminated comment" }
  | '\010'
    { incr_loc lexbuf 0;
      comment_buffer#store_lexeme lexbuf;
      comment depth lexbuf }
  | ocaml_ident
    { comment_buffer#store_lexeme lexbuf;
      comment depth lexbuf }
  | _
    { comment_buffer#store_lexeme lexbuf;
      comment depth lexbuf }

and action stk = parse
  | '(' { action_buffer#store_lexeme lexbuf; action ('(' :: stk) lexbuf }
  | '{' { action_buffer#store_lexeme lexbuf; action ('{' :: stk) lexbuf }
  | ')'
    { match stk with
      | '(' :: stk' -> action_buffer#store_lexeme lexbuf; action stk' lexbuf
      | _ -> raise_lexical_error lexbuf "Unmatched ) in action" }
  | '}'
    { match stk with
      | [] -> Lexing.lexeme_start_p lexbuf (* ... |} *)
      | '{' :: stk' ->
        action_buffer#store_lexeme lexbuf;
        action stk' lexbuf
      | _ -> raise_lexical_error lexbuf "Unmatched } in action" }
  | '"'
    { string_buffer#reset_string_buffer ();
      let _ = handle_lexical_error string Action lexbuf in
      let content = string_buffer#get_stored_string () in 
      action_buffer#store_string @@ spr "\"%s\"" content;
      string_buffer#reset_string_buffer ();
      action stk lexbuf }
  | '{' ('%' '%'? extattrident blank*)? (lowercase* as delim) "|"
    { action_buffer#store_lexeme lexbuf; (* "{...|" *)
      quoted_string delim action_buffer lexbuf;
      action_buffer#store_lexeme lexbuf;
      action stk lexbuf }
  | "'"
    { action_buffer#store_lexeme lexbuf; (* The left quote. *)
      skip_char lexbuf ;
      action_buffer#store_lexeme lexbuf; (* The char and right quote. *)
      action stk lexbuf }
  | "(*"
    { action_buffer#store_lexeme lexbuf;
      comment_buffer#reset_string_buffer ();
      comment 0 lexbuf |> ignore;
      action_buffer#store_string (comment_buffer#get_stored_string ());
      comment_buffer#reset_string_buffer();
      action stk lexbuf }
  | eof
    { raise_lexical_error lexbuf "unterminated action" }
  | '\010'
    { action_buffer#store_lexeme lexbuf;
      incr_loc lexbuf 0;
      action stk lexbuf }
  | ocaml_ident
    { action_buffer#store_lexeme lexbuf;
      action stk lexbuf }
  | _
    { action_buffer#store_lexeme lexbuf;
      action stk lexbuf }

and skip_char = parse
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
  | "" { () }
