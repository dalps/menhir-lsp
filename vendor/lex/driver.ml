open Printf
open Syntax

(** The first component of the type parameter represents captured variables, the
    second one the location of the action including braces. *)
let parse_dfa ?(source_name = "<text>") lexbuf =
  lexbuf.Lexing.lex_curr_p <-
    {
      Lexing.pos_fname = source_name;
      Lexing.pos_lnum = 1;
      Lexing.pos_bol = 0;
      Lexing.pos_cnum = 0;
    };
  try
    let def = Parser.lexer_definition Lexer.main lexbuf in
    let entries, transitions = Lexgen.make_dfa def.entrypoints in
    Ok (entries, transitions)
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    let fmt = Format.sprintf in
    let message =
      begin match exn with
      | Cset.Bad ->
          let p = Lexing.lexeme_start_p lexbuf in
          fmt "File \"%s\", line %d, character %d: character set expected.\n"
            p.Lexing.pos_fname p.Lexing.pos_lnum
            (p.Lexing.pos_cnum - p.Lexing.pos_bol)
      | Parsing.Parse_error ->
          let p = Lexing.lexeme_start_p lexbuf in
          fmt "File \"%s\", line %d, character %d: syntax error.\n"
            p.Lexing.pos_fname p.Lexing.pos_lnum
            (p.Lexing.pos_cnum - p.Lexing.pos_bol)
      | Lexer.Lexical_error (msg, file, line, col) ->
          fmt "File \"%s\", line %d, character %d: %s.\n" file line col msg
      | Lexgen.Memory_overflow ->
          fmt "File \"%s\":\n Position memory overflow, too many bindings\n"
            source_name
      | Output.Table_overflow ->
          fmt "File \"%s\":\ntransition table overflow, automaton is too big\n"
            source_name
      | _ -> Printexc.raise_with_backtrace exn bt
      end
    in
    Error message

let parse_string text =
  let lexbuf = Lexing.from_string text in
  parse_dfa lexbuf

let parse_file source_name =
  In_channel.with_open_bin source_name (fun ic ->
      let lexbuf = Lexing.from_channel ic in
      parse_dfa lexbuf)

let pr = Format.printf
