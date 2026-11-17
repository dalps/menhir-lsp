open Lex
open Menhir_lsp_lib.Utils

type entrypoint = (string list, Syntax.location) Lexgen.automata_entry
type position = Lexing.position
type range = position * position
type automata = Lex.Lexgen.automata array
type lexer = entrypoint list * automata
type token_data = { text : string; action : int }
type token = token_data Range.Located.t

val parse_dfa : string -> (lexer, string) result

module Token : sig
  type t = token

  val make : Lexing.lexbuf -> int -> t
  val yojson_of_t : t -> Yojson.Safe.t
end

exception EmptyToken

val next_token : Lexing.lexbuf -> entrypoint -> Lexgen.automata array -> Token.t

val tokenize_with_idx :
  start_rule_idx:int -> Lexing.lexbuf -> lexer -> Token.t list

val tokenize_with_entry :
  Lexing.lexbuf -> entrypoint -> Lexgen.automata array -> Token.t list

module Persistent : sig
  type lexbuf = {
    lex_start_pos : int;
    lex_curr_pos : int;
    lex_eof_reached : bool;
    lex_start_p : position;
    lex_curr_p : position;
  }

  val zero_pos : position
  val start_lexbuf : lexbuf

  module Token : sig
    type t = token

    val make : bytes -> lexbuf -> int -> t
    val yojson_of_t : t -> Yojson.Safe.t
  end

  val next_token :
    bytes -> lexbuf -> entrypoint -> Lexgen.automata array -> lexbuf * Token.t

  val tokenize_with_idx :
    start_rule_idx:int ->
    bytes ->
    lexbuf ->
    entrypoint list * Lexgen.automata array ->
    Token.t list

  val tokenize_with_entry :
    bytes -> lexbuf -> entrypoint -> Lexgen.automata array -> Token.t list
end
