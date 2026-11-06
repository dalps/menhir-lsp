open Lex
open Menhir_lsp_lib.Utils

type range = Lexing.position * Lexing.position
type rule = { name : string; module_path : string }
type entrypoint = (string list, Syntax.location) Lexgen.automata_entry
type lexer_machine = entrypoint list * Lex.Lexgen.automata array

val parse_dfa : string -> (lexer_machine, string) result

module Token : sig
  type data = { text : string; action : int }
  type t = data Range.Located.t

  val make : Lexing.lexbuf -> int -> t
  val yojson_of_t : t -> Yojson.Safe.t
end

val eof : int

exception EmptyToken

val next_token : Lexing.lexbuf -> entrypoint -> Lexgen.automata array -> Token.t

val tokenize_with_idx :
  start_rule_idx:int -> Lexing.lexbuf -> lexer_machine -> Token.t list

val tokenize_with_entry :
  Lexing.lexbuf -> entrypoint -> Lexgen.automata array -> Token.t list
