open Lex

type range = Lexing.position * Lexing.position
type rule = { name : string; module_path : string }
type entrypoint = (string list, Syntax.location) Lexgen.automata_entry
type lexer_machine = entrypoint list * Lex.Lexgen.automata array

val parse_dfa : string -> (lexer_machine, string) result

module Token : sig
  type t = { text : string; loc : range }

  val make : Lexing.lexbuf -> t
  val yojson_of_t : t -> Yojson.Safe.t
end

val eof : int

exception EmptyToken

val tokenize : start_rule_idx:int -> Lexing.lexbuf -> lexer_machine -> Token.t list
val tokenize_to_json : int -> Lexing.lexbuf -> lexer_machine -> Yojson.Safe.t
