open Menhirdebug_lib.Plugin

module M = struct
  type semantic_value = int

  module Parser = Calc
  module Lexer = Calc_lexer
end

let () = provide (Menhirdebug_plugin (module M))
