open MenhirLib

(** Barrel module containing all the information needed to start a debugging
    session. Define it and feed it to [provide] *)
module type M = sig
  type semantic_value

  module Parser : sig
    type token

    module Tables : TableFormat.TABLES with type token = token

    module MenhirInterpreter :
      IncrementalEngine.EVERYTHING with type token = token

    module Incremental : sig
      val main : Lexing.position -> semantic_value MenhirInterpreter.checkpoint
    end
  end

  module Lexer : sig
    val token : Lexing.lexbuf -> Parser.token
  end
end

type Linkage.plugin += Menhirdebug_plugin of (module M)

let provide = Linkage.provide
