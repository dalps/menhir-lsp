open MenhirLib

(** Barrel module containing all the information needed to start a debugging
    session. Define it and feed it to [provide] *)
module type M = sig
  type semantic_value

  module Parser : sig
    type token

    module Tables : TableFormat.TABLES with type token = token

    module MenhirInterpreter :
      IncrementalEngine.INCREMENTAL_ENGINE
      (* IncrementalEngine.EVERYTHING if you need --inspection stuff *)
        with type token = token

    module Incremental : sig
      val main : Lexing.position -> semantic_value MenhirInterpreter.checkpoint
    end
  end

  module Lexer : sig
    val token : Lexing.lexbuf -> Parser.token
  end
end

type Linkage.plugin += Menhirdebug_plugin of (module M)

(** To be called by a debuggee program. *)
let provide = Linkage.provide

let get_terminal_count uri : (int, string) result =
  match Linkage.load uri with
  | Ok (Menhirdebug_plugin m) ->
      let module M = (val m) in
      Ok M.Parser.Tables.terminal_count
  | Error e -> Error (Linkage.string_of_error e)
  | e -> Linkage.raise_error e
