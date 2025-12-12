open MenhirSyntax

let filename = Sys.argv.(1)
let () = Main.load_grammar_from_file filename |> ignore
