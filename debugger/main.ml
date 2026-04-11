let () =
  let s = "./_build/default/test_debug/test_debug.cma" in
  let module M =
    (val match Linkage.load s with
         | Ok (Menhirdebug_lib.Plugin.Menhirdebug_plugin m) -> m
         | e -> Linkage.raise_error e)
  in
  Printf.printf "The parser has %d terminals\n" M.Parser.Tables.terminal_count
