let () = Findlib.init ()

let () =
  let pkgs = Fl_package_base.list_packages ~prefix:"samples" () in
  print_endline "Findlib packages:";
  pkgs |> List.iter print_endline

let () =
  let _s1 = "./_build/default/test_debug/calc_debug.cmxs" in
  let _s2 =
    "./_build/default/test_debug/.test_debug.objs/native/test_debug__Calc_debug.cmx"
  in
  let _s3 =
    "./_build/default/test_debug/.test_debug.objs/byte/test_debug__Calc_debug.cmo"
  in
  let module M =
    (val match Linkage.load _s2 with
         | Ok (Menhirdebug_lib.Plugin.Menhirdebug_plugin m) -> m
         | e -> Linkage.raise_error e)
  in
  ()
