let run () =
  let in_, out = Lwt_io.(stdin, stdout) in
  let server = Debug_rpc.create ~in_ ~out () in
  prerr_endline "Started DAP server";
  let task = Debug_rpc.start server in
  
  match Lwt_main.run task with
  | () -> ()
  | exception e ->
      let e = Printexc.to_string e in
      Printf.eprintf "error: %s\n%!" e;
      exit 1

let () =
  Printexc.record_backtrace true;
  run ()
