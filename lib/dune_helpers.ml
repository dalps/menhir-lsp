open Utils

let _build_dir = ref (Error "")

let get_build_dir _ =
  let f () =
    let err = Error "Failure: dune describe" in
    try
      let inp = Unix.open_process_in "dune describe workspace" in
      let s = CCSexp.parse_chan inp in
      In_channel.close inp;
      match s with
      | Ok
          (`List
             (`List [ `Atom "root"; `Atom root_dir ]
             :: `List [ `Atom "build_context"; `Atom context ]
             :: _)) ->
          _build_dir := Ok (root_dir, context);
          !_build_dir
      | _ -> err
    with _ ->
      (* May fail due to 'A running dune (pid: ..) instance has locked the build directory.') *)
      err
  in
  match !_build_dir with Ok _ -> !_build_dir | Error _ -> f ()

(** e.g. if [uri] is

    [/home/foo/menhir-lsp/test/calc.mly]

    then [fetch_build_dir ~ext:".conflicts" uri] is

    [/home/foo/menhir-lsp/_build/default/test/calc.conflicts] *)
let fetch_build_dir ?(ext : string option) uri =
  let module P = Stdune.Path in
  let module F = Filename in
  let s_path = DocumentUri.to_path uri in
  let s_name = F.basename s_path in
  let s_slug = F.remove_extension s_name in
  let s_ext = O.get_or ~default:(F.extension s_name) ext in
  let open R in
  let* root, ctx = get_build_dir () in
  let p_root = P.of_string root in
  let p_dir = P.of_string (F.dirname s_path) in
  match P.drop_prefix p_dir ~prefix:p_root with
  | None ->
      Error
        (spr
           "No config found for %s. Make sure (menhir (modules .. %s ..)) is \
            included in the stanza's dune file."
           s_name s_slug)
  | Some p_rel ->
      let p_ctx = P.of_string (F.concat root ctx) in
      let p_res = P.append_local p_ctx p_rel in
      let res = F.concat (P.to_string p_res) (s_slug ^ s_ext) in
      Ok res
