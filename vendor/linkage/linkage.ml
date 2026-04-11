type plugin = ..

exception Loaded of string * plugin

let provide p =
  raise (Loaded
    ("This module is a plugin, and should not be run directly", p))


type error =
| Dynlink_error of Dynlink.error
| Not_a_linkage_plugin
| Wrong_plugin_type of plugin


let load s =
  let s =
    if Filename.check_suffix s ".cma" ||
       Filename.check_suffix s ".cmo" then
      Dynlink.adapt_filename s
    else
      s in
  (* [menhir-lsp] For some weird reason, we need to re-raise the exception *)
  try
    try
      match Dynlink.loadfile_private s with () -> Error Not_a_linkage_plugin
    with
    | Loaded (_, p) -> Ok p (* nope *)
    | Dynlink.Error e -> (
        match e with
        | Dynlink.Library's_module_initializers_failed loadedexn ->
            raise loadedexn
        | e -> Error (Dynlink_error e))
  with Loaded (_, p) -> Ok p

(* By putting a string in the exception, the default
   exception printer will do a better job *)
exception Error of string * error

let raise_error r =
  let err = match r with
    | Ok p ->
       Wrong_plugin_type p
    | Error e ->
       e in
  let text = match err with
    | Dynlink_error e -> Dynlink.error_message e
    | Not_a_linkage_plugin -> "Not a Linkage plugin"
    | Wrong_plugin_type p ->
       "Wrong plugin type " ^
         Obj.Extension_constructor.(name (of_val p))
      (* Obj.(extension_name (extension_constructor p)) *)
      in
  raise (Error (text, err))
