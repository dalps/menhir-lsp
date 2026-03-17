(** Based on:
    https://github.com/ocaml-ppx/ocamlformat/blob/main/test/rpc/rpc_test.ml *)

open Utils

module IO = struct
  type 'a t = 'a

  let ( >>= ) x f = f x
  let return x = x

  type ic = in_channel
  type oc = out_channel

  let read ic =
    match Csexp.input ic with Ok x -> return (Some x) | Error _ -> return None

  let write oc ss =
    L.iter (Csexp.to_channel oc) ss;
    Stdlib.flush oc;
    return ()
end

open Ocamlformat_rpc_lib
module Ocf = Make (IO)

let log = Format.eprintf

type close = unit -> unit
type state = Uninitialized | Running of Ocf.client * close | Errored

let state : state ref = ref Uninitialized

open R

let ( >>| ) x f = R.map f x

let start () =
  let ((input, output) : in_channel * out_channel) =
    Unix.open_process_args "ocamlformat-rpc" [||]
  in
  let pid = Unix.process_pid (input, output) in
  let versions = List.map Version.to_string [ V2; V1 ] in
  (match
     Ocf.pick_client ~pid input output versions >>| fun client ->
     let close =
      fun _ ->
       close_out output;
       close_in input
     in
     state := Running (client, close);
     client
   with
    | exception _ -> Error (`Msg "OCamlFormat-RPC did not respond.")
    | x -> x)
  |> R.map_err (fun (`Msg msg) ->
      state := Errored;
      log
        "An error occured while initializing and configuring ocamlformat:\n\
         %s\n\
         %!"
        msg;
      `No_process)

let get_client () =
  match !state with
  | Uninitialized -> start ()
  | Running (cl, _) ->
      let i, _ = Unix.waitpid [ WNOHANG ] (Ocf.pid cl) in
      if i = 0 then Ok cl else start ()
  | Errored -> Error `No_process

let close_client () =
  match !state with
  | Uninitialized | Errored -> ()
  | Running (cl, close) ->
      let i, _ = Unix.waitpid [ WNOHANG ] (Ocf.pid cl) in
      if i = 0 then close ()

let set_config c =
  get_client () >>= fun cl ->
  log "[ocf] Setting client's config.";
  Ocf.config c cl

let format ?(format_args = empty_args) x =
  get_client () >>= fun cl ->
  log "[ocf] Format '%s'\n" x;
  Ocf.format ~format_args x cl

let halt () =
  get_client () >>= fun cl ->
  log "[ocf] Halt\n";
  Ocf.halt cl >>= fun () ->
  close_client ();
  state := Uninitialized;
  return ()
