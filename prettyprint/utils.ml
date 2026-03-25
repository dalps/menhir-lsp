include Menhir_lsp_lib.Utils

module PPrint = struct
  include PPrint

  let text = string

  let between sep d1 d2 =
    match (is_empty d1, is_empty d2) with
    | false, false -> d1 ^^ sep ^^ d2
    | false, true -> d1
    | _ -> d2

  let ( // ) = between hardline
  let ( //// ) = between (twice hardline)
  let ( ^-^ ) = between (blank 1)
  let ( ^/^ ) = between (break 1)
  let ( <|> ) d e = if is_empty d then e else d
  let ( <!> ) d e = if is_empty d then empty else e

  (** Prefix [sep] to [d] if [d] is nonempty. *)
  let ( ^! ) sep d = d <!> sep ^^ d

  (** Append [sep] to [d] if [d] is nonempty. *)
  let ( !^ ) d sep = d <!> d ^^ sep

  (** A smarter [separate_map] that doesn't insert [sep] if either side is
      empty. *)
  let separate_map sep f docs =
    L.fold_left (fun accu -> f >> between sep accu) empty docs

  (** Indexed [separate_map]. *)
  let separate_mapi sep f docs =
    L.foldi (fun accu idx -> f idx >> between sep accu) empty docs

  let separate sep = separate_map sep (fun x -> x)

  (** A smarter [flow_map] that doesn't insert [sep] if either side is empty. *)
  let flow_map sep f docs =
    L.foldi
      (fun accu i doc ->
        let doc' = f doc in
        if i = 0 then doc'
        else if is_empty accu then doc'
        else accu ^^ group (sep ^! doc'))
      empty docs

  let flow sep = flow_map sep (fun x -> x)
  let if_ ?(else_ = empty) ~then_ b = if b then then_ else else_
  let barspace = text "| "
  let enclose l x r = enclose l r x
end
