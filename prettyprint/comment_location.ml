open Utils

let debug = false
let log_info s = Printf.ksprintf (fun s -> if debug then prerr_endline s) s

(** This module is responsible for attaching comments to located syntax nodes
    over a generic syntax interface. Every lexed comment must be attached to a
    syntax node before formatting, so nodes can be moved around freely by the
    formatter. *)
module Make (Syntax : sig
  type range = Lexing.position * Lexing.position

  type 'a located = { p : range; v : 'a; mutable comment : comments }
  and comments = comment list option
  and comment = { text : string; relpos : relpos }

  type syntax
end) =
struct
  open Syntax

  module Cmt = struct
    type t =
      | Cmt : {
          text : string;
          relpos : relpos;
              (* The relative position to the node it is attached or nearest to. *)
          range : Range.t;
          last_parent : _ located option ref;
          nearest_loc : (_ located ref * relpos) option;
        }
          -> t

    let of_lexer_comment (c : string located) : t =
      Cmt
        {
          text = c.v;
          relpos = `Before { character = -1; line = -1 };
          range = Range.of_lexical_positions c.p;
          last_parent = ref None;
          nearest_loc = None;
        }

    let with_parent (Cmt c : t) parent : t = Cmt { c with last_parent = parent }

    let with_nearest (Cmt c : t) ((_, new_relpos) as new_nearest) : t =
      Cmt { c with relpos = new_relpos; nearest_loc = Some new_nearest }

    let compare (Cmt c1) (Cmt c2) =
      Range.compare c1.range c2.range |> Ordering.to_int

    let show (Cmt cmt) = spr "`%s`%s" cmt.text (Range.show cmt.range)
  end

  module Bag = struct
    include CCSet.Make (Cmt)

    let replace bag old new_ = add new_ @@ remove old bag
    let replace_ref (bag : t ref) old new_ = bag := replace !bag old new_
  end

  let show_loc ~doc loc =
    let range_loc = Range.of_lexical_positions loc.p in
    spr "`%s`%s"
      (Utils.substring doc range_loc |> O.get_string)
      (Range.show range_loc)

  (** Check whether the section of [doc] delimited by [rng] contains only
      whitespace or comments. *)
  let only_comments ~doc ~allow_newlines rng : bool =
    try
      Utils.substring doc rng |> Option.get |> Lexing.from_string
      |> Comments.main allow_newlines;
      true
    with _ -> false

  let init_bag = L.map Cmt.of_lexer_comment >> Bag.of_list >> ref

  (** Override the visit_located method of your endo object with this. *)
  let visit_attach :
      'env 'a.
      bag_of_comments:Bag.t ref ->
      doc:TD.t ->
      ('env -> 'a -> 'a) ->
      'env ->
      'a located ->
      'a located =
   fun ~bag_of_comments ~doc visit_v env located ->
    let range_loc = Range.of_lexical_positions located.p in
    let parent_ref = ref (Some located) in
    let nearest_ref = ref located in
    (* Scan the bag for comments that can be attached directly to the node,
    and update the other comments. *)
    let ok_comments =
      !bag_of_comments
      |> Bag.filter_map (fun (Cmt cmt as c) ->
          (* log_info "Seeing if comment %s can be directly attached to node: %s"
            (Cmt.show c) (show_loc ~doc located); *)
          let relpos = Range.compare_inclusion cmt.range range_loc in
          match relpos with
          | `Before dist
            when only_comments ~doc ~allow_newlines:`AllowNewlines
                   Range.(create ~start:cmt.range.end_ ~end_:range_loc.start) ->
              (* log_info "* Yes, prepending."; *)
              Some (Cmt { cmt with relpos = `Before dist })
          | `After dist
            when only_comments ~doc ~allow_newlines:`DisallowNewlines
                   Range.(create ~start:range_loc.end_ ~end_:cmt.range.start) ->
              (* log_info "* Yes, appending."; *)
              Some (Cmt { cmt with relpos = `After dist })
          | `Contained ->
              (* log_info
                "* No, but it is contained in the node, so I'll remember this \
                 node as its parent."; *)
              Bag.replace_ref bag_of_comments c (Cmt.with_parent c parent_ref);
              None
          | _ ->
              (* log_info "* No."; *)
              (match relpos with
              | (`After _dist | `Before _dist) as relpos -> (
                  (* log_info "The distance to this node is %s."
                    (Position.show dist); *)
                  match cmt.nearest_loc with
                  | None ->
                      (* log_info
                        "The nearest_loc field was empty, so I'll make this \
                         node the nearest."; *)
                      Bag.replace_ref bag_of_comments c
                        (Cmt.with_nearest c (nearest_ref, relpos))
                  | Some (_, old_relpos)
                    when compare_relpos relpos old_relpos < 0 ->
                      (* log_info
                        "The node is closer than the old nearest with distance \
                         %s"
                        (Position.show
                           (match old_relpos with
                           | `After dist | `Before dist -> dist)); *)
                      Bag.replace_ref bag_of_comments c
                        (Cmt.with_nearest c (nearest_ref, relpos))
                  | _ -> ())
              | _ -> ());

              None)
    in
    (* Update the bag to remove the attached comments *)
    bag_of_comments := Bag.diff !bag_of_comments ok_comments;
    (* Compute the node's comment list (every node is processed once!) *)
    let comment =
      ok_comments |> Bag.to_list
      |> L.map (fun (Cmt.Cmt cmt) -> { text = cmt.text; relpos = cmt.relpos })
      |> O.some
    in
    let located' = { located with v = visit_v env located.v; comment } in
    (* Sync the comment's parent ref *)
    parent_ref := Some located';
    nearest_ref := located';
    (* Return the updated node *)
    located'

  (** Starts the attaching process. *)
  let attach_comments ~bag_of_comments grammar (v : syntax -> syntax) ~doc:_ =
    (* Run the visitor at the root node *)
    let res = v grammar in
    (* log_info
      "There are %d comments left in the bag. Trying to find a home for them..."
      (Bag.cardinal !bag_of_comments); *)
    Bag.iter
      (fun (Cmt { text; relpos; last_parent; nearest_loc; _ } as _cmt) ->
        let c = { text; relpos } in
        (* log_info "Considering comment %s." (Cmt.show cmt); *)
        match !last_parent with
        | Some loc ->
            (* log_info "This comment has a parent!"; *)
            loc.comment <-
              O.fold (fun init cs -> cs @ init) [ c ] loc.comment |> O.some
        | None -> (
            match nearest_loc with
            | Some (loc, _) ->
                let loc = !loc in
                (* log_info
                  "The comment '%s' is an orphan, so I'll attach it to its \
                   nearest node at %s."
                  c.text
                  Range.(show (of_lexical_positions loc.p)); *)
                loc.comment <-
                  O.fold (fun init cs -> cs @ init) [ c ] loc.comment |> O.some
            | None -> failwith "impossible"))
      !bag_of_comments;
    res

  (** Comments may appear before or after located syntax nodes. Before comments
      are broken up into individual lines, after comments (line comments) are
      laid out on the same line separated by single spaces. *)
  let render_located k ({ comment; _ } as located) : PPrint.document =
    let open PPrint in
    let before_comments, after_comments =
      O.map_or ~default:([], [])
        (L.partition_map_either (fun ({ text; relpos } : comment) ->
             (* let text = string text in *)
             match relpos with
             | `Before dist -> CCEither.Left (text, dist)
             | `After dist -> CCEither.Right (text, dist)))
        comment
    in
    (* let rest, closest =
      L.(take_drop (length before_comments - 1) before_comments)
      |> CCPair.map_snd L.head_opt
    in *)
    let n_before = L.(length before_comments) in
    L.foldi
      (fun doc idx (text, { character = _; line }) ->
        (* log_info "\nbefore comment %s with distance (%d, %d)" text character
          line; *)
        doc ^^ string text
        ^^
        if idx = n_before - 1 then repeat (min 2 line |> max 1) hardline
        else twice hardline)
      empty before_comments
    ^^ k located
    ^^ L.foldi
         (fun doc idx (text, { character; line }) ->
           (* log_info "\nafter comment %s with distance (%d, %d)" text character
             line; *)
           doc
           ^^ (match (idx, line, character) with
             | 0, 0, _ -> blank 1 (* inline this, break the rest *)
             | 0, l, _ -> repeat (min 2 l) hardline
             | _ -> twice hardline)
           ^^ string text)
         empty after_comments
end
