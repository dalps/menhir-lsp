open Utils

let log_info s = Printf.ksprintf prerr_endline s

(** This module is responsible for attaching comments to located syntax nodes
    over a generic syntax interface. Every lexed comment must be attached to a
    syntax node, so the latter can be freely moved around by the formatter. *)
module Make (Syntax : sig
  type range = Lexing.position * Lexing.position

  type 'a located = { p : range; v : 'a; mutable comment : comments }
  and comments = comment list option
  and distance = { character : int; line : int }

  and comment = {
    text : string;
    relpos : [ `Before of distance | `After of distance ];
  }

  type syntax
end) =
struct
  open Syntax

  module Cmt = struct
    type t =
      | Cmt : {
          range : Range.t;
          c : comment;
          last_parent : _ located option ref;
        }
          -> t

    let of_lexer_comment (c : string located) : t =
      Cmt
        {
          c = { text = c.v; relpos = `Before { character = 42; line = 0 } };
          range = Range.of_lexical_positions c.p;
          last_parent = ref None;
        }

    let with_parent (Cmt c : t) parent : t = Cmt { c with last_parent = parent }

    let compare (Cmt c1) (Cmt c2) =
      Range.compare c1.range c2.range |> Ordering.to_int

    let show (Cmt cmt) = spr "`%s`%s" cmt.c.text (Range.show cmt.range)
  end

  module Bag = CCSet.Make (Cmt)

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
    let ok_comments =
      !bag_of_comments
      |> Bag.filter_map (fun (Cmt cmt as c) ->
          log_info "Seeing if comment %s can be directly attached to node: %s"
            (Cmt.show c) (show_loc ~doc located);
          let relpos = Range.compare_inclusion cmt.range range_loc in
          match relpos with
          | `Before { character; line }
            when only_comments ~doc ~allow_newlines:`AllowNewlines
                   Range.(create ~start:cmt.range.end_ ~end_:range_loc.start) ->
              log_info "* Yes, prepending.";
              Some
                (Cmt
                   {
                     cmt with
                     c = { cmt.c with relpos = `Before { character; line } };
                   })
          | `After { character; line }
            when only_comments ~doc ~allow_newlines:`DisallowNewlines
                   Range.(create ~start:range_loc.end_ ~end_:cmt.range.start) ->
              log_info "* Yes, appending.";
              Some
                (Cmt
                   {
                     cmt with
                     c = { cmt.c with relpos = `After { character; line } };
                   })
          | `Contained ->
              (* The comment comes way before or is contained: remember this node in case the comment is never picked up by the previous case. *)
              log_info
                "* No, but it is contained in the node, so I'll remember this \
                 node as its parent.";
              bag_of_comments :=
                Bag.add (Cmt.with_parent c parent_ref)
                @@ Bag.remove c !bag_of_comments;
              None
          | _ ->
              log_info "* No.";
              None)
    in
    bag_of_comments := Bag.diff !bag_of_comments ok_comments;
    let comment =
      ok_comments |> Bag.to_list |> L.map (fun (Cmt.Cmt cmt) -> cmt.c) |> O.some
    in
    let located' = { located with v = visit_v env located.v; comment } in
    parent_ref := Some located';
    located'

  (** Starts the attaching process. *)
  let attach_comments ~bag_of_comments grammar (v : syntax -> syntax) ~doc:_ =
    let res = v grammar in
    (* log_info
      "There are %d comments left in the bag. Trying to find a home for them..."
      (Bag.cardinal !bag_of_comments); *)
    Bag.iter
      (fun (Cmt { last_parent; c; _ } as _cmt) ->
        (* log_info "Considering comment %s." (Cmt.show cmt); *)
        O.iter
          (fun (loc : _ located) ->
            (* log_info "This comment has a parent!"; *)
            loc.comment <-
              O.fold (fun init cs -> cs @ init) [ c ] loc.comment |> O.some)
          !last_parent)
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
      (fun doc idx (text, { character; line }) ->
        log_info "\nbefore comment %s with distance (%d, %d)" text character
          line;
        doc ^^ string text
        ^^
        if idx = n_before - 1 then repeat (min 2 line |> max 1) hardline
        else twice hardline)
      empty before_comments
    ^^ k located
    ^^ L.foldi
         (fun doc idx (text, { character; line }) ->
           log_info "\nafter comment %s with distance (%d, %d)" text character
             line;

           doc
           ^^ (match (idx, line, character) with
             | 0, 0, _ -> blank 1 (* inline this, break the rest *)
             | 0, l, _ -> repeat (min 2 l) hardline
             | _ -> twice hardline)
           ^^ string text)
         empty after_comments
end
