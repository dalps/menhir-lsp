open Utils

module Make (Syntax : sig
  type range = Lexing.position * Lexing.position

  type 'a located = { p : range; v : 'a; mutable comment : comments }
  and comments = string located list option

  (* val get_comments : unit -> string located list *)

  type syntax
end) =
struct
  open Syntax

  type comment = string located

  module Cmt = struct
    type t =
      | Cmt : {
          range : Range.t;
          c : comment;
          last_parent : _ located option ref;
        }
          -> t

    let of_lexer_comment (c : comment) : t =
      Cmt { c; range = Range.of_lexical_positions c.p; last_parent = ref None }

    let with_parent (Cmt c : t) parent : t = Cmt { c with last_parent = parent }

    let compare (Cmt c1) (Cmt c2) =
      Range.compare c1.range c2.range |> Ordering.to_int

    let show (Cmt cmt) = spr "`%s`%s" cmt.c.v (Range.show cmt.range)
  end

  module Bag = CCSet.Make (Cmt)

  let show_loc ~doc loc =
    let range_loc = Range.of_lexical_positions loc.p in
    spr "`%s`%s"
      (Utils.substring doc range_loc |> O.get_string)
      (Range.show range_loc)

  let init_bag = L.map Cmt.of_lexer_comment >> Bag.of_list >> ref

  let visit_located ~bag_of_comments ~doc ~notify_back visit_v env located =
    let range_loc = Range.of_lexical_positions located.p in
    let parent_ref = ref (Some located) in
    let ok_comments =
      !bag_of_comments
      |> Bag.filter_map (fun (Cmt cmt as c) ->
          log_info ~notify_back
            "Seeing if comment %s can be directly attached to node: %s"
            (Cmt.show c) (show_loc ~doc located);
          match Range.compare_inclusion cmt.range range_loc with
          | `Before
          (* when _only_comments
                     Range.(
                       create ~start:cmt.range.end_ ~end_:range_loc.start) *)
            ->
              log_info ~notify_back "* Yes, prepending.";
              Some (Cmt cmt)
          | `Contained ->
              (* The comment comes way before or is contained: remember this node in case the comment is never picked up by the previous case. *)
              log_info ~notify_back
                "* No, but it is contained in the node, so I'll remember this \
                 node as its parent.";
              bag_of_comments :=
                Bag.add (Cmt.with_parent c parent_ref)
                @@ Bag.remove c !bag_of_comments;
              None
          | _ ->
              log_info ~notify_back "* No.";
              None)
    in
    bag_of_comments := Bag.diff !bag_of_comments ok_comments;
    let comment =
      ok_comments |> Bag.to_list |> L.map (fun (Cmt.Cmt cmt) -> cmt.c) |> O.some
    in
    let located' = { located with v = visit_v env located.v; comment } in
    parent_ref := Some located';
    located'

  let attach_comments ~bag_of_comments grammar (v : syntax -> syntax)
      ~notify_back ~doc =
    let _only_comments rng : bool =
      try
        let text = Utils.substring doc rng |> Option.get in
        text |> Lexing.from_string |> Comments.main;
        true
      with _ ->
        log_info ~notify_back "Bad range!";
        false
    in

    let res = v grammar in

    log_info ~notify_back "There are %d comments left in the bag."
      (Bag.cardinal !bag_of_comments);
    Bag.iter
      (fun (Cmt { last_parent; c; _ } as cmt) ->
        log_info ~notify_back "Considering comment %s." (Cmt.show cmt);
        O.iter
          (fun (loc : _ located) ->
            log_info ~notify_back "This comment has a parent!";
            loc.comment <-
              O.fold (fun init cs -> cs @ init) [ c ] loc.comment |> O.some)
          !last_parent)
      !bag_of_comments;
    res
end
