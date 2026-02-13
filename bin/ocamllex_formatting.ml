open OcamllexSyntax
open Utils
open Located

module Cmt = struct
  type t =
    | Cmt : {
        range : Range.t;
        c : Lexer.comment;
        last_parent : _ located option ref;
      }
        -> t

  let of_lexer_comment (c : Lexer.comment) : t =
    Cmt { c; range = Range.of_lexical_positions c.p; last_parent = ref None }

  let with_parent (Cmt c : t) parent : t = Cmt { c with last_parent = parent }

  let compare (Cmt c1) (Cmt c2) =
    Range.compare c1.range c2.range |> Ordering.to_int

  let show (Cmt cmt) = spr "`%s`%s" cmt.c.v (Range.show cmt.range)
end

module Cmts = CCSet.Make (Cmt)

let attach_comments grammar ~notify_back ~doc =
  let bag_of_comments : Cmts.t ref =
    Lexer.get_comments () |> L.map Cmt.of_lexer_comment |> Cmts.of_list |> ref
  in
  let _only_comments rng : bool =
    try
      let text = Utils.substring doc rng |> Option.get in
      text |> Lexing.from_string |> Comments.main;
      true
    with _ ->
      log_info ~notify_back "Bad range!";
      false
  in
  let show_loc loc =
    let range_loc = Range.of_lexical_positions loc.p in
    spr "`%s`%s"
      (Utils.substring doc range_loc |> O.get_string)
      (Range.show range_loc)
  in
  let v =
    object
      inherit [_] Syntax.syntax_endo

      method! visit_located visit_v env located =
        let range_loc = Range.of_lexical_positions located.p in
        let parent_ref = ref (Some located) in
        let ok_comments =
          !bag_of_comments
          |> Cmts.filter_map (fun (Cmt cmt as c) ->
              log_info ~notify_back
                "Seeing if comment %s can be directly attached to node: %s"
                (Cmt.show c) (show_loc located);
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
                    "* No, but it is contained in the node, so I'll remember \
                     this node as its parent.";
                  bag_of_comments :=
                    Cmts.add (Cmt.with_parent c parent_ref)
                    @@ Cmts.remove c !bag_of_comments;
                  None
              | _ ->
                  log_info ~notify_back "* No.";
                  None)
        in
        bag_of_comments := Cmts.diff !bag_of_comments ok_comments;
        let comment =
          ok_comments |> Cmts.to_list
          |> L.map (fun (Cmt.Cmt cmt) -> cmt.c)
          |> O.some
        in
        let located' = { (Located.map (visit_v env) located) with comment } in
        parent_ref := Some located';
        located'
    end
  in

  let res = v#visit_lexer_definition () grammar in

  log_info ~notify_back "There are %d comments left in the bag."
    (Cmts.cardinal !bag_of_comments);
  Cmts.iter
    (fun (Cmt { last_parent; c; _ } as cmt) ->
      log_info ~notify_back "Considering comment %s." (Cmt.show cmt);
      O.iter
        (fun (loc : _ Located.located) ->
          log_info ~notify_back "This comment has a parent!";
          loc.comment <-
            O.fold (fun init cs -> cs @ init) [ c ] loc.comment |> O.some)
        !last_parent)
    !bag_of_comments;
  res

open PPrint

let render ~(notify_back : notify_back) ~(doc : Text_document.t) =
  let open Syntax in
  let _ = (doc, notify_back) in
  let v =
    object (self)
      inherit [_] syntax_reduce as super
      method zero = empty
      method plus = ( ^^ )
      method text = string

      (* --------------------------------------- *)
      (* Shorthands with unit environments. *)

      method private with_located :
          'env 'a. ('a -> document) -> 'a located -> document =
        fun f b -> self#visit_located (fun _ v -> f v) () b

      method private visit_regexp : regular_expression_syntax -> document =
        self#visit_regular_expression_syntax ()

      method private visit_charclass : character_class_syntax -> document =
        self#visit_character_class_syntax ()

      (* --------------------------------------- *)

      (* Comments may only appear before located syntax nodes. *)
      method! visit_located visit_v env ({ comment; _ } as located) =
        let comments =
          optional (separate_map hardline (Located.value >> text)) comment
        in
        group @@ (comments // super#visit_located visit_v env located)

      (* --------------------------------------- *)

      method! visit_action _ =
        self#with_located (fun v ->
            surround 2 1 lbrace (v |> String.trim |> text) rbrace)

      method! visit_lexer_definition _ lexer_definition =
        let { header; entrypoints; trailer; refill_handler; named_regexps } =
          lexer_definition
        in
        optional (self#visit_action ()) header
        //// optional (self#visit_action ()) refill_handler
        //// separate_map
               (hardline ^^ break 1)
               (self#visit_located self#visit_named_regexp ())
               named_regexps
        ////
        let h, t = L.take_drop 1 entrypoints in
        let render_entries start =
          separate_map
            (hardline ^^ break 1)
            (fun e -> text start ^-^ self#visit_located self#visit_entry () e)
        in
        render_entries "rule" h // render_entries "and" t
        //// optional (self#visit_action ()) trailer

      method! visit_named_regexp _ { name; regexp } =
        prefix 2 1
          (text "let" ^-^ self#with_located text name ^-^ text "=")
          (self#with_located self#visit_regexp regexp)

      method! visit_entry _ { name; shortest; args; clauses } =
        let barspace = bar ^^ space in
        prefix 2 1
          (flow space
          @@ [
               self#with_located text name;
               nest 2 @@ flow_map (break 1) (self#with_located text) args;
               equals;
               self#with_located
                 ((fun v -> if v then "shortest" else "parse") >> text)
                 shortest;
             ])
        @@ align barspace
        ^^ separate_map (hardline ^^ barspace) (self#visit_case ()) clauses

      method visit_case _ (regexp, action) =
        prefix 2 1
          (self#with_located self#visit_regexp regexp)
          (self#visit_action () action)

      method! visit_Wildcard _ = self#with_located (fun _ -> text "_")
      method! visit_EOF _ = self#with_located (fun _ -> text "eof")

      method! visit_Character _ =
        self#with_located (char_of_int >> Char.escaped >> text >> squotes)

      method! visit_Char = self#visit_Character
      method! visit_String _ = self#with_located (text >> dquotes)
      method! visit_Ref _ = self#with_located text

      method! visit_Seq _ re1 re2 =
        group @@ align
        @@ self#with_located self#visit_regexp re1
        ^/^ self#with_located self#visit_regexp re2

      method! visit_Alt _ re1 re2 =
        group @@ align
        @@ self#with_located self#visit_regexp re1
        ^/^ bar
        ^-^ self#with_located self#visit_regexp re2

      method! visit_CharSetDifference _ re1 re2 =
        group @@ align
        @@ self#with_located self#visit_regexp re1
        ^/^ sharp
        ^-^ self#with_located self#visit_regexp re2

      method! visit_Rep _ =
        self#with_located (fun re -> self#visit_regexp re ^^ star)

      method! visit_Rep1 _ =
        self#with_located (fun re -> self#visit_regexp re ^^ plus)

      method! visit_Option _ =
        self#with_located (fun re -> self#visit_regexp re ^^ qmark)

      method! visit_Group _ =
        self#with_located (function
          | Group re -> self#visit_Group () re (* already grouped *)
          | re -> surround 2 1 lparen (self#visit_regexp re) rparen)

      method! visit_As _ re ident =
        (* To get a better idea of what is being captured, we could surround alternation and sequences with parens. *)
        self#with_located self#visit_regexp re
        ^/^ text "as"
        ^-^ self#with_located text ident

      method! visit_CharSet _ =
        self#with_located (fun v ->
            surround 2 0 lbracket (self#visit_charclass v) rbracket)

      method! visit_Union _ cls1 cls2 =
        group
        @@ self#with_located self#visit_charclass cls1
        ^/^ self#with_located self#visit_charclass cls2

      method! visit_Complement _ cls =
        caret ^^ self#with_located self#visit_charclass cls

      method! visit_Range _ c1 c2 =
        self#visit_Character () c1 ^^ minus ^^ self#visit_Character () c2 (**)
    end
  in
  v#visit_lexer_definition ()
