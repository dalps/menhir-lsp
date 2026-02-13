open Utils
open OcamllexSyntax
open PPrint

include Comment_location.Make (struct
  type syntax = Syntax.main

  include Located
  include Range
end)

class formatter ~(notify_back : notify_back) ~(doc : Text_document.t) =
  let open Syntax in
  let _ = (doc, notify_back) in
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
