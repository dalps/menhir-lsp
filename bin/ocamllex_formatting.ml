open OcamllexSyntax
open Utils
module AST = Syntax
module DCST = Parser.DCST
module CST = Parser.CST

module AST2DCST : sig
  val main : AST.lexer_definition -> DCST.lexer_definition
end = struct
  open DCST

  let never_produced () = raise (Failure "never produced")

  (* To avoid any confusion: 'clause' and 'case' mean the same thing here. *)
  type ast_entry_clause = AST.regular_expression_syntax AST.located * AST.action

  let rec dcst_of_lexer_definition (def : AST.lexer_definition) :
      lexer_definition =
    let ( >> ) f g x = g (f x) in
    let of_option ~none ~some = function
      | None -> none ()
      | Some loc -> some loc
    in
    let rec of_definitions :
        AST.entry AST.located list ->
        DCST.separated_nonempty_list_Tand_definition_ = function
      | [] -> never_produced ()
      | [ e ] -> one_tand_definition (dcst_of_entry e.v)
      | e :: t -> more_tand_definition (dcst_of_entry e.v) (of_definitions t)
    in
    let rec of_named_regexps :
        AST.named_regexp AST.located list -> DCST.list_located_named_regexp__ =
      function
      | [] -> nil_located_named_regexp_ ()
      | e :: t ->
          cons_located_named_regexp_
            (located_named_regexp @@ dcst_of_named_regexp e.v)
            (of_named_regexps t)
    in
    lexer
      (of_option ~none:none_header ~some:(header >> some_header) def.header)
      (of_named_regexps def.named_regexps)
      (of_option ~none:none_refill_handler
         ~some:(refill_handler >> some_refill_handler)
         def.refill_handler)
      (of_definitions def.entrypoints)
      (of_option ~none:none_header ~some:(header >> some_header) def.trailer)

  and dcst_of_entry : AST.entry -> DCST.definition =
   fun entry ->
    let rec of_list : string AST.located list -> list_located_Tident__ =
      function
      | [] -> nil_located_tident_ ()
      | s :: t -> cons_located_tident_ (located_tident s.v) (of_list t)
    in
    rule_definition
      (located_tident entry.name.v)
      (of_list entry.args)
      (if entry.shortest.v then located_parse_or_shortest_parse ()
       else located_parse_or_shortest_shortest ())
      (dcst_of_entry_clauses entry.clauses)

  and dcst_of_clause : ast_entry_clause -> DCST.case =
   fun (re, a) -> case (dcst_of_regexp re) a

  and dcst_of_named_regexp : AST.named_regexp -> DCST.named_regexp = function
    | nre ->
        named_regexp (located_tident nre.name.v) (dcst_of_regexp nre.regexp)

  and dcst_of_entry_clauses : ast_entry_clause list -> DCST.entry =
   fun l ->
    let rec of_list :
        ast_entry_clause list -> DCST.separated_nonempty_list_Tor_case_ =
      function
      | [] -> never_produced ()
      | [ c ] -> one_tor_case (dcst_of_clause c)
      | c :: t -> more_tor_case (dcst_of_clause c) (of_list t)
    in
    entry (none_tor ()) (of_list l)

  and regexp_paren : regexp -> regexp = function
    | re -> regexp_choice re (regexp_group re)

  and dcst_of_regexp (re : AST.regular_expression_syntax AST.located) =
    regexp_paren
    @@
    match re.v with
    | AST.Epsilon _ -> never_produced ()
    | AST.CharSet { v = AST.Wildcard _; _ } ->
        regexp_wildcard (located_tunderscore ())
    | AST.CharSet c -> regexp_charset (dcst_of_charclass c.v)
    | AST.String s -> regexp_string s
    | AST.EOF _ -> regexp_eof (located_teof ())
    | AST.Seq (re1, re2) ->
        regexp_sequence (dcst_of_regexp re1) (dcst_of_regexp re2)
    | AST.Alt (re1, re2) ->
        regexp_alternative (dcst_of_regexp re1) (dcst_of_regexp re2)
    | AST.CharSetDifference (re1, re2) ->
        regexp_difference (dcst_of_regexp re1) (dcst_of_regexp re2)
    | AST.Rep re -> regexp_repetition (dcst_of_regexp re)
    | AST.Rep1 re -> regexp_repetition1 (dcst_of_regexp re)
    | AST.Ref s -> regexp_reference (located_tident s.v)
    | AST.Group re -> regexp_group (dcst_of_regexp re)
    | AST.Option re -> regexp_option (dcst_of_regexp re)
    | AST.As (re, s) ->
        regexp_binding (dcst_of_regexp re) (located_ident (identifier s.v))

  and dcst_of_charclass : AST.character_class_syntax -> DCST.char_class =
    function
    | AST.Complement c -> charclass_complement (dcst_of_charclass1 c.v)
    | c -> charclass1 (dcst_of_charclass1 c)

  and dcst_of_charclass1 : AST.character_class_syntax -> DCST.char_class1 =
    function
    | AST.Character c -> charclass_character (located_tchar c.v)
    | AST.Range (c1, c2) ->
        charclass_range (located_tchar c1.v) (located_tchar c2.v)
    | AST.Union (cls1, cls2) ->
        charclass_union (dcst_of_charclass1 cls1.v) (dcst_of_charclass1 cls2.v)
    | _ -> never_produced ()

  let main = dcst_of_lexer_definition
end

let spr = Printf.sprintf

module CST2String = struct
  class print =
    object
      inherit [string] CST.reduce
      method zero = ""
      method cat = spr "%s %s"
      method text s = s
      method visit_Tstring s = spr "\"%s\"" s.v
      method visit_Tident ide = ide
      method! visit_Tend = ""
      method visit_Tchar i = i |> Char.chr |> Char.escaped |> spr "'%s'"

      (* TODO: change Taction's semantic value to string located. You will have to collect the lexemes inside the action in the lexer. *)
      method visit_Taction a = spr "{ %s }" a.v
    end

  let main = (new print)#visit_lexer_definition
end

module CST2Document = struct
  open PPrint

  let smart_lparen (char : document) = char ^^ ifflat empty space
  let smart_rparen (char : document) = ifflat empty space ^^ char

  (* open Document *)

  class print ~(notify_back : notify_back) ~(doc : Text_document.t) =
    let _ = (notify_back, doc) in
    object (self)
      inherit [document] CST.reduce as super
      method zero = empty
      method cat = ( ^^ )
      method text = string
      method visit_Tstring s = utf8format "\"%s\"" s.v
      method visit_Tident = string

      method visit_Tchar i =
        let c = i |> Char.chr |> Char.escaped in
        utf8format "'%s'" c

      (* TODO: trim whitespace around action's content (what's strictly inside {}) so you can always surround it with one space in flat mode. *)
      method visit_Taction a = surround 2 0 lbrace (string a.v) rbrace
      method! visit_Tend = empty
      method! visit_Thash = space ^^ sharp ^^ break 1
      method! visit_Tand = space ^^ string "and" ^^ break 1
      method! visit_Tas = space ^^ string "as" ^^ break 1
      method! visit_Tequal = space ^^ equals ^^ break 1
      method! visit_Tlet = hardline ^^ super#visit_Tlet ^^ space
      method! visit_Trule = hardline ^^ super#visit_Trule ^^ space

      method! case_rule_definition ident args parse_or_shortest entry =
        group
        @@ flow (break 1)
             [
               flow space [ super#visit_located_Tident_ ident ];
               super#visit_list_located_Tident__ args;
               super#visit_Tequal;
               group
                 (flow (break 1)
                    [
                      super#visit_located_parse_or_shortest_ parse_or_shortest;
                      nest 2 (super#visit_entry entry);
                    ]);
             ]

      method! case_named_regexp name regexp =
        group
        @@ flow space
             [
               super#visit_Tlet;
               super#visit_located_Tident_ name;
               super#visit_Tequal;
               nest 2 (super#visit_regexp regexp);
             ]

      method! case_cons_located_named_regexp_ def rest =
        flow hardline
          [
            super#visit_located_named_regexp_ def;
            super#visit_list_located_named_regexp__ rest;
          ]

      method! case_nil_located_tident_ () = empty

      method! case_cons_located_tident_ ide rest =
        separate space
          [
            super#visit_located_Tident_ ide;
            super#visit_list_located_Tident__ rest;
          ]

      (* This will try to fit a regexp in a single line, breaks it up otherwise. *)
      method! visit_regexp re = group (super#visit_regexp re)

      (* This will try to fit a regexp in a single line, breaks it up otherwise. *)
      method! visit_char_class cls = group (super#visit_char_class cls)
      method! visit_char_class1 cls = group (super#visit_char_class1 cls)

      method! case_regexp_alternative re1 re2 =
        flow space
          [ super#visit_regexp re1; super#visit_Tor; super#visit_regexp re2 ]

      method! case_regexp_sequence re1 re2 =
        flow space [ super#visit_regexp re1; super#visit_regexp re2 ]

      method! case_charclass_union =
        fun cls1 cls2 ->
          self#visit_char_class1 cls1 ^^ break 1 ^^ self#visit_char_class1 cls2

      method! case_case =
        fun regexp location ->
          break 1 ^^ super#visit_regexp regexp ^^ space
          ^^ self#visit_Taction location
          ^^ break 1
    end

  let main ~notify_back ~doc =
    (new print ~notify_back ~doc)#visit_lexer_definition
end

module Cmts = CCSet.Make (struct
  type t = Lexer.comment * Range.t

  let compare (_, r1) (_, r2) = Range.compare r1 r2 |> Ordering.to_int
end)

let attach_comments =
  let v =
    object
      inherit [_] Syntax.syntax_map
    end
  in
  v#visit_lexer_definition ()

let render ~(notify_back : notify_back) =
  let open PPrint in
  let open Syntax in
  let bag_of_comments : Cmts.t ref =
    Lexer.get_comments ()
    |> L.map (fun (c : Lexer.comment) -> (c, Range.of_lexical_positions c.p))
    |> Cmts.of_list |> ref
  in

  let ( // ) d1 d2 = d1 ^^ hardline ^^ d2 in
  let ( ^-^ ) d1 d2 = d1 ^^ space ^^ d2 in
  let opt = O.map_or ~default:empty in
  let text = string in

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

      method private visit_regexp :
          'env 'a. regular_expression_syntax -> document =
        fun regexp -> self#visit_regular_expression_syntax () regexp

      method private visit_charclass = self#visit_character_class_syntax ()

      (* --------------------------------------- *)

      (* Comments may only appear before located syntax nodes. *)
      method! visit_located v env located =
        log_info ~notify_back "There are %d comments left to render in the bag."
          (Cmts.cardinal !bag_of_comments);

        let comment_texts : string list ref = ref [] in
        let range_loc = Range.of_lexical_positions located.p in
        let ok_comments =
          !bag_of_comments
          |> Cmts.filter_map (fun ((cmt, range_cmt) as elt) ->
              let open Range in
              log_info ~notify_back "comment vs located node: %s <= %s"
                (show range_cmt) (show range_loc);
              match compare range_cmt range_loc with
              | Lt ->
                  log_info ~notify_back
                    "comment comes before node. consuming and updating bag.";
                  comment_texts := cmt.v :: !comment_texts;
                  Some elt
              | _ -> None)
        in
        bag_of_comments := Cmts.diff !bag_of_comments ok_comments;
        let comments =
          L.(!comment_texts |> rev |> map string |> separate (break 1))
        in
        comments ^/^ super#visit_located v env located

      method! visit_action _ =
        self#with_located (fun v ->
            surround 2 1 lbrace (v |> String.trim |> text) rbrace)

      method! visit_lexer_definition _ lexer_definition =
        opt (self#visit_action ()) lexer_definition.header
        // opt (self#visit_action ()) lexer_definition.refill_handler
        // separate_map
             (hardline ^^ break 1)
             (self#visit_located self#visit_named_regexp ())
             lexer_definition.named_regexps
        //
        let h, t = L.take_drop 1 lexer_definition.entrypoints in
        let render_entries start =
          separate_map
            (hardline ^^ break 1)
            (fun e -> text start ^-^ self#visit_located self#visit_entry () e)
        in
        render_entries "rule" h // render_entries "and" t
        // opt (self#visit_action ()) lexer_definition.trailer

      method! visit_named_regexp _ named_regexp =
        group @@ text "let" ^-^ text named_regexp.name.v ^-^ text "="
        ^/^ self#visit_located self#visit_regular_expression_syntax ()
              named_regexp.regexp

      method! visit_entry _ entry =
        group @@ text entry.name.v
        ^-^ separate_map space (Located.value >> text) entry.args
        ^-^ equals ^/^ group @@ nest 2
        @@ (if entry.shortest.v then text "shortest" else text "parse")
        ^/^ nest 2
        @@ separate_map
             (break 1 ^-^ text "|" ^^ space)
             (self#visit_case ()) entry.clauses

      method visit_case _ (regexp, action) =
        group
        @@ self#with_located self#visit_regexp regexp
        ^/^ self#visit_action () action

      method! visit_Wildcard _ = self#with_located (fun _ -> text "_")
      method! visit_EOF _ = self#with_located (fun _ -> text "_")

      method! visit_Character _ =
        self#with_located (char_of_int >> Char.escaped >> text >> squotes)

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
        self#with_located (fun re ->
            surround 2 1 lparen (self#visit_regexp re) rparen)

      method! visit_As _ re ident =
        self#with_located
          (fun re -> surround 2 1 lparen (self#visit_regexp re) rparen)
          re
        ^/^ text "as"
        ^-^ self#with_located text ident

      method! visit_CharSet _ =
        self#with_located (fun v ->
            surround 2 1 lbracket (self#visit_charclass v) rbracket)

      method! visit_Union _ cls1 cls2 =
        group
        @@ self#with_located self#visit_charclass cls1
        ^^ self#with_located self#visit_charclass cls2

      method! visit_Complement _ cls =
        caret ^^ self#with_located self#visit_charclass cls

      method! visit_Range _ c1 c2 =
        self#visit_Character () c1 ^^ minus ^^ self#visit_Character () c2 (**)
    end
  in
  v#visit_lexer_definition ()
