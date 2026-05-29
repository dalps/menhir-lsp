open Utils
open OcamllexSyntax
open PPrint

include Comment_location.Make (struct
  type syntax = Syntax.main

  include Located
  include Range

  (* let get_comments = Lexer.get_comments *)
end)

class formatter (config : Config.t) =
  let Config.{ tabsize } = config in
  let open Syntax in
  object (self)
    inherit [_] ast_reduce as super
    method zero = empty
    method plus = ( ^^ )

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

    method! visit_located visit_v env =
      render_located (super#visit_located visit_v env)

    (* --------------------------------------- *)

    method! visit_action _ =
      self#with_located (fun v ->
          let v =
            match Ocamlformat_client.format (String.trim v) with
            | Ok formatted ->
                Printf.eprintf "[ocf] '%s' --> '%s'\n\n" v formatted;
                formatted
            | Error _ -> v
          in
          surround tabsize 1 lbrace
            (v |> String.trim |> arbitrary_string)
            rbrace)

    method! visit_lexer_definition _ lexer_definition =
      let { header; entrypoints; trailer; refill_handler; named_regexps } =
        lexer_definition
      in
      optional (self#visit_action ()) header
      //// optional
             (fun handler -> text "refill" ^-^ self#visit_action () handler)
             refill_handler
      //// separate_map
             (hardline ^^ break 1)
             (self#visit_located self#visit_named_regexp ())
             named_regexps
      //// separate_mapi
             (hardline ^^ break 1)
             (fun idx (e : entry located) ->
               let opening = if idx = 0 then "rule" else "and" in
               self#with_located
                 (fun located -> text opening ^-^ self#visit_entry () located)
                 e)
             entrypoints
      //// optional (self#visit_action ()) trailer

    method! visit_named_regexp _ { name; regexp } =
      prefix tabsize 1
        (text "let" ^-^ self#with_located text name ^-^ text "=")
        (self#with_located self#visit_regexp regexp)

    method! visit_entry _ { name; shortest; args; clauses } =
      prefix tabsize 1
        (flow space
        @@ [
             self#with_located text name;
             nest tabsize @@ flow_map (break 1) (self#with_located text) args;
             equals;
             self#with_located
               ((fun v -> if v then "shortest" else "parse") >> text)
               shortest;
           ])
      @@ separate_map hardline (self#with_located (self#visit_case ())) clauses

    method! visit_case _ (regexp, action) =
      bar ^-^ nest tabsize @@ group
      @@ self#with_located self#visit_regexp regexp
      ^/^ self#visit_action () action

    method! visit_Wildcard _ = self#with_located (fun _ -> text "_")
    method! visit_EOF _ = self#with_located (fun _ -> text "eof")

    method! visit_Character _ =
      self#with_located (char_of_int >> Char.escaped >> text >> squotes)

    method! visit_Char = self#visit_Character
    method! visit_String _ = self#with_located (text >> dquotes)
    method! visit_Ref _ = self#with_located text

    (* todo: let the user decide whether to break the regexp onto multiple lines
    if it doesn't fit in one (by default, we only break alternations) *)
    method! visit_Seq _ re1 re2 =
      self#with_located self#visit_regexp re1
      ^-^ self#with_located self#visit_regexp re2

    method! visit_Alt _ re1 re2 =
      group @@ align
      @@ self#with_located self#visit_regexp re1
      ^/^ bar
      ^-^ self#with_located self#visit_regexp re2

    method! visit_CharSetDifference _ re1 re2 =
      group @@ align
      @@ self#with_located self#visit_regexp re1
      ^-^ sharp
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
        | re -> enclose lparen (self#visit_regexp re) rparen)

    method! visit_As _ re ident =
      (* To get a better idea of what is being captured, we could surround alternation and sequences with parens. *)
      self#with_located self#visit_regexp re
      ^-^ text "as"
      ^-^ self#with_located text ident

    method! visit_CharSet _ =
      self#with_located (fun v ->
          enclose lbracket (self#visit_charclass v) rbracket)

    method! visit_Union _ cls1 cls2 =
      group
      @@ self#with_located self#visit_charclass cls1
      ^-^ self#with_located self#visit_charclass cls2

    method! visit_Complement _ cls =
      caret ^^ self#with_located self#visit_charclass cls

    method! visit_Range _ c1 c2 =
      self#visit_Character () c1 ^^ minus ^^ self#visit_Character () c2 (**)
  end

(* This should really go in comment_location, but I couldn't figure out how to generalize it over the endo visitor :/ *)
let main ~config ~ast ~doc =
  let buf = Buffer.create 80 in
  let bag_of_comments = Lexer.get_comments () |> init_bag in
  let attach_vtor =
    object
      inherit [_] Syntax.ast_endo
      method! visit_located = visit_attach ~bag_of_comments ~doc
    end
  in
  attach_comments ast (attach_vtor#visit_main ()) ~bag_of_comments ~doc
  |> (new formatter config)#visit_main ()
  |> PPrint.ToBuffer.pretty 0.8 80 buf;
  Buffer.contents buf
