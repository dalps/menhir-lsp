/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* The grammar for lexer definitions */

%{
open Syntax
open Located

(* Auxiliaries for the parser. *)

let raise exn =
  Hashtbl.reset named_regexps;
  Stdlib.raise exn

let _regexp_for_string s =
  let rec re_string n =
    if n >= String.length s then Epsilon
    else if succ n = String.length s then
      Characters (Cset.singleton (Char.code s.[n]))
    else
      Sequence
        (Characters(Cset.singleton (Char.code s.[n])),
         re_string (succ n))
  in re_string 0

let rec remove_as = function
  | Bind (e,_) -> remove_as e
  | Epsilon|Eof|Characters _ as e -> e
  | Sequence (e1, e2) -> Sequence (remove_as e1, remove_as e2)
  | Alternative (e1, e2) -> Alternative (remove_as e1, remove_as e2)
  | Repetition e -> Repetition (remove_as e)

let rec as_cset = function
  | Characters s -> s
  | Alternative (e1, e2) -> Cset.union (as_cset e1) (as_cset e2)
  | _ -> raise Cset.Bad

let _remove_as = remove_as
let _as_cset = as_cset

%}

%token <string> Tident
%token <int> Tchar
%token <string Located.located> Tstring
%token <Syntax.location> Taction
%token Trule "rule" Tparse "parse" Tparse_shortest "shortest" Tand "and" Tequal "=" Tend "EOF" Tor "|" Tunderscore "_" Teof
       Tlbracket "[" Trbracket "]" Trefill "refill"
%token Tstar "*" Tmaybe "?" Tplus "+" Tlparen "(" Trparen ")" Tcaret "^" Tdash "-" Tlet "let" Tas "as" Thash "#"

%right "as"
%left "|"
%nonassoc CONCAT
%nonassoc "?" "*" "+"
%left "#"
%nonassoc Tident Tchar Tstring "_" Teof "[" "("

%start lexer_definition
%type <Syntax.lexer_definition> lexer_definition
%type <Syntax.regular_expression_syntax located> regexp

%type <(character_class_syntax * Cset.t) located> char_class
%type <(character_class_syntax * Cset.t) located> char_class1

%%

lexer_definition:
    header = header? named_regexps_l = named_regexp* refill_handler = refill_handler? "rule" definitions = separated_list("and", definition) 
    trailer = header? "EOF"
        { let v = {
            header;
            refill_handler;
            entrypoints = definitions;
            trailer;
            named_regexps = named_regexps_l
        } in
        Hashtbl.reset named_regexps;
        v }

header:
    a = Taction { a }

named_regexp:
    let_ = located("let") name = located(Tident) "=" regexp = regexp 
    {
        let range = (startp let_, endp regexp) in
        let res = { name; regexp } in 
        Hashtbl.add named_regexps name.v @@ locate range res;
        res
    }

refill_handler:
      "refill" a = Taction { a }

(* [menhir-lsp] located name and args. *)
definition:
    name = located(Tident) args = list(located(Tident)) "=" shortest = located(parse_or_shortest) clauses = entry
        { 
            let _range = (startp shortest, endp shortest) in
            {name ; shortest ; args ; clauses} }

parse_or_shortest:
    "parse"     { true }
  | "shortest"  { false }

entry:
    option("|") l = separated_nonempty_list("|", case) { l }

case:
    re = regexp a = Taction
        { (re, a) }

regexp:
    u = located("_")
        { locate u.p @@ CharSet (locate u.p @@ Wildcard u) }
  | u = located(Teof)
        { locate u.p @@ EOF u }
  | c = located(Tchar)
        { locate c.p @@ CharSet (locate c.p @@ Character c) }
  | s = Tstring
        { locate s.p @@ String s }
  | lbr = located("[") cls = char_class rbr = located("]")
        { locate (startp lbr, endp rbr) @@ CharSet { cls with v = cls.v |> fst } }
  | re = regexp op = located("*")
        { locate (startp re, endp op) @@ Rep re }
  | re = regexp op = located("?")
        { locate (startp re, endp op) @@ Option re }
  | re = regexp op = located("+")
        { locate (startp re, endp op) @@ Rep1 re }
  | re1 = regexp "#" re2 = regexp
        {
          let _as_cset re = 
            try
              as_cset re.v
            with Cset.Bad ->
              raise (SyntaxError (locate re.p "character set expected."))
          in
          locate (startp re1, endp re2) @@ CharSetDifference (re1, re2)
        }
  | re1 = regexp "|" re2 = regexp
        { locate (startp re1, endp re2) @@ Alt (re1, re2) }
  | re1 = regexp re2 = regexp %prec CONCAT
        { locate (startp re1, endp re2) @@ Seq (re1, re2) }
  | lpr = located("(") re = regexp rpr = located(")")
        { locate (startp lpr, endp rpr) (Group re) }
  | ide = located(Tident)
        { try
            Hashtbl.find named_regexps ide.v |> ignore;
            locate ide.p @@ Ref ide
          with Not_found ->
            let msg = Printf.sprintf "Reference to unbound regexp name `%s'.\n" ide.v in
            raise (SyntaxError (locate ide.p msg)) }
  | re = regexp "as" ide = located(ident)
      { locate (startp re, endp ide) @@ As (re, ide) }

ident:
  ide = Tident { ide }

char_class:
    op = located("^") cls = char_class1
        { locate (startp op, endp cls) (Complement (locate cls.p (cls.v |> fst)), Cset.complement (cls.v |> snd)) }
  | cls = char_class1
        { cls }

char_class1:
    c1 = located(Tchar) "-" c2 = located(Tchar)
        { locate (startp c1, endp c2) (Range (c1, c2), Cset.interval c1.v c2.v) }
  | c = located(Tchar)
        { locate c.p (Character c, Cset.singleton c.v) }
  | cls1 = char_class1 cls2 = char_class1 %prec CONCAT
        { locate (startp cls1, endp cls2) 
            (Union (locate cls1.p (cls1.v |> fst), locate cls2.p (cls2.v |> fst)), 
              Cset.union (cls1.v |> snd) (cls2.v |> snd)) }


(* -------------------------------------------------------------------------- *)

(* [located(X)] recognizes the same language as [X] and converts the resulting
   value from type ['a] to type ['a located]. *)

located(X):
  x = X
    { locate $loc x }

%%
