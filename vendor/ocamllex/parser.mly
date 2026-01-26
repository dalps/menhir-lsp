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

let regexp_for_string s =
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
%type <Syntax.regular_expression_syntax located * regular_expression> regexp
%type <character_class_syntax located * Cset.t> char_class
%type <character_class_syntax located * Cset.t> char_class1

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
    { let re, r = regexp in
      let range = (startp let_, endp re) in
      let res = { name; regexp = re } in 
      Hashtbl.add named_regexps name.v @@ (locate range res, r);
      res }

refill_handler:
      "refill" a = Taction { a }

(* [menhir-lsp] located name and args. *)
definition:
    name = located(Tident) args = list(located(Tident)) "=" shortest = located(parse_or_shortest) clauses = entry
    { let _range = (startp shortest, endp shortest) in
      {name ; shortest ; args ; clauses} }

parse_or_shortest:
    "parse"     { true }
  | "shortest"  { false }

entry:
    option("|") l = separated_nonempty_list("|", case) { l }

case:
    re = regexp a = Taction
        { (fst re, a) }

(* The semantic actions are really ugly because they produce two things: in the first component, we wrap each regexp AST node with its region in the source file, the second component preserves the original semantics of Ocamllex that computes character sets and validates references. *)
regexp:
    u = located("_")
        { locate u.p @@ CharSet (locate u.p @@ Wildcard u), Characters Cset.all_chars }
  | u = located(Teof)
        { locate u.p @@ EOF u, Eof }
  | c = located(Tchar)
        { locate c.p @@ CharSet (locate c.p @@ Character c), Characters (Cset.singleton c.v) }
  | s = Tstring
        { locate s.p @@ String s, regexp_for_string s.v }
  | lbr = located("[") cls = char_class rbr = located("]")
        { let cls, cset = cls in
          locate (startp lbr, endp rbr) @@ CharSet cls, Characters cset }
  | re = regexp op = located("*")
        { let re, r = re in
          locate (startp re, endp op) @@ Rep re, Repetition r }
  | re = regexp op = located("?")
        { let re, r = re in
          locate (startp re, endp op) @@ Option re, Alternative(Epsilon, r) }
  | re = regexp op = located("+")
        { let re, r = re in
          locate (startp re, endp op) @@ Rep1 re, Sequence(Repetition (remove_as r), r) }
  | re1 = regexp "#" re2 = regexp
        {
          let re1, r1 = re1 in
          let re2, r2 = re2 in
          let as_cset re = 
            try
              as_cset re.v
            with Cset.Bad ->
              let explanation =
                match re.v with
                  Repetition _ 
                | Sequence _ -> "\nA sequence is not a valid character set." 
                | _ -> ""
              in
              raise (SyntaxError (locate re.p @@ "character set expected.\n\nThis regexp must be a character set defined with […] or a single character expression or underscore _." ^ explanation))
          in
          let s1 = as_cset (locate re1.p r1)
          and s2 = as_cset (locate re2.p r2)
          in
          locate (startp re1, endp re2) @@ CharSetDifference (re1, re2), Characters (Cset.diff s1 s2)
        }
  | re1 = regexp "|" re2 = regexp
        { let re1, r1 = re1 in
          let re2, r2 = re2 in
          locate (startp re1, endp re2) @@ Alt (re1, re2), Alternative (r1, r2) }
  | re1 = regexp re2 = regexp %prec CONCAT
        { let re1, r1 = re1 in
          let re2, r2 = re2 in
          locate (startp re1, endp re2) @@ Seq (re1, re2), Sequence (r1, r2) }
  | lpr = located("(") re = regexp rpr = located(")")
        { let re, r = re in
          locate (startp lpr, endp rpr) (Group re), r }
  | ide = located(Tident)
        { try
            let _re, r = Hashtbl.find named_regexps ide.v in
            locate ide.p @@ Ref ide, r
          with Not_found ->
            let msg = Printf.sprintf "Reference to unbound regexp name `%s'.\n" ide.v in
            raise (SyntaxError (locate ide.p msg)) }
  | re = regexp "as" ide = located(ident)
        { let re, r = re in
          locate (startp re, endp ide) @@ As (re, ide), Bind (r, ide) }

ident:
  ide = Tident { ide }

(* [menhir-lsp] This rule produces two things as well: AST annotated with source locations * character set. *)
char_class:
    op = located("^") cls = char_class1
        { let cls, cset = cls in
          locate (startp op, endp cls) @@ Complement cls,
            Cset.complement cset }
  | cls = char_class1
        { cls }

char_class1:
    c1 = located(Tchar) "-" c2 = located(Tchar)
        { locate (startp c1, endp c2) @@ Range (c1, c2),
            Cset.interval c1.v c2.v }
  | c = located(Tchar)
        { locate c.p @@ Character c, Cset.singleton c.v }
  | cls1 = char_class1 cls2 = char_class1 %prec CONCAT
        { let cls1, cset1 = cls1 in
          let cls2, cset2 = cls2 in
          locate (startp cls1, endp cls2) @@ Union (cls1, cls2),
            Cset.union cset1 cset2 }


(* -------------------------------------------------------------------------- *)

(* [located(X)] recognizes the same language as [X] and converts the resulting
   value from type ['a] to type ['a located]. *)

located(X):
  x = X
    { locate $loc x }

%%
