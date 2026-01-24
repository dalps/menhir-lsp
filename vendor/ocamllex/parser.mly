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

let regexp_for_string s loc =
  let rec re_string n =
    if n >= String.length s then Epsilon
    else
    let c = (Char.code s.[n]) in
    let c = Characters (locate loc @@ (Character (locate loc c), Cset.singleton c)) in
    if succ n = String.length s then
      c
    else
      Sequence
        (locate loc c,
        (locate loc @@ re_string (succ n)))
  in re_string 0

let rec remove_as = function
  | { v; p } ->
    match v with
    | Bind (e,_) -> remove_as e
    | Epsilon|Eof|Characters _ as e -> locate p e
    | Sequence (e1, e2) -> locate p @@ Sequence (remove_as e1, remove_as e2)
    | Alternative (e1, e2) -> locate p @@ Alternative (remove_as e1, remove_as e2)
    | Ref ide -> remove_as @@ (Hashtbl.find named_regexps ide.v |> snd) (* [menhir-lsp] handled. *)
    | Repetition e -> locate p @@ Repetition (remove_as e)

let rec as_cset = function
  | Characters s -> snd s.v
  | Alternative (e1, e2) -> Cset.union (as_cset e1.v) (as_cset e2.v)
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
%type <Syntax.regular_expression located> regexp

%type <(character_class * Cset.t) located> char_class
%type <(character_class * Cset.t) located> char_class1

%%

lexer_definition:
    header = header named_regexps_l = named_regexp* refill_handler = refill_handler? "rule" definitions = separated_list("and", definition) 
    trailer = header "EOF"
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
    a = Taction
        { a }
  | /*epsilon*/
        { Range.(pos_zero, pos_zero) }

named_regexp:
    "let" name = located(Tident) "=" re = regexp 
    {
        Hashtbl.add named_regexps name.v (name.p, re);
        (name, re)
    }

refill_handler:
      "refill" a = Taction { a }

(* [menhir-lsp] located name and args. *)
definition:
    name = located(Tident) args = list(located(Tident)) "=" "parse" clauses = entry
        { {name ; shortest=false ; args ; clauses} }
  |  name = located(Tident) args = list(located(Tident)) "=" "shortest" clauses = entry
        { {name ; shortest=true ; args ; clauses} }

entry:
    option("|") l = separated_nonempty_list("|", case) { l }

case:
    re = regexp a = Taction
        { (re, a) }

regexp:
    u = located("_")
        { locate u.p (Characters (locate u.p (Wildcard, Cset.all_chars))) }
  | u = located(Teof)
        { locate u.p Eof }
  | c = located(Tchar)
        { locate c.p @@ Characters (locate c.p (Character c, Cset.singleton c.v)) }
  | s = Tstring
        { locate s.p (regexp_for_string s.v s.p) }
  | lbr = located("[") cls = char_class rbr = located("]")
        { locate (startp lbr, endp rbr) (Characters cls) }
  | re = regexp op = located("*")
        { locate (startp re, endp op) (Repetition re) }
  | re = regexp op = located("?")
        { 
          let p = (startp re, endp op) in
          locate p (Alternative(locate p Epsilon, re)) }
  | re = regexp op = located("+")
        {
          let p = (startp re, endp op) in
          locate p (Sequence(locate p @@ Repetition (remove_as re), re)) }
  | re1 = regexp "#" re2 = regexp
        {
          let as_cset re = 
            try
              as_cset re.v
            with Cset.Bad ->
              raise (SyntaxError (locate re.p "character set expected."))
          in
          let s1 = as_cset re1
          and s2 = as_cset re2 in
          let p = (startp re1, endp re2) in
          let cl = Difference (re1, re2) in
          locate p @@ Characters (locate p (cl, Cset.diff s1 s2))
        }
  | re1 = regexp "|" re2 = regexp
        { locate (startp re1, endp re2) @@ Alternative(re1, re2) }
  | re1 = regexp re2 = regexp %prec CONCAT
        { locate (startp re1, endp re2) @@ Sequence(re1, re2) }
  | lpr = located("(") re = regexp rpr = located(")")
        { locate (startp lpr, endp rpr) re.v }
  | ide = located(Tident)
        { try
            Hashtbl.find named_regexps ide.v |> ignore;
            locate ide.p @@ Ref ide
          with Not_found ->
            let msg = Printf.sprintf "Reference to unbound regexp name `%s'.\n" ide.v in
            raise (SyntaxError (locate ide.p msg)) }
  | re = regexp "as" ide = located(ident)
      { locate (startp re, endp ide) (Bind (re, ide)) }

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
