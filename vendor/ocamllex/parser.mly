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

let named_regexps =
  (Hashtbl.create 13 : (string located, regular_expression) Hashtbl.t)

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
%token <string> Tstring
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

%%

lexer_definition:
    header = header named_regexp* refill_handler = refill_handler? "rule" definitions = separated_list("and", definition) 
    trailer = header "EOF"
        { {header;
           refill_handler;
           entrypoints = definitions;
           trailer;
           named_regexps} }

header:
    a = Taction
        { a }
  | /*epsilon*/
        { Range.(pos_zero, pos_zero) }

named_regexp:
    "let" name = located(Tident) "=" re = regexp { Hashtbl.add named_regexps name re }

refill_handler:
      "refill" a = Taction { a }

(* [menhir-lsp] located name and args. *)
definition:
    name = located(Tident) args = list(located(Tident)) "=" "parse" clauses = entry
        { {name ; shortest=false ; args ; clauses} }
  |  name = located(Tident) args = list(located(Tident)) "=" "shortest" clauses = entry
        { {name ; shortest=true ; args ; clauses} }

entry:
    l = separated_nonempty_list("|", case) { l }

case:
    re = regexp a = Taction
        { (re, a) }

regexp:
    "_"
        { Characters Cset.all_chars }
  | Teof
        { Eof }
  | c = Tchar
        { Characters (Cset.singleton c) }
  | s = Tstring
        { regexp_for_string s }
  | "[" cls = char_class "]"
        { Characters cls }
  | re = regexp "*"
        { Repetition re }
  | re = regexp "?"
        { Alternative(Epsilon, re) }
  | re = regexp "+"
        { Sequence(Repetition (remove_as re), re) }
  | re1 = regexp "#" re2 = regexp
        {
          let s1 = as_cset re1
          and s2 = as_cset re2 in
          Characters (Cset.diff s1 s2)
        }
  | re1 = regexp "|" re2 = regexp
        { Alternative(re1, re2) }
  | re1 = regexp re2 = regexp %prec CONCAT
        { Sequence(re1, re2) }
  | "(" re = regexp ")"
        { re }
  | ide = located(Tident)
        { try
            Hashtbl.find named_regexps ide
          with Not_found ->
            let p = $symbolstartpos in
            Printf.eprintf "File \"%s\", line %d, character %d:\n\
                             Reference to unbound regexp name `%s'.\n"
                           p.Lexing.pos_fname p.Lexing.pos_lnum
                           (p.Lexing.pos_cnum - p.Lexing.pos_bol)
                           ide.v;
            exit 2 }
  | re = regexp "as" ide = located(ident) { Bind (re, ide) }

ident:
  ide = Tident { ide }

char_class:
    "^" cls = char_class1
        { Cset.complement cls }
  | cls = char_class1
        { cls }

char_class1:
    c1 = Tchar "-" c2 = Tchar
        { Cset.interval c1 c2 }
  | c = Tchar
        { Cset.singleton c }
  | cls1 = char_class1 cls2 = char_class1 %prec CONCAT
        { Cset.union cls1 cls2 }


(* -------------------------------------------------------------------------- *)

(* [located(X)] recognizes the same language as [X] and converts the resulting
   value from type ['a] to type ['a located]. *)

located(X):
  x = X
    { Located.locate $loc x }

%%
