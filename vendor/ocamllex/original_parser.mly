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
(* [menhir-lsp] *with token aliases and no semantics. *)

%token <string> Tident "x"
%token <int> Tchar "'c'"
%token <string> Tstring "\"foo\""

/* We consider this position to be the beginning of the rule body
   (list of clauses). It's used when reporting non-exhaustive rules. */
%token (* <Lexing.position> *) Tparse "parse" Tparse_shortest "shortest"

/* An action is represented as (loc, end_pos)
   where loc is the location of the user-defined OCaml code within
   curly braces and end_pos is the position of the closing brace
   that will be used as the end delimiter of the rule body. */
%token <string Located.located> (* <Syntax.location * Lexing.position> *) Taction "{ .. }"

%token Trule "rule" Tand "and" Tequal "=" Tend "EOF" Tor "|" Tunderscore "_" Teof "eof"
       Tlbracket "[" Trbracket "]" Trefill "refill"
%token Tstar "*" Tmaybe "?" Tplus "+" Tlparen "(" Trparen ")" Tcaret "^" Tdash "-" Tlet "let" Tas "as" Thash "#"

%right Tas
%left Tor
%nonassoc CONCAT
%nonassoc Tmaybe Tstar Tplus
%left Thash
%nonassoc Tident Tchar Tstring Tunderscore Teof Tlbracket Tlparen

%start lexer_definition
%type <unit> lexer_definition

%%

lexer_definition:
    header named_regexps refill_handler Trule definition other_definitions
    header Tend
        { () }
;
header:
    Taction
        { () }
  | /*epsilon*/
        { () }
;
named_regexps:
    named_regexps Tlet Tident Tequal regexp
        { () }
  | /*epsilon*/
        { () }
;
other_definitions:
    other_definitions Tand definition
        { () }
  | /*epsilon*/
        { () }
;
refill_handler:
  | Trefill Taction { () }
  | /*empty*/ { () }
;
definition:
    Tident arguments Tequal Tparse entry
        { () }
  |  Tident arguments Tequal Tparse_shortest entry
        { () }
;

arguments:
    Tident arguments        { () }
|     /*epsilon*/           { () }
;


entry:
    case rest_of_entry
        { () }
|   Tor case rest_of_entry
        { () }
;

rest_of_entry:
    rest_of_entry Tor case
        { () }
  |
        { () }
;
case:
    regexp Taction
        { () }
;
regexp:
    Tunderscore
        { () }
  | Teof
        { () }
  | Tchar
        { () }
  | Tstring
        { () }
  | Tlbracket char_class Trbracket
        { () }
  | regexp Tstar
        { () }
  | regexp Tmaybe
        { () }
  | regexp Tplus
        { () }
  | regexp Thash regexp
        { () }
  | regexp Tor regexp
        { () }
  | regexp regexp %prec CONCAT
        { () }
  | Tlparen regexp Trparen
        { () }
  | Tident
        { () }
  | regexp Tas ident
        { () }
;

ident:
  Tident { () }
;

char_class:
    Tcaret char_class1
        { () }
  | char_class1
        { () }
;
char_class1:
    Tchar Tdash Tchar
        { () }
  | Tchar
        { () }
  | char_class1 char_class1 %prec CONCAT
        { () }
;

%%