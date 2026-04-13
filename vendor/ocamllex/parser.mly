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

let last_bar = ref None
%}

%token <string> Tident
%token <int> Tchar
%token <string Located.located> Tstring
%token <string Located.located> Taction
%token Trule "rule" Tparse "parse" Tparse_shortest "shortest" Tand "and" Tequal "=" Tend "EOF" Tor "|" Tunderscore "_" Teof "eof"
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
    header = header? named_regexps_l = located(named_regexp)* refill_handler = refill_handler? d0 = definition("rule") ds = definition("and")*
    trailer = header? "EOF"
        { let v = {
            header;
            refill_handler;
            entrypoints = d0 :: ds;
            trailer;
            named_regexps = named_regexps_l
        } in
        Hashtbl.reset named_regexps;
        v } [@name lexer]

header:
    a = Taction { a } [@name header]

named_regexp:
    "let" name = located(Tident) "=" regexp = regexp 
    { let re, r = regexp in
      let res = { name; regexp = re } in 
      Hashtbl.add named_regexps name.v @@ (locate $loc res, r);
      res } [@name named_regexp]

refill_handler:
      "refill" a = Taction { a } [@name refill_handler]

(* [menhir-lsp] located name and args. *)
(* [opening] is one of "rule" or "and". *)
definition(opening):
    opening name = located(Tident) args = list(located(Tident)) "=" shortest = located(parse_or_shortest) clauses = entry
    { locate $loc {name ; shortest ; args ; clauses} } [@name rule_definition]

%inline parse_or_shortest:
    "parse"     { false } [@name parse]
  | "shortest"  { true } [@name shortest]

mandatory_bar:
  "|" { last_bar := Some $startpos }

optional_bar:
  /* epsilon */ { () }
| "|"
    { last_bar := Some $startpos }
    
entry:
    optional_bar l = separated_nonempty_list(mandatory_bar, case) { l } [@name entry]

case:
    re = regexp a = Taction
        { let re, _ = re in
          let startpos = Option.value ~default:(startp re) !last_bar in
          locate (startpos, $endpos) (re, a) } [@name case]

(* The semantic actions are really ugly because they produce two things: in the first component, we wrap each regexp AST node with its region in the source file, in the second component we preserve the original semantics of Ocamllex that resolve and validate both character sets and references. *)
regexp:
    u = located("_")
        { locate $loc @@ Wildcard u, Characters Cset.all_chars } [@name regexp_wildcard]
  | u = located(Teof)
        { locate $loc @@ EOF u, Eof } [@name regexp_eof]
  | c = located(Tchar)
        { locate $loc @@ Char c, Characters (Cset.singleton c.v) } [@name regexp_character]
  | s = Tstring
        { locate s.p @@ String s, regexp_for_string s.v } [@name regexp_string]
  | "[" cls = char_class "]"
        { let cls, cset = cls in
          locate $loc @@ CharSet cls, Characters cset } [@name regexp_charset]
  | re = regexp "*"
        { let re, r = re in
          locate $loc @@ Rep re, Repetition r } [@name regexp_repetition]
  | re = regexp "?"
        { let re, r = re in
          locate $loc @@ Option re, Alternative(Epsilon, r) } [@name regexp_option]
  | re = regexp "+"
        { let re, r = re in
          locate $loc @@ Rep1 re, Sequence(Repetition (remove_as r), r) } [@name regexp_repetition1]
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
          locate $loc @@ CharSetDifference (re1, re2), Characters (Cset.diff s1 s2)
        } [@name regexp_difference]
  | re1 = regexp "|" re2 = regexp
        { let re1, r1 = re1 in
          let re2, r2 = re2 in
          locate $loc @@ Alt (re1, re2), Alternative (r1, r2) } [@name regexp_alternative]
  | re1 = regexp re2 = regexp %prec CONCAT
        { let re1, r1 = re1 in
          let re2, r2 = re2 in
          locate $loc @@ Seq (re1, re2), Sequence (r1, r2) } [@name regexp_sequence]
  | "(" re = regexp ")"
        { let re, r = re in
          locate $loc (Group re), r } [@name regexp_group]
  | ide = located(Tident)
        { try
            let _re, r = Hashtbl.find named_regexps ide.v in
            locate ide.p @@ Ref ide, r
          with Not_found ->
            let msg = Printf.sprintf "Reference to unbound regexp name `%s'.\n" ide.v in
            raise (SyntaxError (locate ide.p msg)) } [@name regexp_reference]
  | re = regexp "as" ide = located(ident)
        { let re, r = re in
          locate $loc @@ As (re, ide), Bind (r, ide) } [@name regexp_binding]

ident:
  ide = Tident { ide } [@name identifier]

(* [menhir-lsp] This rule produces two things as well: AST annotated with source locations * character set. *)
char_class:
    "^" cls = char_class1
        { let cls, cset = cls in
          locate $loc @@ Complement cls,
            Cset.complement cset } [@name charclass_complement]
  | cls = char_class1
        { cls } [@name charclass1]

char_class1:
    c1 = located(Tchar) "-" c2 = located(Tchar)
        { locate $loc @@ Range (c1, c2),
            Cset.interval c1.v c2.v } [@name charclass_range]
  | c = located(Tchar)
        { locate $loc @@ Character c, Cset.singleton c.v } [@name charclass_character]
  | cls1 = char_class1 cls2 = char_class1 %prec CONCAT
        { let cls1, cset1 = cls1 in
          let cls2, cset2 = cls2 in
          locate $loc @@ Union (cls1, cls2),
            Cset.union cset1 cset2 } [@name charclass_union]


(* -------------------------------------------------------------------------- *)

(* [located(X)] recognizes the same language as [X] and converts the resulting
   value from type ['a] to type ['a located]. *)

located(X):
  x = X
    { locate $loc x } [@name located]

%%
