// An ambiguous grammar with a shift/reduce conflict.

%token Ta "a" Tb "b" Tc "c"

%start <unit> rule_S

%%

rule_S:
  "a" rule_A "c" {}

rule_A:
  | "b" rule_A "b" {}
  | "b" {}