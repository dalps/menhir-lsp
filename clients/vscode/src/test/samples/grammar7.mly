// Same as grammar6.mly but unambiguous.

%token Ta "a" Tb "b" Tc "c"

%start <unit> rule_S

%%

rule_S:
  "a" rule_A "c" {}

rule_A:
  | rule_A "b" "b" {}
  | "b" {}