// An ambiguous grammar with a shift/reduce conflict.

%token TOKEN_A TOKEN_B Tc

%start <u> rule_S

%%

rule_S:
  TOKEN_A rule_A Tc {}

rule_A:
  | TOKEN_B rule_A TOKEN_B {}
  | TOKEN_B {}