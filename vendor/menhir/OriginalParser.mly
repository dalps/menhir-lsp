%start grammar
%token <Syntax.raw_action> ACTION
%token <string Located.located> ANGLED
%token <Attribute.attribute> ATTRIBUTE
%token BAR
%token COLON
%token COLONEQUAL
%token COMMA
%token EOF
%token EQUAL
%token EQUALEQUAL
%token <Attribute.attribute> GRAMMARATTRIBUTE
%token <string Located.located> HEADER
%token INLINE
%token LEFT
%token LET
%token <string Located.located> LID
%token LPAREN
%token MERGE
%token NONASSOC
%token ON_ERROR_REDUCE
%token PARAMETER
%token PERCENTATTRIBUTE
%token <string Located.located Lazy.t> PERCENTPERCENT
%token PLUS
%token PREC
%token PUBLIC
%token <string Located.located> QID
%token QUESTION
%token RIGHT
%token RPAREN
%token SEMI
%token STAR
%token START
%token TILDE
%token TOKEN
%token TYPE
%token <string Located.located> UID
%token UNDERSCORE
%nonassoc no_optional_bar
%nonassoc BAR
%type <unit> grammar
%type <unit> producer
%type <unit> production
%on_error_reduce old_rule
%on_error_reduce list_ATTRIBUTE_
%on_error_reduce action_expression
%on_error_reduce separated_nonempty_list_COMMA_symbol_
%on_error_reduce separated_nonempty_list_COMMA_pattern_
%on_error_reduce loption_delimited_LPAREN_separated_nonempty_list_COMMA_lax_actual__RPAREN__
%on_error_reduce loption_delimited_LPAREN_separated_nonempty_list_COMMA_expression__RPAREN__
%%

option_COMMA_:
  
    {} [@name none_COMMA]
| COMMA
    {} [@name some_COMMA]

option_QID_:
  
    {} [@name none_QID]
| QID
    {} [@name some_QID]

option_merge_function_:
  
    {} [@name none_merge_function]
| MERGE ACTION
    {} [@name some_merge_function]

option_ocamltype_:
  
    {} [@name none_ocamltype]
| ANGLED
    {} [@name some_ocamltype]

boption_PUBLIC_:
  
    {} [@name none_PUBLIC]
| PUBLIC
    {} [@name some_PUBLIC]

loption_delimited_LPAREN_separated_nonempty_list_COMMA_expression__RPAREN__:
  
    {} [@name none_delimited_LPAREN_separated_nonempty_list_COMMA_expression__RPAREN_]
| LPAREN separated_nonempty_list_COMMA_expression_ RPAREN
    {} [@name some_delimited_LPAREN_separated_nonempty_list_COMMA_expression__RPAREN_]

loption_delimited_LPAREN_separated_nonempty_list_COMMA_lax_actual__RPAREN__:
  
    {} [@name none_delimited_LPAREN_separated_nonempty_list_COMMA_lax_actual__RPAREN_]
| LPAREN separated_nonempty_list_COMMA_lax_actual_ RPAREN
    {} [@name some_delimited_LPAREN_separated_nonempty_list_COMMA_lax_actual__RPAREN_]

loption_delimited_LPAREN_separated_nonempty_list_COMMA_strict_actual__RPAREN__:
  
    {} [@name none_delimited_LPAREN_separated_nonempty_list_COMMA_strict_actual__RPAREN_]
| LPAREN separated_nonempty_list_COMMA_strict_actual_ RPAREN
    {} [@name some_delimited_LPAREN_separated_nonempty_list_COMMA_strict_actual__RPAREN_]

loption_delimited_LPAREN_separated_nonempty_list_COMMA_symbol__RPAREN__:
  
    {} [@name none_delimited_LPAREN_separated_nonempty_list_COMMA_symbol__RPAREN_]
| LPAREN separated_nonempty_list_COMMA_symbol_ RPAREN
    {} [@name some_delimited_LPAREN_separated_nonempty_list_COMMA_symbol__RPAREN_]

loption_separated_nonempty_list_COMMA_pattern__:
  
    {} [@name none_separated_nonempty_list_COMMA_pattern_]
| separated_nonempty_list_COMMA_pattern_
    {} [@name some_separated_nonempty_list_COMMA_pattern_]

list_ATTRIBUTE_:
  
    {} [@name nil_ATTRIBUTE]
| ATTRIBUTE list_ATTRIBUTE_
    {} [@name cons_ATTRIBUTE]

list_SEMI_:
  
    {} [@name nil_SEMI]
| SEMI list_SEMI_
    {} [@name cons_SEMI]

list_declaration_:
  
    {} [@name nil_declaration]
| declaration list_declaration_
    {} [@name cons_declaration]

list_producer_:
  
    {} [@name nil_producer]
| producer list_producer_
    {} [@name cons_producer]

list_rule_:
  
    {} [@name nil_rule]
| old_rule list_rule_
    {} [@name cons_rule]
| new_rule list_rule_
    {} [@name cons_rule]

nonempty_list_ATTRIBUTE_:
  ATTRIBUTE
    {} [@name one_ATTRIBUTE]
| ATTRIBUTE nonempty_list_ATTRIBUTE_
    {} [@name more_ATTRIBUTE]

separated_nonempty_list_BAR_production_:
  production
    {} [@name one_BAR_production]
| production BAR separated_nonempty_list_BAR_production_
    {} [@name more_BAR_production]

separated_nonempty_list_BAR_production_group_:
  production_group
    {} [@name one_BAR_production_group]
| production_group BAR separated_nonempty_list_BAR_production_group_
    {} [@name more_BAR_production_group]

separated_nonempty_list_COMMA_expression_:
  expression
    {} [@name one_COMMA_expression]
| expression COMMA separated_nonempty_list_COMMA_expression_
    {} [@name more_COMMA_expression]

separated_nonempty_list_COMMA_lax_actual_:
  lax_actual
    {} [@name one_COMMA_lax_actual]
| lax_actual COMMA separated_nonempty_list_COMMA_lax_actual_
    {} [@name more_COMMA_lax_actual]

separated_nonempty_list_COMMA_pattern_:
  pattern
    {} [@name one_COMMA_pattern]
| pattern COMMA separated_nonempty_list_COMMA_pattern_
    {} [@name more_COMMA_pattern]

separated_nonempty_list_COMMA_strict_actual_:
  strict_actual
    {} [@name one_COMMA_strict_actual]
| strict_actual COMMA separated_nonempty_list_COMMA_strict_actual_
    {} [@name more_COMMA_strict_actual]

separated_nonempty_list_COMMA_symbol_:
  symbol
    {} [@name one_COMMA_symbol]
| symbol COMMA separated_nonempty_list_COMMA_symbol_
    {} [@name more_COMMA_symbol]

separated_nonempty_list_option_COMMA__nonterminal_:
  LID
    {} [@name one_option_COMMA__nonterminal]
| LID option_COMMA_ separated_nonempty_list_option_COMMA__nonterminal_
    {} [@name more_option_COMMA__nonterminal]

separated_nonempty_list_option_COMMA__strict_actual_:
  strict_actual
    {} [@name one_option_COMMA__strict_actual]
| strict_actual option_COMMA_ separated_nonempty_list_option_COMMA__strict_actual_
    {} [@name more_option_COMMA__strict_actual]

separated_nonempty_list_option_COMMA__symbol_:
  symbol
    {} [@name one_option_COMMA__symbol]
| symbol option_COMMA_ separated_nonempty_list_option_COMMA__symbol_
    {} [@name more_option_COMMA__symbol]

separated_nonempty_list_option_COMMA__terminal_alias_attrs_:
  UID option_QID_ list_ATTRIBUTE_
    {} [@name one_option_COMMA__terminal_alias_attrs]
| UID option_QID_ list_ATTRIBUTE_ option_COMMA_ separated_nonempty_list_option_COMMA__terminal_alias_attrs_
    {} [@name more_option_COMMA__terminal_alias_attrs]

grammar:
  list_declaration_ PERCENTPERCENT list_rule_ postlude
    {}

declaration:
  HEADER
    {}
| TOKEN option_ocamltype_ separated_nonempty_list_option_COMMA__terminal_alias_attrs_
    {}
| START option_ocamltype_ separated_nonempty_list_option_COMMA__nonterminal_
    {}
| TYPE ANGLED separated_nonempty_list_option_COMMA__strict_actual_
    {}
| priority_keyword separated_nonempty_list_option_COMMA__symbol_
    {}
| PARAMETER ANGLED
    {}
| GRAMMARATTRIBUTE
    {}
| PERCENTATTRIBUTE separated_nonempty_list_option_COMMA__strict_actual_ nonempty_list_ATTRIBUTE_
    {}
| ON_ERROR_REDUCE separated_nonempty_list_option_COMMA__strict_actual_
    {}
| MERGE ACTION
    {}
| SEMI
    {}
| PUBLIC
    {}
| INLINE
    {}
| COLON
    {}
| LET
    {}
| EOF
    {}

priority_keyword:
  LEFT
    {}
| RIGHT
    {}
| NONASSOC
    {}

symbol:
  LID
    {}
| UID
    {}
| QID
    {}

old_rule:
  flags symbol list_ATTRIBUTE_ loption_delimited_LPAREN_separated_nonempty_list_COMMA_symbol__RPAREN__ COLON optional_bar separated_nonempty_list_BAR_production_group_ option_merge_function_ list_SEMI_
    {}

flags:
  
    {}
| PUBLIC
    {}
| INLINE
    {}
| PUBLIC INLINE
    {}
| INLINE PUBLIC
    {}

optional_bar:
   %prec no_optional_bar
    {}
| BAR
    {}

production_group:
  separated_nonempty_list_BAR_production_ ACTION list_ATTRIBUTE_
    {} [@name none_precedence]
| separated_nonempty_list_BAR_production_ ACTION precedence list_ATTRIBUTE_
    {} [@name some_precedence]

precedence:
  PREC symbol
    {}

production:
  list_producer_
    {} [@name none_precedence]
| list_producer_ precedence
    {} [@name some_precedence]

producer:
  actual list_ATTRIBUTE_ list_SEMI_
    {} [@name none_terminated_LID_EQUAL_]
| LID EQUAL actual list_ATTRIBUTE_ list_SEMI_
    {} [@name some_terminated_LID_EQUAL_]

strict_actual:
  symbol loption_delimited_LPAREN_separated_nonempty_list_COMMA_strict_actual__RPAREN__
    {}
| strict_actual located_modifier_
    {}

actual:
  symbol loption_delimited_LPAREN_separated_nonempty_list_COMMA_lax_actual__RPAREN__
    {}
| actual located_modifier_
    {}

lax_actual:
  symbol loption_delimited_LPAREN_separated_nonempty_list_COMMA_lax_actual__RPAREN__
    {}
| actual located_modifier_
    {}
| located_branches_
    {}

modifier:
  QUESTION
    {}
| PLUS
    {}
| STAR
    {}

postlude:
  EOF
    {}
| PERCENTPERCENT
    {}

new_rule:
  boption_PUBLIC_ LET LID list_ATTRIBUTE_ loption_delimited_LPAREN_separated_nonempty_list_COMMA_symbol__RPAREN__ equality_symbol expression option_merge_function_
    {}

equality_symbol:
  COLONEQUAL
    {}
| EQUALEQUAL
    {}

expression:
  located_choice_expression_
    {}

raw_seq_expression:
  symbol_expression SEMI located_raw_seq_expression_
    {}
| pattern EQUAL symbol_expression SEMI located_raw_seq_expression_
    {}
| symbol_expression
    {}
| action_expression
    {}

symbol_expression:
  symbol loption_delimited_LPAREN_separated_nonempty_list_COMMA_expression__RPAREN__ list_ATTRIBUTE_
    {}
| located_symbol_expression_ located_modifier_ list_ATTRIBUTE_
    {}

action_expression:
  action list_ATTRIBUTE_
    {}
| precedence action list_ATTRIBUTE_
    {}
| action precedence list_ATTRIBUTE_
    {}

action:
  ACTION
    {}
| ANGLED
    {}

pattern:
  LID
    {}
| UNDERSCORE
    {}
| TILDE
    {}
| LPAREN loption_separated_nonempty_list_COMMA_pattern__ RPAREN
    {}

reversed_preceded_or_separated_nonempty_llist_BAR_branch_:
  located_raw_seq_expression_
    {} [@name none_BAR]
| BAR located_raw_seq_expression_
    {} [@name some_BAR]
| reversed_preceded_or_separated_nonempty_llist_BAR_branch_ BAR located_raw_seq_expression_
    {}

located_branches_:
  separated_nonempty_list_BAR_production_group_
    {}

located_choice_expression_:
  reversed_preceded_or_separated_nonempty_llist_BAR_branch_
    {}

located_modifier_:
  modifier
    {}

located_raw_seq_expression_:
  raw_seq_expression
    {}

located_symbol_expression_:
  symbol_expression
    {}

%%
