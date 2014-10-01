/*
 Grammar Grammar
*/
{
#include "gramgram.h"
#include "d.h"

char *
dup_code(const char *s, const char *e);
void Trace(int, char*);
#define TRACE(...) Trace(__LINE__, #__VA_ARGS__); __VA_ARGS__
}

grammar: top_level_statement*;

top_level_statement: global_code | production | include_statement;

include_statement: 'include' regex { TRACE(
  char *grammar_pathname = dup_str($n1.start_loc.s+1, $n1.end-1);
  if (parse_grammar($g, grammar_pathname, 0) < 0)
    d_fail("unable to parse grammar '%s'", grammar_pathname);
  FREE(grammar_pathname);    )
};

global_code
  : '%<' balanced_code* '%>'
    { TRACE(add_global_code($g, $n0.start_loc.s+2, $n2.end-2, $n0.start_loc.line);) }
  | curly_code { TRACE(add_global_code($g, $n0.start_loc.s+1, $n0.end-1, $n0.start_loc.line);) }
  | '${scanner' balanced_code+ '}' { TRACE(
      $g->scanner.code = dup_str($n1.start_loc.s, $n1.end);
      $g->scanner.line = $n0.start_loc.line;
      )
    }
  | '${declare' declarationtype identifier* '}' { TRACE(
      if (!d_get_number_of_children(&$n2))
     	add_declaration($g, $n2.start_loc.s, $n2.end,  $1.kind, $n2.start_loc.line);
      else {
	int i, n = d_get_number_of_children(&$n2);
	for (i = 0; i < n; i++) {
	  D_ParseNode *pn = d_get_child(&$n2, i);
	  add_declaration($g, pn->start_loc.s, pn->end,  $1.kind, pn->start_loc.line);
      
	}
      } )
    }
  | '${token' token_identifier+ '}'
  | '${pass' identifier pass_types '}' { TRACE(
      add_pass($g, $n1.start_loc.s, $n1.end,  $2.kind, $n1.start_loc.line);
      )
    }
  ;

pass_types
  : 
  | pass_type pass_types { TRACE( $$.kind = $0.kind | $1.kind; ) }
  ;

pass_type 
  : 'preorder' { TRACE( $$.kind |= D_PASS_PRE_ORDER; ) } 
  | 'postorder' { TRACE( $$.kind |= D_PASS_POST_ORDER; ) }
  | 'manual' { TRACE( $$.kind |= D_PASS_MANUAL; ) }
  | 'for_all'  { TRACE( $$.kind |= D_PASS_FOR_ALL; ) }
  | 'for_undefined' { TRACE( $$.kind |= D_PASS_FOR_UNDEFINED; ) }
  ;

declarationtype
  : 'tokenize' { TRACE( $$.kind = DECLARE_TOKENIZE; ) } 
  | 'longest_match' { TRACE( $$.kind = DECLARE_LONGEST_MATCH; ) }
  | 'whitespace' { TRACE( $$.kind = DECLARE_WHITESPACE; ) }
  | 'all_matches' { TRACE( $$.kind = DECLARE_ALL_MATCHES; ) }
  | 'set_op_priority_from_rule' { TRACE( $$.kind = DECLARE_SET_OP_PRIORITY; ) }
  | 'all_subparsers' { TRACE( $$.kind = DECLARE_STATES_FOR_ALL_NTERMS; ) }
  | 'subparser' { TRACE( $$.kind = DECLARE_STATE_FOR; ) }
  | 'save_parse_tree' { TRACE( $$.kind = DECLARE_SAVE_PARSE_TREE; ) }
  ;

token_identifier: identifier { TRACE( new_token($g, $n0.start_loc.s, $n0.end); ) };

production 
  : production_name ':' rules ';' 
  | production_name regex_production rules ';'
  | ';';
regex_production : '::=' { TRACE( 
  $g->p->regex = 1; 
) }; 

production_name : (identifier | '_') { TRACE( $g->p = new_production($g, dup_str($n0.start_loc.s, $n0.end)); ) } ;

rules : rule ('|' rule)*; 

rule : new_rule ((element element_modifier*)* simple_element element_modifier*)? rule_modifier* rule_code { TRACE(
  vec_add(&$g->p->rules, $g->r);
) };

new_rule : { TRACE( $g->r = new_rule($g, $g->p); ) };

simple_element
  : string { TRACE( $g->e = new_string($g, $n0.start_loc.s, $n0.end, $g->r); ) }
  | regex { TRACE( $g->e = new_string($g, $n0.start_loc.s, $n0.end, $g->r); ) }
  | unicode_char { TRACE( $g->e = new_utf8_char($g, $n0.start_loc.s, $n0.end, $g->r); ) }
  | identifier { TRACE( $g->e = new_ident($n0.start_loc.s, $n0.end, $g->r); ) }
  | '${scan' balanced_code+ '}' { TRACE( $g->e = new_code($g, $n1.start_loc.s, $n1.end, $g->r); ) }
  | '(' new_subrule rules ')' { TRACE(
      printf("// $1.p: %p\n// $1.r: %p\n", $1.p, $1.r);
      $g->e = new_elem_nterm($g->p, $1.r);
      $g->p = $1.p;
      $g->r = $1.r;
      vec_add(&$g->r->elems, $g->e);
    ) }
  ;

element
  : simple_element
  | bracket_code { TRACE(
      Production *p = new_internal_production($g, NULL);
      Rule *r = new_rule($g, p);
      vec_add(&p->rules, r);
      r->speculative_code.code = dup_code($n0.start_loc.s + 1, $n0.end - 1);
      r->speculative_code.line = $n0.start_loc.line;
      $g->e = new_elem_nterm(p, $g->r);
      vec_add(&$g->r->elems, $g->e);
    ) }
  | curly_code { TRACE(
      Production *p = new_internal_production($g, NULL);
      Rule *r = new_rule($g, p);
      vec_add(&p->rules, r);
      r->final_code.code = dup_code($n0.start_loc.s + 1, $n0.end - 1);
      r->final_code.line = $n0.start_loc.line;
      $g->e = new_elem_nterm(p, $g->r);
      vec_add(&$g->r->elems, $g->e);
  ) }
  ;

new_subrule : { TRACE(
  printf("// g.p: %p\n// g.r: %p\n", $g->p, $g->r);
  $$.p = $g->p;
  $$.r = $g->r;
  $g->p = new_internal_production($g, $g->p);
  $g->r = 0;
) };

element_modifier 
  : '$term' integer { TRACE( 
      if ($g->e->kind != ELEM_TERM) 
        d_fail("terminal priority on non-terminal");
      $g->e->e.term->term_priority = strtol($n1.start_loc.s, NULL, 0); 
    ) }
  | '$name' (string|regex) { TRACE( 
      if ($g->e->kind != ELEM_TERM) 
	d_fail("terminal name on non-terminal");
      $g->e->e.term->term_name = dup_str($n1.start_loc.s+1, $n1.end-1); 
    ) }
  | '/i' { TRACE( 
      if ($g->e->kind != ELEM_TERM) 
	d_fail("ignore-case (/i) on non-terminal");
      $g->e->e.term->ignore_case = 1; 
    ) }
  | '?' { TRACE( conditional_EBNF($g); ) }
  | '*' { TRACE( star_EBNF($g); ) }
  | '+' { TRACE( plus_EBNF($g); ) } 
  | '@' integer { TRACE( rep_EBNF($g, strtol($n1.start_loc.s, NULL, 0), -1); ) } 
  | '@' integer ':' integer { TRACE( rep_EBNF($g, strtol($n1.start_loc.s, NULL, 0), strtol($n3.start_loc.s, NULL, 0)); ) } 
  ;

rule_modifier : rule_assoc rule_priority | external_action;

rule_assoc
  : '$unary_op_right' { TRACE( $g->r->op_assoc = ASSOC_UNARY_RIGHT; ) }
  | '$unary_op_left' { TRACE( $g->r->op_assoc = ASSOC_UNARY_LEFT; ) }
  | '$binary_op_right' { TRACE( $g->r->op_assoc = ASSOC_BINARY_RIGHT; ) }
  | '$binary_op_left' { TRACE( $g->r->op_assoc = ASSOC_BINARY_LEFT; ) }
  | '$unary_right' { TRACE( $g->r->rule_assoc = ASSOC_UNARY_RIGHT; ) }
  | '$unary_left' { TRACE( $g->r->rule_assoc = ASSOC_UNARY_LEFT; ) }
  | '$binary_right' { TRACE( $g->r->rule_assoc = ASSOC_BINARY_RIGHT; ) }
  | '$binary_left' { TRACE( $g->r->rule_assoc = ASSOC_BINARY_LEFT; ) }
  | '$right' { TRACE( $g->r->rule_assoc = ASSOC_NARY_RIGHT; ) }
  | '$left' { TRACE( $g->r->rule_assoc = ASSOC_NARY_LEFT; ) }
  ;

rule_priority : integer { TRACE( 
  if ($g->r->op_assoc) $g->r->op_priority = strtol($n0.start_loc.s, NULL, 0); 
  else $g->r->rule_priority = strtol($n0.start_loc.s, NULL, 0); 
) };

external_action
  : '${action}' { TRACE( $g->r->action_index = $g->action_index++; ) }
  | '${action' integer '}' { TRACE( $g->r->action_index = strtol($n1.start_loc.s, NULL, 0); ) }
  ;

rule_code : speculative_code? final_code? pass_code*;

speculative_code : bracket_code { TRACE(
  $g->r->speculative_code.code = dup_code($n0.start_loc.s + 1, $n0.end - 1);
  $g->r->speculative_code.line = $n0.start_loc.line;
) };

final_code : curly_code { TRACE(
  $g->r->final_code.code = dup_code($n0.start_loc.s + 1, $n0.end - 1);
  $g->r->final_code.line = $n0.start_loc.line;
) };

pass_code : identifier ':' curly_code { TRACE(
  add_pass_code($g, $g->r, $n0.start_loc.s, $n0.end, $n2.start_loc.s+1,
    $n2.end-1, $n0.start_loc.line, $n2.start_loc.line);
) };

curly_code: '{' balanced_code* '}';
bracket_code: '[' balanced_code* ']';
balanced_code 
  : '(' balanced_code* ')' | '[' balanced_code* ']' | '{' balanced_code* '}'
  | string | identifier | regex | integer | symbols;
symbols : "[!~`@#$%^&*\-_+=|:;\\<,>.?/]";
string: "'([^'\\]|\\[^])*'";
regex: "\"([^\"\\]|\\[^])*\"";
unicode_char: "[uU]\+[0-9a-fA-F]+";
identifier: "[a-zA-Z_][a-zA-Z_0-9]*" $term -1;
integer: decimalint | hexint | octalint;
decimalint: "-?[1-9][0-9]*[uUlL]?";
hexint: "-?(0x|0X)[0-9a-fA-F]+[uUlL]?";
octalint: "-?0[0-7]*[uUlL]?";

