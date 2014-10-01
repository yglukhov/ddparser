module ddparser.gramgram;

import ddparser.gram;
import ddparser.write_tables;
import ddparser.dparse;
import ddparser.dparse_tables;
import ddparser.util;
import ddparser.parse;
import core.stdc.string;
import core.stdc.stdlib;
import std.json;
import std.stdio;
import ddparser.serialize;
import std.string;


struct GramGramParseNode_User
{
  Production *p;
  Rule *r;
  Elem *e;
  uint 	kind;
}

private enum commonValues = q{
auto g = (cast(Grammar*)(D_PN(new_ps, pn_offset).globals));
auto n0 = n_children > 0 ? ((D_PN(children[0], pn_offset))) : null;
auto n1 = n_children > 1 ? ((D_PN(children[1], pn_offset))) : null;
auto n2 = n_children > 2 ? ((D_PN(children[2], pn_offset))) : null;
auto n3 = n_children > 3 ? ((D_PN(children[3], pn_offset))) : null;
auto u0 = cast(GramGramParseNode_User*)(n_children > 0 ? &((D_PN(children[0], pn_offset)).user) : null);
auto u1 = cast(GramGramParseNode_User*)(n_children > 1 ? &((D_PN(children[1], pn_offset)).user) : null);
auto u2 = cast(GramGramParseNode_User*)(n_children > 2 ? &((D_PN(children[2], pn_offset)).user) : null);
auto uu = cast(GramGramParseNode_User*)&(D_PN(new_ps, pn_offset)).user;
};

char* dup_code(string c)
{
    return cast(char*)c.toStringz();
}

char* dup_code(char* s, char* e)
{
    return dup_str(s, e);
}

Grammar* grammarGrammar()
{
      Grammar *g;

  g = new_D_Grammar("-".ptr);
  /* grammar construction options */
  g.set_op_priority_from_rule = 0;
  g.right_recursive_BNF = 0;
  g.states_for_whitespace = 1;
  g.states_for_all_nterms = 0;
  g.tokenizer = 0;
  g.longest_match = 0;
  /* grammar writing options */
  strcpy(g.grammar_ident.ptr, "-".ptr);
  g.scanner_blocks = 4;
  g.scanner_block_size = 0;
  g.write_line_directives = 1;
  g.write_header = -1;
  g.token_type = 0;


  initialize_productions(g);


//    return g;

// TRACE 28
// add_global_code(g, _c0.start_loc.s+1, _c0.end-1, _c0.start_loc.line);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "grammar"); // : 0x10FC84300
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC86E40
// g.r = new_rule(g, g.p); // : 0x10FC84200
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("top_level_statement", g.r); // new_elem: 0x10FC86DA0
// TRACE 159
star_EBNF(g);
// g.p = new_production(g, "grammar__2"); // : 0x10FC84100
// new_elem: 0x10FC86D20
// g.r = new_rule(g, g.p); // : 0x10FC84000
// new_elem: 0x10FC86D00
// new_elem: 0x10FC86CE0
// new_elem: 0x10FC86CC0
// g.r = new_rule(g, g.p); // : 0x10FC87F00
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "top_level_statement"); // : 0x10FC87E00
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC86AC0
// g.r = new_rule(g, g.p); // : 0x10FC87D00
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("global_code", g.r); // new_elem: 0x10FC86A20
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC86900
// g.r = new_rule(g, g.p); // : 0x10FC87C00
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("production", g.r); // new_elem: 0x10FC86860
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC86740
// g.r = new_rule(g, g.p); // : 0x10FC87B00
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("include_statement", g.r); // new_elem: 0x10FC866A0
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "include_statement"); // : 0x10FC87A00
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC86480
// g.r = new_rule(g, g.p); // : 0x10FC87900
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'include'`, g.r);
// new_elem: 0x10FC863C0
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("regex", g.r); // new_elem: 0x10FC86320
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;

 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
 {
     mixin(commonValues);
  char *grammar_pathname = dup_str(n1.start_loc.s+1, n1.end-1);
  if (parse_grammar(g, grammar_pathname, null) < 0)
    d_fail("unable to parse grammar '%s'", grammar_pathname);
  //FREE(grammar_pathname);    
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "global_code"); // : 0x10FC87500
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC860E0
// g.r = new_rule(g, g.p); // : 0x10FC87400
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'%<'`, g.r);
// new_elem: 0x10FC86020
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("balanced_code", g.r); // new_elem: 0x10FC89F80
// TRACE 159
star_EBNF(g);
// g.p = new_production(g, "global_code__6"); // : 0x10FC87300
// new_elem: 0x10FC89F20
// g.r = new_rule(g, g.p); // : 0x10FC87200
// new_elem: 0x10FC89F00
// new_elem: 0x10FC89EE0
// new_elem: 0x10FC89EC0
// g.r = new_rule(g, g.p); // : 0x10FC87100
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'%>'`, g.r);
// new_elem: 0x10FC89E00
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);
add_global_code(g, n0.start_loc.s+2, n2.end-2, n0.start_loc.line);
return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC89C60
// g.r = new_rule(g, g.p); // : 0x10FC8B600
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("curly_code", g.r); // new_elem: 0x10FC89BC0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);
add_global_code(g, n0.start_loc.s+1, n0.end-1, n0.start_loc.line);
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC89A20
// g.r = new_rule(g, g.p); // : 0x10FC8DB00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'${scanner'`, g.r);
// new_elem: 0x10FC89960
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("balanced_code", g.r); // new_elem: 0x10FC898C0
// TRACE 160
plus_EBNF(g);
// g.p = new_production(g, "global_code__7"); // : 0x10FC8DA00
// new_elem: 0x10FC89860
// g.r = new_rule(g, g.p); // : 0x10FC8D900
// new_elem: 0x10FC89840
// new_elem: 0x10FC89800
// new_elem: 0x10FC897E0
// g.r = new_rule(g, g.p); // : 0x10FC8D800
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'}'`, g.r);
// new_elem: 0x10FC89720
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

      g.scanner.code = dup_str(n1.start_loc.s, n1.end);
      g.scanner.line = n0.start_loc.line;
      
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC89580
// g.r = new_rule(g, g.p); // : 0x10FC8FB00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'${declare'`, g.r);
// new_elem: 0x10FC894C0
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("declarationtype", g.r); // new_elem: 0x10FC89420
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("identifier", g.r); // new_elem: 0x10FC89360
// TRACE 159
star_EBNF(g);
// g.p = new_production(g, "global_code__8"); // : 0x10FC8FA00
// new_elem: 0x10FC89300
// g.r = new_rule(g, g.p); // : 0x10FC8F900
// new_elem: 0x10FC892E0
// new_elem: 0x10FC892C0
// new_elem: 0x10FC892A0
// g.r = new_rule(g, g.p); // : 0x10FC8F800
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'}'`, g.r);
// new_elem: 0x10FC891E0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

      if (!d_get_number_of_children(n2))
     	add_declaration(g, n2.start_loc.s, n2.end,  u1.kind, n2.start_loc.line);
      else {
	int i, n = d_get_number_of_children(n2);
	for (i = 0; i < n; i++) {
	  D_ParseNode *pn = d_get_child(n2, i);
	  add_declaration(g, pn.start_loc.s, pn.end,  u1.kind, pn.start_loc.line);
      
	}
      } 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC89040
// g.r = new_rule(g, g.p); // : 0x10FC91100
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'${token'`, g.r);
// new_elem: 0x10FC92F80
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("token_identifier", g.r); // new_elem: 0x10FC92EE0
// TRACE 160
plus_EBNF(g);
// g.p = new_production(g, "global_code__9"); // : 0x10FC91000
// new_elem: 0x10FC92E60
// g.r = new_rule(g, g.p); // : 0x10FC94F00
// new_elem: 0x10FC92E40
// new_elem: 0x10FC92DE0
// new_elem: 0x10FC92DC0
// g.r = new_rule(g, g.p); // : 0x10FC94E00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'}'`, g.r);
// new_elem: 0x10FC92D00
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC92BE0
// g.r = new_rule(g, g.p); // : 0x10FC94D00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'${pass'`, g.r);
// new_elem: 0x10FC92B20
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("identifier", g.r); // new_elem: 0x10FC92A80
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("pass_types", g.r); // new_elem: 0x10FC929E0
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'}'`, g.r);
// new_elem: 0x10FC92920
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);
      add_pass(g, n1.start_loc.s, n1.end,  u2.kind, n1.start_loc.line);
      
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "pass_types"); // : 0x10FC94000
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC926E0
// g.r = new_rule(g, g.p); // : 0x10FC96F00
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC925C0
// g.r = new_rule(g, g.p); // : 0x10FC96E00
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("pass_type", g.r); // new_elem: 0x10FC92520
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("pass_types", g.r); // new_elem: 0x10FC92480
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);
 uu.kind = u0.kind | u1.kind; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "pass_type"); // : 0x10FC96B00
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC92240
// g.r = new_rule(g, g.p); // : 0x10FC96A00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'preorder'`, g.r);
// new_elem: 0x10FC92180
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);
 uu.kind |= D_PASS_PRE_ORDER; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC97FE0
// g.r = new_rule(g, g.p); // : 0x10FC96700
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'postorder'`, g.r);
// new_elem: 0x10FC97F20
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);
 uu.kind |= D_PASS_POST_ORDER; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC97D80
// g.r = new_rule(g, g.p); // : 0x10FC96400
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'manual'`, g.r);
// new_elem: 0x10FC97CC0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);
 uu.kind |= D_PASS_MANUAL; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC97B20
// g.r = new_rule(g, g.p); // : 0x10FC96100
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'for_all'`, g.r);
// new_elem: 0x10FC97A60
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);
 uu.kind |= D_PASS_FOR_ALL; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC978C0
// g.r = new_rule(g, g.p); // : 0x10FC99E00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'for_undefined'`, g.r);
// new_elem: 0x10FC97800
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);
 uu.kind |= D_PASS_FOR_UNDEFINED; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "declarationtype"); // : 0x10FC99B00
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC975A0
// g.r = new_rule(g, g.p); // : 0x10FC99A00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'tokenize'`, g.r);
// new_elem: 0x10FC974E0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);
 uu.kind = DeclarationKind.DECLARE_TOKENIZE; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC97340
// g.r = new_rule(g, g.p); // : 0x10FC99600
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'longest_match'`, g.r);
// new_elem: 0x10FC97280
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);
 uu.kind = DeclarationKind.DECLARE_LONGEST_MATCH; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC970E0
// g.r = new_rule(g, g.p); // : 0x10FC99300
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'whitespace'`, g.r);
// new_elem: 0x10FC97020
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);
 uu.kind = DeclarationKind.DECLARE_WHITESPACE; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC9BE80
// g.r = new_rule(g, g.p); // : 0x10FC99000
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'all_matches'`, g.r);
// new_elem: 0x10FC9BDC0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);
 uu.kind = DeclarationKind.DECLARE_ALL_MATCHES; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC9BC20
// g.r = new_rule(g, g.p); // : 0x10FC9DD00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'set_op_priority_from_rule'`, g.r);
// new_elem: 0x10FC9BB40
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);
 uu.kind = DeclarationKind.DECLARE_SET_OP_PRIORITY; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC9B9A0
// g.r = new_rule(g, g.p); // : 0x10FC9DA00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'all_subparsers'`, g.r);
// new_elem: 0x10FC9B8E0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);
 uu.kind = DeclarationKind.DECLARE_STATES_FOR_ALL_NTERMS; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC9B740
// g.r = new_rule(g, g.p); // : 0x10FC9D700
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'subparser'`, g.r);
// new_elem: 0x10FC9B680
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);
 uu.kind = DeclarationKind.DECLARE_STATE_FOR; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC9B4E0
// g.r = new_rule(g, g.p); // : 0x10FC9D400
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'save_parse_tree'`, g.r);
// new_elem: 0x10FC9B400
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);
 uu.kind = DeclarationKind.DECLARE_SAVE_PARSE_TREE; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "token_identifier"); // : 0x10FC9D100
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FC9B160
// g.r = new_rule(g, g.p); // : 0x10FC9D000
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("identifier", g.r); // new_elem: 0x10FC9B0C0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 new_token(g, n0.start_loc.s, n0.end); 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "production"); // : 0x10FC9FD00
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCA0E80
// g.r = new_rule(g, g.p); // : 0x10FC9FC00
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("production_name", g.r); // new_elem: 0x10FCA0DE0
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `':'`, g.r);
// new_elem: 0x10FCA0D00
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("rules", g.r); // new_elem: 0x10FCA0C60
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `';'`, g.r);
// new_elem: 0x10FCA0BA0
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCA0A80
// g.r = new_rule(g, g.p); // : 0x10FC9FB00
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("production_name", g.r); // new_elem: 0x10FCA09E0
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("regex_production", g.r); // new_elem: 0x10FCA0920
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("rules", g.r); // new_elem: 0x10FCA0860
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `';'`, g.r);
// new_elem: 0x10FCA07A0
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCA0680
// g.r = new_rule(g, g.p); // : 0x10FC9FA00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `';'`, g.r);
// new_elem: 0x10FCA05C0
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "regex_production"); // : 0x10FC9F900
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCA03A0
// g.r = new_rule(g, g.p); // : 0x10FC9F800
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'::='`, g.r);
// new_elem: 0x10FCA02E0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 
  g.p.regex = 1; 

  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "production_name"); // : 0x10FC9F500
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCA0080
// g.r = new_rule(g, g.p); // : 0x10FC9F300
// TRACE 140
auto var0x10fc9f500 = g.p;
auto var0x10fc9f300 = g.r;
///printf("// g.p: %p\n// g.r: %p\n", g.p, g.r); _ps.user.p = g.p; _ps.user.r = g.r;
g.p = new_internal_production(g, g.p); g.r = null;
// g.p: 0x10fc9f500
// g.r: 0x10fc9f300
// g.p = new_production(g, "production_name__17"); // : 0x10FCA2F00
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCA3F60
// g.r = new_rule(g, g.p); // : 0x10FCA2E00
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("identifier", g.r); // new_elem: 0x10FCA3EC0
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCA3DA0
// g.r = new_rule(g, g.p); // : 0x10FCA2D00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'_'`, g.r);
// new_elem: 0x10FCA3CE0
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 109
///printf("// u1.p: %p\n// u1.r: %p\n", _c1.user.p, _c1.user.r);
g.e = new_elem_nterm(g.p, var0x10fc9f300); g.p = var0x10fc9f500; g.r = var0x10fc9f300; vec_add(&g.r.elems, g.e);
// u1.p: 0x10fc9f500
// u1.r: 0x10fc9f300
// new_elem: 0x10FCA3BE0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 g.p = new_production(g, dup_str(n0.start_loc.s, n0.end)); 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "rules"); // : 0x10FCA2700
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCA39A0
// g.r = new_rule(g, g.p); // : 0x10FCA2600
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("rule", g.r); // new_elem: 0x10FCA3900
// TRACE 140
auto var0x10fca2700 = g.p;
auto var0x10fca2600 = g.r;
///printf("// g.p: %p\n// g.r: %p\n", g.p, g.r); _ps.user.p = g.p; _ps.user.r = g.r;
g.p = new_internal_production(g, g.p); g.r = null;
// g.p: 0x10fca2700
// g.r: 0x10fca2600
// g.p = new_production(g, "rules__19"); // : 0x10FCA2200
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCA37E0
// g.r = new_rule(g, g.p); // : 0x10FCA2100
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'|'`, g.r);
// new_elem: 0x10FCA3720
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("rule", g.r); // new_elem: 0x10FCA3680
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 109
///printf("// u1.p: %p\n// u1.r: %p\n", _c1.user.p, _c1.user.r);
g.e = new_elem_nterm(g.p, var0x10fca2600); g.p = var0x10fca2700; g.r = var0x10fca2600; vec_add(&g.r.elems, g.e);
// u1.p: 0x10fca2700
// u1.r: 0x10fca2600
// new_elem: 0x10FCA3580
// TRACE 159
star_EBNF(g);
// g.p = new_production(g, "rules__20"); // : 0x10FCA5D00
// new_elem: 0x10FCA3520
// g.r = new_rule(g, g.p); // : 0x10FCA5C00
// new_elem: 0x10FCA3500
// new_elem: 0x10FCA34E0
// new_elem: 0x10FCA34C0
// g.r = new_rule(g, g.p); // : 0x10FCA5B00
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "rule"); // : 0x10FCA5A00
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCA3300
// g.r = new_rule(g, g.p); // : 0x10FCA5900
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("new_rule", g.r); // new_elem: 0x10FCA3260
// TRACE 140
auto var0x10fca5a00 = g.p;
auto var0x10fca5900 = g.r;
///printf("// g.p: %p\n// g.r: %p\n", g.p, g.r); _ps.user.p = g.p; _ps.user.r = g.r;
g.p = new_internal_production(g, g.p); g.r = null;
// g.p: 0x10fca5a00
// g.r: 0x10fca5900
// g.p = new_production(g, "rule__22"); // : 0x10FCA5500
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCA3140
// g.r = new_rule(g, g.p); // : 0x10FCA5400
// TRACE 140
auto var0x10fca5500 = g.p;
auto var0x10fca5400 = g.r;
///printf("// g.p: %p\n// g.r: %p\n", g.p, g.r); _ps.user.p = g.p; _ps.user.r = g.r;
g.p = new_internal_production(g, g.p); g.r = null;
// g.p: 0x10fca5500
// g.r: 0x10fca5400
// g.p = new_production(g, "rule__22__23"); // : 0x10FCA5000
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCA3020
// g.r = new_rule(g, g.p); // : 0x10FCA6F00
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("element", g.r); // new_elem: 0x10FCA7F80
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("element_modifier", g.r); // new_elem: 0x10FCA7EE0
// TRACE 159
star_EBNF(g);
// g.p = new_production(g, "rule__22__23__24"); // : 0x10FCA6E00
// new_elem: 0x10FCA7E40
// g.r = new_rule(g, g.p); // : 0x10FCA6D00
// new_elem: 0x10FCA7E20
// new_elem: 0x10FCA7E00
// new_elem: 0x10FCA7DE0
// g.r = new_rule(g, g.p); // : 0x10FCA6C00
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 109
///printf("// u1.p: %p\n// u1.r: %p\n", _c1.user.p, _c1.user.r);
g.e = new_elem_nterm(g.p, var0x10fca5400); g.p = var0x10fca5500; g.r = var0x10fca5400; vec_add(&g.r.elems, g.e);
// u1.p: 0x10fca5500
// u1.r: 0x10fca5400
// new_elem: 0x10FCA7CE0
// TRACE 159
star_EBNF(g);
// g.p = new_production(g, "rule__22__25"); // : 0x10FCA6800
// new_elem: 0x10FCA7C80
// g.r = new_rule(g, g.p); // : 0x10FCA6700
// new_elem: 0x10FCA7C60
// new_elem: 0x10FCA7C40
// new_elem: 0x10FCA7C20
// g.r = new_rule(g, g.p); // : 0x10FCA6600
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("simple_element", g.r); // new_elem: 0x10FCA7B80
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("element_modifier", g.r); // new_elem: 0x10FCA7AE0
// TRACE 159
star_EBNF(g);
// g.p = new_production(g, "rule__22__26"); // : 0x10FCA6500
// new_elem: 0x10FCA7A60
// g.r = new_rule(g, g.p); // : 0x10FCA6400
// new_elem: 0x10FCA7A40
// new_elem: 0x10FCA7A20
// new_elem: 0x10FCA7A00
// g.r = new_rule(g, g.p); // : 0x10FCA6300
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 109
///printf("// u1.p: %p\n// u1.r: %p\n", _c1.user.p, _c1.user.r);
g.e = new_elem_nterm(g.p, var0x10fca5900); g.p = var0x10fca5a00; g.r = var0x10fca5900; vec_add(&g.r.elems, g.e);
// u1.p: 0x10fca5a00
// u1.r: 0x10fca5900
// new_elem: 0x10FCA7900
// TRACE 158
conditional_EBNF(g);
// g.p = new_production(g, "rule__27"); // : 0x10FCAAF00
// new_elem: 0x10FCA7880
// g.r = new_rule(g, g.p); // : 0x10FCAAE00
// new_elem: 0x10FCA7860
// g.r = new_rule(g, g.p); // : 0x10FCAAD00
// new_elem: 0x10FCA7840
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("rule_modifier", g.r); // new_elem: 0x10FCA77A0
// TRACE 159
star_EBNF(g);
// g.p = new_production(g, "rule__28"); // : 0x10FCAAC00
// new_elem: 0x10FCA7740
// g.r = new_rule(g, g.p); // : 0x10FCAAB00
// new_elem: 0x10FCA7720
// new_elem: 0x10FCA7700
// new_elem: 0x10FCA76E0
// g.r = new_rule(g, g.p); // : 0x10FCAAA00
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("rule_code", g.r); // new_elem: 0x10FCA7640
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{

     mixin(commonValues);

  vec_add(&g.p.rules, g.r);

  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "new_rule"); // : 0x10FCAA700
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCA7400
// g.r = new_rule(g, g.p); // : 0x10FCAA600
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 g.r = new_rule(g, g.p); 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "simple_element"); // : 0x10FCAA300
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCA71C0
// g.r = new_rule(g, g.p); // : 0x10FCAA200
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("string", g.r); // new_elem: 0x10FCA7120
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 g.e = new_string(g, n0.start_loc.s, n0.end, g.r); 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCAEF80
// g.r = new_rule(g, g.p); // : 0x10FCADB00
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("regex", g.r); // new_elem: 0x10FCAEEE0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 g.e = new_string(g, n0.start_loc.s, n0.end, g.r); 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCAED40
// g.r = new_rule(g, g.p); // : 0x10FCAD400
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("unicode_char", g.r); // new_elem: 0x10FCAECA0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 g.e = new_utf8_char(g, n0.start_loc.s, n0.end, g.r); 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCAEB00
// g.r = new_rule(g, g.p); // : 0x10FCB0D00
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("identifier", g.r); // new_elem: 0x10FCAEA60
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 g.e = new_ident(n0.start_loc.s, n0.end, g.r); 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCAE8C0
// g.r = new_rule(g, g.p); // : 0x10FCB0900
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'${scan'`, g.r);
// new_elem: 0x10FCAE800
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("balanced_code", g.r); // new_elem: 0x10FCAE760
// TRACE 160
plus_EBNF(g);
// g.p = new_production(g, "simple_element__31"); // : 0x10FCB0800
// new_elem: 0x10FCAE700
// g.r = new_rule(g, g.p); // : 0x10FCB0700
// new_elem: 0x10FCAE6E0
// new_elem: 0x10FCAE6A0
// new_elem: 0x10FCAE680
// g.r = new_rule(g, g.p); // : 0x10FCB0600
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'}'`, g.r);
// new_elem: 0x10FCAE5C0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 g.e = new_code(g, n1.start_loc.s, n1.end, g.r); 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCAE420
// g.r = new_rule(g, g.p); // : 0x10FCB2F00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'('`, g.r);
// new_elem: 0x10FCAE360
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("new_subrule", g.r); // new_elem: 0x10FCAE2C0
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("rules", g.r); // new_elem: 0x10FCAE220
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `')'`, g.r);
// new_elem: 0x10FCAE160
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);


      g.e = new_elem_nterm(g.p, u1.r);
      g.p = u1.p;
      g.r = u1.r;
      vec_add(&g.r.elems, g.e);
    
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "element"); // : 0x10FCB4500
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCB7F20
// g.r = new_rule(g, g.p); // : 0x10FCB4400
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("simple_element", g.r); // new_elem: 0x10FCB7E80
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCB7D60
// g.r = new_rule(g, g.p); // : 0x10FCB4300
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("bracket_code", g.r); // new_elem: 0x10FCB7CC0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{

     mixin(commonValues);

      Production *p = new_internal_production(g, null);
      Rule *r = new_rule(g, p);
      vec_add(&p.rules, r);
      r.speculative_code.code = dup_code(n0.start_loc.s + 1, n0.end - 1);
      r.speculative_code.line = n0.start_loc.line;
      g.e = new_elem_nterm(p, g.r);
      vec_add(&g.r.elems, g.e);
    
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCB7B20
// g.r = new_rule(g, g.p); // : 0x10FCBEB00
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("curly_code", g.r); // new_elem: 0x10FCB7A80
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{

     mixin(commonValues);

      Production *p = new_internal_production(g, null);
      Rule *r = new_rule(g, p);
      vec_add(&p.rules, r);
      r.final_code.code = dup_code(n0.start_loc.s + 1, n0.end - 1);
      r.final_code.line = n0.start_loc.line;
      g.e = new_elem_nterm(p, g.r);
      vec_add(&g.r.elems, g.e);
  
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "new_subrule"); // : 0x10FCC4200
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCB7840
// g.r = new_rule(g, g.p); // : 0x10FCC4100
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);


  uu.p = g.p;
  uu.r = g.r;
  g.p = new_internal_production(g, g.p);
  g.r = null;

  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "element_modifier"); // : 0x10FCC8200
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCB75A0
// g.r = new_rule(g, g.p); // : 0x10FCC8100
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'$term'`, g.r);
// new_elem: 0x10FCB74E0
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("integer", g.r); // new_elem: 0x10FCB7440
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 
      if (g.e.kind != ELEM_TERM) 
        d_fail("terminal priority on non-terminal");
      g.e.e.term.term_priority = cast(int)strtol(n1.start_loc.s, null, 0); 
    
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCB72A0
// g.r = new_rule(g, g.p); // : 0x10FCCB700
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'$name'`, g.r);
// new_elem: 0x10FCB71E0
// TRACE 140
auto var0x10fcc8200 = g.p;
auto var0x10fccb700 = g.r;
///printf("// g.p: %p\n// g.r: %p\n", g.p, g.r); _ps.user.p = g.p; _ps.user.r = g.r;
g.p = new_internal_production(g, g.p); g.r = null;
// g.p: 0x10fcc8200
// g.r: 0x10fccb700
// g.p = new_production(g, "element_modifier__35"); // : 0x10FCCB300
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCB70A0
// g.r = new_rule(g, g.p); // : 0x10FCCB200
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("string", g.r); // new_elem: 0x10FCB7000
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCCDEE0
// g.r = new_rule(g, g.p); // : 0x10FCCB100
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("regex", g.r); // new_elem: 0x10FCCDE40
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 109
///printf("// u1.p: %p\n// u1.r: %p\n", _c1.user.p, _c1.user.r);
g.e = new_elem_nterm(g.p, var0x10fccb700); g.p = var0x10fcc8200; g.r = var0x10fccb700; vec_add(&g.r.elems, g.e);
// u1.p: 0x10fcc8200
// u1.r: 0x10fccb700
// new_elem: 0x10FCCDD40
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 
      if (g.e.kind != ELEM_TERM) 
	d_fail("terminal name on non-terminal");
      g.e.e.term.term_name = dup_str(n1.start_loc.s+1, n1.end-1); 
    
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCCDBA0
// g.r = new_rule(g, g.p); // : 0x10FCCF400
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'/i'`, g.r);
// new_elem: 0x10FCCDAE0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 
      if (g.e.kind != ELEM_TERM) 
	d_fail("ignore-case (/i) on non-terminal");
      g.e.e.term.ignore_case = 1; 
    
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCCD940
// g.r = new_rule(g, g.p); // : 0x10FCD1B00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'?'`, g.r);
// new_elem: 0x10FCCD880
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 conditional_EBNF(g); 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCCD6E0
// g.r = new_rule(g, g.p); // : 0x10FCD1800
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'*'`, g.r);
// new_elem: 0x10FCCD620
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 star_EBNF(g); 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCCD460
// g.r = new_rule(g, g.p); // : 0x10FCD1500
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'+'`, g.r);
// new_elem: 0x10FCCD3A0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 plus_EBNF(g); 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCCD1E0
// g.r = new_rule(g, g.p); // : 0x10FCD1200
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'@'`, g.r);
// new_elem: 0x10FCCD120
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("integer", g.r); // new_elem: 0x10FCCD080
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 rep_EBNF(g, cast(int)strtol(n1.start_loc.s, null, 0), -1); 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCD3EE0
// g.r = new_rule(g, g.p); // : 0x10FCD4F00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'@'`, g.r);
// new_elem: 0x10FCD3E20
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("integer", g.r); // new_elem: 0x10FCD3D80
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `':'`, g.r);
// new_elem: 0x10FCD3CC0
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("integer", g.r); // new_elem: 0x10FCD3C20
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 rep_EBNF(g, cast(int)strtol(n1.start_loc.s, null, 0), cast(int)strtol(n3.start_loc.s, null, 0)); 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "rule_modifier"); // : 0x10FCD4B00
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCD39E0
// g.r = new_rule(g, g.p); // : 0x10FCD4A00
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("rule_assoc", g.r); // new_elem: 0x10FCD3940
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("rule_priority", g.r); // new_elem: 0x10FCD38A0
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCD3780
// g.r = new_rule(g, g.p); // : 0x10FCD4900
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("external_action", g.r); // new_elem: 0x10FCD36E0
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "rule_assoc"); // : 0x10FCD4700
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCD3500
// g.r = new_rule(g, g.p); // : 0x10FCD4600
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'$unary_op_right'`, g.r);
// new_elem: 0x10FCD3420
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 g.r.op_assoc = ASSOC_UNARY_RIGHT; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCD3280
// g.r = new_rule(g, g.p); // : 0x10FCD4300
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'$unary_op_left'`, g.r);
// new_elem: 0x10FCD31C0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 g.r.op_assoc = ASSOC_UNARY_LEFT; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCD3020
// g.r = new_rule(g, g.p); // : 0x10FCD4000
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'$binary_op_right'`, g.r);
// new_elem: 0x10FCD7F40
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 g.r.op_assoc = ASSOC_BINARY_RIGHT; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCD7DA0
// g.r = new_rule(g, g.p); // : 0x10FCD9D00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'$binary_op_left'`, g.r);
// new_elem: 0x10FCD7CC0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 g.r.op_assoc = ASSOC_BINARY_LEFT; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCD7B20
// g.r = new_rule(g, g.p); // : 0x10FCD9A00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'$unary_right'`, g.r);
// new_elem: 0x10FCD7A60
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 g.r.rule_assoc = ASSOC_UNARY_RIGHT; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCD78C0
// g.r = new_rule(g, g.p); // : 0x10FCD9700
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'$unary_left'`, g.r);
// new_elem: 0x10FCD7800
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 g.r.rule_assoc = ASSOC_UNARY_LEFT; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCD7660
// g.r = new_rule(g, g.p); // : 0x10FCD9400
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'$binary_right'`, g.r);
// new_elem: 0x10FCD75A0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 g.r.rule_assoc = ASSOC_BINARY_RIGHT; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCD7400
// g.r = new_rule(g, g.p); // : 0x10FCD9100
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'$binary_left'`, g.r);
// new_elem: 0x10FCD7340
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 g.r.rule_assoc = ASSOC_BINARY_LEFT; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCD71A0
// g.r = new_rule(g, g.p); // : 0x10FCDBE00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'$right'`, g.r);
// new_elem: 0x10FCD70E0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 g.r.rule_assoc = ASSOC_NARY_RIGHT; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCDCF40
// g.r = new_rule(g, g.p); // : 0x10FCDBB00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'$left'`, g.r);
// new_elem: 0x10FCDCE80
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 g.r.rule_assoc = ASSOC_NARY_LEFT; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "rule_priority"); // : 0x10FCDB700
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCDCC40
// g.r = new_rule(g, g.p); // : 0x10FCDB600
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("integer", g.r); // new_elem: 0x10FCDCBA0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 
  if (g.r.op_assoc) g.r.op_priority = cast(int)strtol(n0.start_loc.s, null, 0); 
  else g.r.rule_priority = cast(int)strtol(n0.start_loc.s, null, 0); 

  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "external_action"); // : 0x10FCDE600
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCDC940
// g.r = new_rule(g, g.p); // : 0x10FCDE500
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'${action}'`, g.r);
// new_elem: 0x10FCDC880
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 g.r.action_index = g.action_index++; 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCDC6E0
// g.r = new_rule(g, g.p); // : 0x10FCDE100
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'${action'`, g.r);
// new_elem: 0x10FCDC620
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("integer", g.r); // new_elem: 0x10FCDC580
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'}'`, g.r);
// new_elem: 0x10FCDC4C0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);

 g.r.action_index = cast(int)strtol(n1.start_loc.s, null, 0); 
  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "rule_code"); // : 0x10FCE0D00
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCDC280
// g.r = new_rule(g, g.p); // : 0x10FCE0C00
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("speculative_code", g.r); // new_elem: 0x10FCDC1E0
// TRACE 158
conditional_EBNF(g);
// g.p = new_production(g, "rule_code__41"); // : 0x10FCE0B00
// new_elem: 0x10FCDC140
// g.r = new_rule(g, g.p); // : 0x10FCE0A00
// new_elem: 0x10FCDC120
// g.r = new_rule(g, g.p); // : 0x10FCE0900
// new_elem: 0x10FCDC100
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("final_code", g.r); // new_elem: 0x10FCDC060
// TRACE 158
conditional_EBNF(g);
// g.p = new_production(g, "rule_code__42"); // : 0x10FCE0800
// new_elem: 0x10FCE2FE0
// g.r = new_rule(g, g.p); // : 0x10FCE0700
// new_elem: 0x10FCE2FC0
// g.r = new_rule(g, g.p); // : 0x10FCE0600
// new_elem: 0x10FCE2FA0
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("pass_code", g.r); // new_elem: 0x10FCE2F00
// TRACE 159
star_EBNF(g);
// g.p = new_production(g, "rule_code__43"); // : 0x10FCE0500
// new_elem: 0x10FCE2EA0
// g.r = new_rule(g, g.p); // : 0x10FCE0400
// new_elem: 0x10FCE2E80
// new_elem: 0x10FCE2E60
// new_elem: 0x10FCE2E40
// g.r = new_rule(g, g.p); // : 0x10FCE0300
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "speculative_code"); // : 0x10FCE0100
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCE2C20
// g.r = new_rule(g, g.p); // : 0x10FCE0000
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("bracket_code", g.r); // new_elem: 0x10FCE2B80
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);


  g.r.speculative_code.code = dup_code(n0.start_loc.s + 1, n0.end - 1);
  g.r.speculative_code.line = n0.start_loc.line;

  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "final_code"); // : 0x10FCE5B00
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCE2940
// g.r = new_rule(g, g.p); // : 0x10FCE5A00
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("curly_code", g.r); // new_elem: 0x10FCE28A0
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);


  g.r.final_code.code = dup_code(n0.start_loc.s + 1, n0.end - 1);
  g.r.final_code.line = n0.start_loc.line;

  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "pass_code"); // : 0x10FCE7600
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCE2660
// g.r = new_rule(g, g.p); // : 0x10FD77600
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("identifier", g.r); // new_elem: 0x10FCE25C0
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `':'`, g.r);
// new_elem: 0x10FCE2500
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("curly_code", g.r); // new_elem: 0x10FCE2460
// TRACE 200
/// g.r.final_code.code = dup_code(_c0.start_loc.s + 1, _c0.end - 1); g.r.final_code.line = _c0.start_loc.line;
 g.r.final_code.f = (new_ps, children, n_children, pn_offset, parser)
{
     mixin(commonValues);


  add_pass_code(g, g.r, n0.start_loc.s, n0.end, n2.start_loc.s+1,
    n2.end-1, n0.start_loc.line, n2.start_loc.line);

  return 0;
};

// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "curly_code"); // : 0x10FD7C000
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FCE2220
// g.r = new_rule(g, g.p); // : 0x10FD7FF00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'{'`, g.r);
// new_elem: 0x10FCE2160
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("balanced_code", g.r); // new_elem: 0x10FCE20C0
// TRACE 159
star_EBNF(g);
// g.p = new_production(g, "curly_code__48"); // : 0x10FD7FE00
// new_elem: 0x10FCE2060
// g.r = new_rule(g, g.p); // : 0x10FD7FD00
// new_elem: 0x10FCE2040
// new_elem: 0x10FCE2020
// new_elem: 0x10FCE2000
// g.r = new_rule(g, g.p); // : 0x10FD7FC00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'}'`, g.r);
// new_elem: 0x10FD80F40
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "bracket_code"); // : 0x10FD7FA00
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FD80D80
// g.r = new_rule(g, g.p); // : 0x10FD7F900
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'['`, g.r);
// new_elem: 0x10FD80CC0
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("balanced_code", g.r); // new_elem: 0x10FD80C20
// TRACE 159
star_EBNF(g);
// g.p = new_production(g, "bracket_code__50"); // : 0x10FD7F800
// new_elem: 0x10FD80BA0
// g.r = new_rule(g, g.p); // : 0x10FD7F700
// new_elem: 0x10FD80B80
// new_elem: 0x10FD80B60
// new_elem: 0x10FD80B40
// g.r = new_rule(g, g.p); // : 0x10FD7F600
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `']'`, g.r);
// new_elem: 0x10FD80A80
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "balanced_code"); // : 0x10FD7F400
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FD808C0
// g.r = new_rule(g, g.p); // : 0x10FD7F300
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'('`, g.r);
// new_elem: 0x10FD80800
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("balanced_code", g.r); // new_elem: 0x10FD80760
// TRACE 159
star_EBNF(g);
// g.p = new_production(g, "balanced_code__52"); // : 0x10FD7F200
// new_elem: 0x10FD80700
// g.r = new_rule(g, g.p); // : 0x10FD7F100
// new_elem: 0x10FD806E0
// new_elem: 0x10FD806C0
// new_elem: 0x10FD806A0
// g.r = new_rule(g, g.p); // : 0x10FD7F000
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `')'`, g.r);
// new_elem: 0x10FD805E0
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FD804C0
// g.r = new_rule(g, g.p); // : 0x10FD83F00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'['`, g.r);
// new_elem: 0x10FD80400
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("balanced_code", g.r); // new_elem: 0x10FD80360
// TRACE 159
star_EBNF(g);
// g.p = new_production(g, "balanced_code__53"); // : 0x10FD83E00
// new_elem: 0x10FD80300
// g.r = new_rule(g, g.p); // : 0x10FD83D00
// new_elem: 0x10FD802E0
// new_elem: 0x10FD802C0
// new_elem: 0x10FD802A0
// g.r = new_rule(g, g.p); // : 0x10FD83C00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `']'`, g.r);
// new_elem: 0x10FD801E0
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FD800C0
// g.r = new_rule(g, g.p); // : 0x10FD83B00
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'{'`, g.r);
// new_elem: 0x10FD80000
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("balanced_code", g.r); // new_elem: 0x10FD84F60
// TRACE 159
star_EBNF(g);
// g.p = new_production(g, "balanced_code__54"); // : 0x10FD83A00
// new_elem: 0x10FD84F00
// g.r = new_rule(g, g.p); // : 0x10FD83900
// new_elem: 0x10FD84EE0
// new_elem: 0x10FD84EC0
// new_elem: 0x10FD84EA0
// g.r = new_rule(g, g.p); // : 0x10FD83800
// TRACE 98
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `'}'`, g.r);
// new_elem: 0x10FD84DE0
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FD84CC0
// g.r = new_rule(g, g.p); // : 0x10FD83700
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("string", g.r); // new_elem: 0x10FD84C20
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FD84B00
// g.r = new_rule(g, g.p); // : 0x10FD83600
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("identifier", g.r); // new_elem: 0x10FD84A60
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FD84940
// g.r = new_rule(g, g.p); // : 0x10FD83500
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("regex", g.r); // new_elem: 0x10FD848A0
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FD84780
// g.r = new_rule(g, g.p); // : 0x10FD83400
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("integer", g.r); // new_elem: 0x10FD846E0
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FD845C0
// g.r = new_rule(g, g.p); // : 0x10FD83300
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("symbols", g.r); // new_elem: 0x10FD84520
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "symbols"); // : 0x10FD83100
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FD84360
// g.r = new_rule(g, g.p); // : 0x10FD83000
// TRACE 99
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `"[!~` ~ "`" ~ `@#$%^&*\-_+=|:;\\<,>.?/]"`, g.r);
// new_elem: 0x10FD84280
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "string"); // : 0x10FD87E00
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FD840C0
// g.r = new_rule(g, g.p); // : 0x10FD87D00
// TRACE 99
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `"'([^'\\]|\\[^])*'"`, g.r);
// new_elem: 0x10FD88FE0
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "regex"); // : 0x10FD87B00
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FD88E20
// g.r = new_rule(g, g.p); // : 0x10FD87A00
// TRACE 99
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `"\"([^\"\\]|\\[^])*\""`, g.r);
// new_elem: 0x10FD88D40
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "unicode_char"); // : 0x10FD87800
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FD88B80
// g.r = new_rule(g, g.p); // : 0x10FD87700
// TRACE 99
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `"[uU]\+[0-9a-fA-F]+"`, g.r);
// new_elem: 0x10FD88AA0
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "identifier"); // : 0x10FD87500
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FD888E0
// g.r = new_rule(g, g.p); // : 0x10FD87400
// TRACE 99
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `"[a-zA-Z_][a-zA-Z_0-9]*"`, g.r);
// new_elem: 0x10FD88800
// TRACE 147
/// if (g.e.kind != ELEM_TERM) d_fail("terminal priority on non-terminal"); 
 assert(g.e.kind == ELEM_TERM);
 g.e.e.term.term_priority = -1;
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "integer"); // : 0x10FD8AF00
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FD885E0
// g.r = new_rule(g, g.p); // : 0x10FD8AE00
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("decimalint", g.r); // new_elem: 0x10FD88540
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FD88420
// g.r = new_rule(g, g.p); // : 0x10FD8AD00
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("hexint", g.r); // new_elem: 0x10FD88380
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FD88260
// g.r = new_rule(g, g.p); // : 0x10FD8AC00
// TRACE 101
/// g.e = new_ident(_c0.start_loc.s, _c0.end, g.r);
 g.e = new_ident("octalint", g.r); // new_elem: 0x10FD881C0
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "decimalint"); // : 0x10FD8AA00
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FD88000
// g.r = new_rule(g, g.p); // : 0x10FD8A900
// TRACE 99
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `"-?[1-9][0-9]*[uUlL]?"`, g.r);
// new_elem: 0x10FD8CF20
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "hexint"); // : 0x10FD8A700
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FD8CD60
// g.r = new_rule(g, g.p); // : 0x10FD8A600
// TRACE 99
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `"-?(0x|0X)[0-9a-fA-F]+[uUlL]?"`, g.r);
// new_elem: 0x10FD8CC80
// TRACE 93
vec_add(&g.p.rules, g.r);
// TRACE 87
/// g.p = new_production(g, dup_str(_c0.start_loc.s, _c0.end));
 g.p = new_production(g, "octalint"); // : 0x10FD8A400
// TRACE 95
g.r = new_rule(g, g.p);
// new_elem: 0x10FD8CAC0
// g.r = new_rule(g, g.p); // : 0x10FD8A300
// TRACE 99
/// g.e = new_string(g, _c0.start_loc.s, _c0.end, g.r);
 g.e = new_string(g, `"-?0[0-7]*[uUlL]?"`, g.r);
// new_elem: 0x10FD8C9E0
// TRACE 93
vec_add(&g.p.rules, g.r);

finish_productions(g);
build_grammar(g);

   return g; 
}

private __gshared D_ParserTables* gramGramTables;

extern(C) int
parse_grammar(Grammar *g, char *pathname, char *sarg) {

    if (gramGramTables == null)
        gramGramTables = createTablesFromGrammar(grammarGrammar(), null, null);

    int res = 0;
    char *s = sarg;

    vec_add(&g.all_pathnames, dup_str(pathname, null));
    if (!s) 
    {
        s = sbuf_read(pathname);
        if (!s)
            return -1;
    }
    if (!g.productions.n)
        initialize_productions(g);
    D_Parser *p = new_D_Parser(gramGramTables, (GramGramParseNode_User).sizeof);
    p.initial_globals = g;
    p.loc.pathname = pathname;
    if (dparse(p, s, cast(int)strlen(s))) {
        if (g.productions.n > 1)
            finish_productions(g);
    } else
        res = -1;
    if (!sarg)
        FREE(s);
    free_D_Parser(p);
    return res;
}

void testGramJson()
{
      Grammar *g;


  g = new_D_Grammar("-".ptr);
  /* grammar construction options */
  g.set_op_priority_from_rule = 0;
  g.right_recursive_BNF = 0;
  g.states_for_whitespace = 1;
  g.states_for_all_nterms = 0;
  g.tokenizer = 0;
  g.longest_match = 0;
  /* grammar writing options */
  strcpy(g.grammar_ident.ptr, "-".ptr);
  g.scanner_blocks = 4;
  g.scanner_block_size = 0;
  g.write_line_directives = 1;
  g.write_header = -1;
  g.token_type = 0;

  if (parse_grammar(g, cast(char*)"d/grammar.g".ptr, null) < 0)
    assert(false);

    auto s = new Serializer();
    JSONValue v = s.serialize(g);
    string v1 = toJSON(&v, true);
    writeln("JSON: ", v1);

    Grammar* otherG;
    s.deserialize(otherG, v);

    v = s.serialize(otherG);
    string v2 = toJSON(&v, true);
    
    if (v2 != v1)
    {
        writeln("JSON2: ", v2);
        assert(false);
    }

    g = grammarGrammar();
    v = s.serialize(g);
    string v3 = toJSON(&v, true);
    if (v3 != v2)
    {
        writeln("V3: ", v3);
    }
}

