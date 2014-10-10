

/*
  Copyright 2002-2004 John Plevyak, All Rights Reserved
*/
module ddparser.gram;

import std.bitmanip;
import ddparser.util;
import ddparser.dparse_tables;
import ddparser.lr;
import ddparser.lex;
import ddparser.dparse;
import ddparser.parse;
import core.stdc.string;
import std.stdio;
import std.json;
import std.conv;
import ddparser.serialize;
import std.string;
import std.ascii;
import std.algorithm;
import std.array;

enum EOF_SENTINAL = "\377";
enum NO_PROD			=0xFFFFFFFF;


alias Item = Elem;

struct Code {
  char 	*code;
  int	line;
  void serialize(Serializer s)
  {
      //s.map(line, "line");
      //if (line != -1)
      //    s.mapCString(code, "code");
  }

  @property void f(D_ReductionCode c)
  {
      code = cast(char*)c;
      line = -1;
  }

  @property D_ReductionCode f()
  {
      if (line == -1) return cast(D_ReductionCode)code;
      return null;
  }
}

struct Goto {
  Elem	*elem;
  State	*state;
}

alias VecGoto = Vec!(Goto*);

enum ActionKind { 
  ACTION_ACCEPT, ACTION_SHIFT, ACTION_REDUCE, ACTION_SHIFT_TRAILING
}

struct Action {
  ActionKind		kind;
  Term		*term;
  Rule 		*rule;
  State 		*state;
  uint			index;
  string	temp_string;
}

alias VecAction = Vec!(Action*);

struct Hint {
  uint		depth;
  State	*state;
  Rule	*rule;
}
alias VecHint = Vec!(Hint*);

alias VecScanStateTransition = Vec!(ScanStateTransition*);
alias VecScanState = Vec!(ScanState *);

struct Scanner {
  VecScanState			states;
  VecScanStateTransition	transitions;
}

struct State {
  uint		index;
  uint64	hash;
  Vec!(Item*)	items;
  Vec!(Item*)	items_hash;
  VecGoto	gotos;
  VecAction	shift_actions;
  VecAction	reduce_actions;
  VecHint	right_epsilon_hints;
  VecHint	error_recovery_hints;
  Scanner	scanner;
  mixin(bitfields!(
       bool, "accept", 1,
       uint, "scanner_code", 1,
       uint, "goto_on_token", 1,
       uint, "scan_kind", 3,
       uint, "trailing_context", 1,
       uint, "", 25
       ));
  int		goto_table_offset;
  State	*same_shifts;
  State  *reduces_to;
  Rule   *reduces_with;
  Rule   *reduces_to_then_with;
}

enum ASSOC_LEFT   	=0x0001;
enum ASSOC_RIGHT   	=0x0002;
enum ASSOC_NARY   	=0x0004;
enum ASSOC_UNARY  	=0x0008;
enum ASSOC_BINARY 	=0x0010;

enum AssocKind {
  ASSOC_NONE		= 0,
  ASSOC_NARY_LEFT 	= (ASSOC_NARY|ASSOC_LEFT),
  ASSOC_NARY_RIGHT 	= (ASSOC_NARY|ASSOC_RIGHT),
  ASSOC_UNARY_LEFT 	= (ASSOC_UNARY|ASSOC_LEFT),
  ASSOC_UNARY_RIGHT 	= (ASSOC_UNARY|ASSOC_RIGHT),
  ASSOC_BINARY_LEFT	= (ASSOC_BINARY|ASSOC_LEFT), 
  ASSOC_BINARY_RIGHT	= (ASSOC_BINARY|ASSOC_RIGHT),
  ASSOC_NO		= 0x0020
}

@nogc @safe nothrow pure
{
bool IS_RIGHT_ASSOC(AssocKind _x) { return cast(bool)(_x & ASSOC_RIGHT); }
bool IS_LEFT_ASSOC(AssocKind _x) { return cast(bool)(_x & ASSOC_LEFT); }
bool IS_NARY_ASSOC(AssocKind _x) { return cast(bool)(_x & ASSOC_NARY); }
bool IS_BINARY_ASSOC(AssocKind _x) { return cast(bool)(_x & ASSOC_BINARY); }
bool IS_UNARY_ASSOC(AssocKind _x) { return cast(bool)(_x & ASSOC_UNARY); }
bool IS_UNARY_BINARY_ASSOC(AssocKind _x) { return IS_UNARY_ASSOC(_x) || IS_BINARY_ASSOC(_x); }
bool IS_BINARY_NARY_ASSOC(AssocKind _x) { return IS_NARY_ASSOC(_x) || IS_BINARY_ASSOC(_x); }

/* not valid for NARY */
bool IS_EXPECT_RIGHT_ASSOC(AssocKind _x) { return _x && _x != AssocKind.ASSOC_UNARY_LEFT; }
bool IS_EXPECT_LEFT_ASSOC(AssocKind _x) { return _x && _x != AssocKind.ASSOC_UNARY_RIGHT; }
}

struct Rule {
  uint			index;
  Production	*prod;
  int			op_priority;
  AssocKind		op_assoc;
  int			rule_priority;
  AssocKind		rule_assoc;
  Vec!(Elem*)	elems;
  Elem		*end;
  Code			speculative_code;
  Code			final_code;
  Vec!(Code*)		pass_code;
  int			action_index;
  Rule		*same_reduction;

    void serialize(Serializer s)
    {
        s.map(index, "index");
        s.map(prod, "prod");
        s.map(op_priority, "op_priority");
        s.map(op_assoc, "op_assoc");
        s.map(rule_priority, "rule_priority");
        s.map(rule_assoc, "rule_assoc");
        s.map(elems, "elems");
        s.map(end, "end");
        s.map(speculative_code, "speculative_code");
        s.map(final_code, "final_code");
        s.map(pass_code, "pass_code");
        s.map(action_index, "action_index");
        s.map(same_reduction, "same_reduction");
    }
}

enum TermKind {
  TERM_STRING, TERM_REGEX, TERM_CODE, TERM_TOKEN
}
struct Term {
  TermKind		kind;
  uint			index;
  int			term_priority;
  string		term_name;
  AssocKind		op_assoc;
  int			op_priority;
  char			*string_;
  int			string_len;
  mixin(bitfields!(
              uint, "scan_kind", 3,
              uint, "ignore_case", 1,
              uint, "trailing_context", 1,
              uint, "", 27
              ));
  Production	*regex_production;

    void serialize(Serializer s)
    {
        s.map(kind, "kind");
        s.map(index, "index");
        s.map(term_priority, "term_priority");
        //s.map(term_name, "term_name");
        s.map(op_assoc, "op_assoc");
        s.map(op_priority, "op_priority");
        s.mapCStringWithLength(string_, "string_", string_len);

        mixin(fieldMap!("scan_kind", s));
        mixin(fieldMap!("ignore_case", s));
        mixin(fieldMap!("trailing_context", s));
        s.map(regex_production, "regex_production");
    }

}

alias TermVec = Vec!(Term *) ;

enum DeclarationKind {
  DECLARE_TOKENIZE, DECLARE_LONGEST_MATCH, DECLARE_ALL_MATCHES, 
  DECLARE_SET_OP_PRIORITY, DECLARE_STATES_FOR_ALL_NTERMS, 
  DECLARE_STATE_FOR, DECLARE_WHITESPACE, DECLARE_SAVE_PARSE_TREE, DECLARE_NUM
}
alias DECLARE_NUM = DeclarationKind.DECLARE_NUM;

struct Declaration {
  Elem *	elem;
  uint		kind;
  uint		index;

    void serialize(Serializer s)
    {
        s.map(elem, "elem");
        s.map(kind, "kind");
        s.map(index, "index");
    }
}

enum InternalKind {
  INTERNAL_NOT, INTERNAL_HIDDEN, INTERNAL_CONDITIONAL, INTERNAL_STAR, INTERNAL_PLUS
}

struct Production {
    string			name;
    Vec!(Rule *)	rules;
    uint			index;
    mixin(bitfields!(
                uint, "regex", 1,
                uint, "in_regex", 1,
                uint, "internal", 3, 	/* production used for EBNF */
                uint, "live", 1,
                uint, "", 26
                ));
    Rule			*nullable;	/* shortest rule for epsilon reduction */
    Production 	*declaration_group[DECLARE_NUM];
    Declaration 	*last_declaration[DECLARE_NUM];
    State			*state;	/* state for independent parsing of this productions*/
    Elem		*elem;	/* base elem for the item set of the above state */
    Term		*regex_term;	/* regex production terminal */
    Production	*next;

    void serialize(Serializer s)
    {
        //s.map(name, "name");
        s.map(rules, "rules");
        s.map(index, "index");
        mixin(fieldMap!("regex", s));
        mixin(fieldMap!("in_regex", s));
        mixin(fieldMap!("internal", s));
        mixin(fieldMap!("live", s));
        s.map(nullable, "nullable");
        s.map(declaration_group, "declaration_group");
        s.map(last_declaration, "last_declaration");
        s.map(state, "state");
        s.map(elem, "elem");
        s.map(regex_term, "regex_term");
        s.map(next, "next");
    }
}



enum ElemKind {
  ELEM_NTERM, ELEM_TERM, ELEM_UNRESOLVED, ELEM_END
}

struct Elem {
  ElemKind	kind;
  uint		index;
  Rule		*rule;
  union E {
    Production	*nterm;
    Term	*term;
    void	*term_or_nterm;
    string unresolved;
  } 
  E e;

  void serialize(Serializer s)
  {
      s.map(kind, "kind");
      s.map(index, "index");
      s.map(rule, "rule");

      if (kind == ElemKind.ELEM_NTERM)
          s.map(e.nterm, "nterm");
      else if (kind == ElemKind.ELEM_TERM)
          s.map(e.term, "term");
      /* else if (kind == ElemKind.ELEM_UNRESOLVED) */
      /*     s.map(e.unresolved, "unresolved"); */
  }

}

struct Grammar {
  Vec!(Production *)	productions;
  Vec!(Term *)		terminals;
  Vec!(State *)		states;
  Vec!(Action *)		actions;
  Code			scanner;
  /* Code			*code; */
  /* int			ncode; */
  Vec!(Declaration *)	declarations;
  Vec!(D_Pass *)		passes;
  string			default_white_space;
  /* grammar construction options */
  int			set_op_priority_from_rule;
  int			right_recursive_BNF;
  int			states_for_whitespace;
  int			states_for_all_nterms;
  int			tokenizer;
  int			longest_match;
  int			save_parse_tree;
  /* grammar writing options */
  char			grammar_ident[256];
  int			scanner_blocks;
  int			scanner_block_size;
  int			write_line_directives;
  int			token_type;
  char			write_extension[256];
  /* temporary variables for grammar construction */
  Production *	p;
  Rule *		r;
  Elem *		e;
  int			action_index;
  int			action_count;
  int			pass_index;
  int			rule_index;

  void serialize(Serializer s)
  {
      /* s.mapCArray(code, "code", ncode); */
      s.map(scanner, "scanner");
      s.map(productions, "productions");
      s.map(terminals, "terminals");
      s.map(states, "states");
      s.map(actions, "actions");
      s.map(declarations, "declarations");
      s.map(passes, "passes");
      //s.mapCString(default_white_space, "default_white_space");


      s.map(set_op_priority_from_rule, "set_op_priority_from_rule");
      s.map(right_recursive_BNF, "right_recursive_BNF");
      s.map(states_for_whitespace, "states_for_whitespace");
      s.map(states_for_all_nterms, "states_for_all_nterms");
      s.map(tokenizer, "tokenizer");
      s.map(longest_match, "longest_match");
      s.map(save_parse_tree, "save_parse_tree");

      s.map(scanner_blocks, "scanner_blocks");
      s.map(scanner_block_size, "scanner_block_size");
      s.map(write_line_directives, "write_line_directives");
      s.map(token_type, "token_type");
  }
}

alias D_Grammar = Grammar;

/* for creating grammars */
ref auto last_elem(Rule* _r)
{
    return (_r.elems.v[_r.elems.n-1]);
}

/*
  Copyright 2002-2004 John Plevyak, All Rights Reserved
*/


immutable string action_types[] = [ "ACCEPT", "SHIFT", "REDUCE" ];


Production* new_production(Grammar *g, string name) {
  Production *p  = lookup_production(g, name); 
  if (p) {
    return p;
  }
  p = new Production();
  vec_add(&g.productions, p);
  p.name = name;
  return p;
}

private Elem *
new_elem() {
  return new Elem();
}

Rule *
new_rule(Grammar *g, Production *p) {
  Rule *r = new Rule();
  r.prod = p;
  r.end = new_elem();
  r.end.kind = ElemKind.ELEM_END;
  r.end.rule = r;
  r.action_index = g.action_index;
  return r;
}

private Term *
new_term() {
  return new Term();
}

private Elem *
new_elem_term(Term *t, Rule *r) {
  Elem *e = new_elem();
  e.kind = ElemKind.ELEM_TERM;
  e.e.term = t;
  e.rule = r;
  vec_add(&r.elems, e);
  return e;
}

Elem *
new_elem_nterm(Production *p, Rule *r) {
  Elem *e = new_elem();
  e.kind = ElemKind.ELEM_NTERM;
  e.e.nterm = p;
  e.rule = r;
  return e;
}

private Elem *
new_term_string(Grammar *g, string s, Rule *r)
{
  Term *t = new_term();
  t.string_ = cast(char*)s.toStringz();
  t.string_len = cast(int)s.length;
  vec_add(&g.terminals, t);
  return new_elem_term(t, r);
}

string escape_string_for_regex(const char[] s)
{
    auto result = appender!string();
    result.reserve(s.length * 2);
    foreach(c; s)
    {
        switch (c)
        {
            case '(':
            case ')':
            case '[':
            case ']':
            case '-':
            case '^':
            case '*':
            case '?':
            case '+':
                result ~= '\\';
                goto default;
            default:
                result ~= c;
        }
    }

    return result.data;
}

private void
unescape_term_string(Term *t) {
  char *s;
  char *start = null;
  char *ss;
  int length;
  uint base = 0;

  char* res = cast(char*)t.string_[0 .. strlen(t.string_)].dup.toStringz();

  for (ss = s = res; *s; s++) {
    if (*s == '\\') {
      switch (s[1]) {
	case '\\':
    if (t.kind == TermKind.TERM_STRING)
      { *ss = '\\'; s++; break; }
    else
      goto Ldefault;
	case 'b': *ss = '\b'; s++; break;
	case 'f': *ss = '\f'; s++; break;
	case 'n': *ss = '\n'; s++; break;
	case 'r': *ss = '\r'; s++; break;
	case 't': *ss = '\t'; s++; break;
	case 'v': *ss = '\v'; s++; break;
	case 'a': *ss = '\a'; s++; break;
	case 'c': *ss = 0; return;
	case '\"': 
	  if (t.kind == TermKind.TERM_REGEX)
	    { *ss = '\"'; s++; break; }
	  else
	    goto Ldefault;
	case '\'': 
	  if (t.kind == TermKind.TERM_STRING)
	    { *ss = '\''; s++; break; }
	  else
	    goto Ldefault;
	case 'x':
	  length = 0;
	  if (s[2].isHexDigit()) {
	    base = 16;
	    start = s + 2;
	    length++;
	    if (s[3].isHexDigit())
	      length++;
	  }
	  s += length + 1;
	  goto Lncont;
	case 'd':
	  length = 0;
	  if (s[2].isDigit()) {
	    base = 10;
	    start = s + 2;
	    length++;
	    if ((s[3]).isDigit()) {
	      length++;
	      if (s[4].isDigit() && ((s[2] < '2') || ((s[2] == '2') && ((s[3] < '5') || 
                   ((s[3] == '5') && (s[4] < '6')))))) 
		length++;
	    }
	  }
	  s += length + 1;
	  goto Lncont;
	case '0': case '1': case '2': case '3': 
	case '4': case '5': case '6': case '7':
	  length = 1;
	  base = 8;
	  start = s + 1;
	  if (s[2].isDigit() && (s[2] != '8') && (s[2] != '9')) {
	    length++;
	    if (s[3].isDigit() && (s[3] != '8') && (s[3] != '9')) {
	      length++;
	    }
	  }
	  s += length;
	  /* fall through */
	Lncont:
	  if (length > 0) {
        *ss = start[0 .. length].to!ubyte(base);
	    if (*s > 0)	     
	      break;
	    d_fail("encountered an escaped null while processing '%s'", t.string_);
	  } else
	    goto next;
      Ldefault:
	default: 
	  *ss++ = *s;
	  *ss = s[1];
	  s++; 
	  break;
      }
    } else
      *ss = *s;
    ss++;
  next:;
  }
  *s = 0;
  *ss = 0;
  //res.length = ss - res.ptr;
  //writeln("aft: ", t.string_[0 .. strlen(t.string_)]);
  t.string_ = cast(char*)res[0 .. strlen(res)].toStringz();
  t.string_len = cast(int)strlen(t.string_);
  if (!t.string_len)
    d_fail("empty string after unescape '%s'", t.string_);
}

Elem * new_string(Grammar *g, string s, Rule *r)
{
  Elem *x = new_term_string(g, s[1 .. $ - 1], r);
  x.e.term.kind = (s[0] == '"') ? TermKind.TERM_REGEX : TermKind.TERM_STRING;
  unescape_term_string(x.e.term);
  return x;
}

Elem *
new_utf8_char(Grammar *g, char *s, char *e, Rule *r) {
  char utf8_code[4];
  ulong utf32_code, base, len = 0;
  for (utf32_code=0, base=1; e>=s+3; base*=16) {
    e--;
    if (*e >= '0' && *e <= '9')
      utf32_code += base * (*e - '0');
    else if (*e >= 'a' && *e <= 'f')
      utf32_code += base * (*e - 'a' + 10);
    else if (*e >= 'A' && *e <= 'F')
      utf32_code += base * (*e - 'A' + 10);
  }
  if (utf32_code < 0x80) {
    utf8_code[0] = cast(char)utf32_code;
    len = 1;
  } else if (utf32_code < 0x800) {
    utf8_code[0] = 0xc0 | ((utf32_code >> 6) & 0x1f);
    utf8_code[1] = 0x80 | (utf32_code & 0x3f);
    len = 2;
  } else if (utf32_code < 0x10000) {
    utf8_code[0] = 0xe0 | ((utf32_code >> 12) & 0x0f);
    utf8_code[1] = 0x80 | ((utf32_code >> 6) & 0x3f);
    utf8_code[2] = 0x80 | (utf32_code & 0x3f);
    len = 3;
  } else if (utf32_code < 0x02000000) {
    utf8_code[0] = 0xf0 | ((utf32_code >> 18) & 0x07);
    utf8_code[1] = 0x80 | ((utf32_code >> 12) & 0x3f);
    utf8_code[2] = 0x80 | ((utf32_code >> 6) & 0x3f);
    utf8_code[3] = 0x80 | (utf32_code & 0x3f);
    len = 4;
  } else {
    d_fail("UTF32 Unicode value U+%8X too large for valid UTF-8 encoding (cf. Unicode Spec 4.0, section 3.9)", utf32_code);
  }
  Elem *x = new_term_string(g, utf8_code[0 .. len].idup, r);
  x.e.term.kind = TermKind.TERM_STRING;
  return x;
}

Elem *
new_ident(string s, Rule *r)
{
  Elem *x = new_elem();
  x.kind = ElemKind.ELEM_UNRESOLVED;
  x.e.unresolved = s;
  x.rule = r;
  if (r)
    vec_add(&r.elems, x);
  return x;

}

void
new_token(Grammar *g, string s) {
  Term *t = new_term();
  t.string_ = cast(char*)s.toStringz();
  t.string_len = cast(int)s.length;
  vec_add(&g.terminals, t);
  t.kind = TermKind.TERM_TOKEN;
}

Elem *
new_code(Grammar *g, string s, Rule *r) {
  Elem *x = new_term_string(g, s, r);
  x.e.term.kind = TermKind.TERM_CODE;
  return x;
}

Elem *
dup_elem(Elem *e, Rule *r) {
  Elem *ee = new Elem();
  memcpy(ee, e, (Elem).sizeof);
  if (ee.kind == ElemKind.ELEM_UNRESOLVED)
    ee.e.unresolved = e.e.unresolved;
  ee.rule = r;
  return ee;
}

void
new_declaration(Grammar *g, Elem *e, uint kind) {
  Declaration *d = new Declaration();
  d.elem = e;
  d.kind = kind;
  d.index = g.declarations.n;
  vec_add(&g.declarations, d);
}

void
add_declaration(Grammar *g, string s, uint kind, uint line) {
  if (s.length == 0) {
    switch (kind) {
      case DeclarationKind.DECLARE_SET_OP_PRIORITY: g.set_op_priority_from_rule = 1; return;
      case DeclarationKind.DECLARE_STATES_FOR_ALL_NTERMS: g.states_for_all_nterms = 1; return;
      case DeclarationKind.DECLARE_LONGEST_MATCH: g.longest_match = 1; break;
      case DeclarationKind.DECLARE_ALL_MATCHES: g.longest_match = 0; break;
      case DeclarationKind.DECLARE_TOKENIZE: g.tokenizer = 1; break;
      case DeclarationKind.DECLARE_SAVE_PARSE_TREE: g.save_parse_tree = 1; return;
      default: d_fail("declare expects argument, line %d", line);
    }
  }
  switch (kind) {
    case DeclarationKind.DECLARE_WHITESPACE: g.default_white_space = s; return;
    case DeclarationKind.DECLARE_SET_OP_PRIORITY: 
      d_fail("declare does not expect argument, line %d", line);
      break;
    default: 
      new_declaration(g, new_ident(s, null), kind);
      break;
  }
}

D_Pass *
find_pass(Grammar *g, char *start, char *end) {
  while (*start && isWhite(*start)) start++;
  auto l = end - start;
  for (int i = 0; i < g.passes.n; i++)
    if (l == g.passes.v[i].name_len &&
	!strncmp(g.passes.v[i].name, start, l))
      return g.passes.v[i];
  return null;
}

void 
add_pass(Grammar *g, char *start, char *end, uint kind, uint line) {
  if (find_pass(g, start, end))
    d_fail("duplicate pass '%s' line %d", dup_str(start, end), line);
  else {
    D_Pass *p = new D_Pass();
    p.name = dup_str(start, end);
    p.name_len = cast(uint)(end - start);
    p.kind = kind;
    p.index = g.pass_index++;
    vec_add(&g.passes, p);
  }
}

void
add_pass_code(Grammar *g, Rule *r, char *pass_start, char *pass_end,
	      char *code_start, char *code_end, uint pass_line, uint code_line)
{
  D_Pass *p = find_pass(g, pass_start, pass_end);
  if (!p)
    d_fail("unknown pass '%s' line %d", dup_str(pass_start, pass_end), pass_line);
  while (r.pass_code.n <= p.index) vec_add(&r.pass_code, null);
  r.pass_code.v[p.index] = new Code();
  r.pass_code.v[p.index].code = dup_str(code_start, code_end);
  r.pass_code.v[p.index].line = code_line;
}

    
Production *
new_internal_production(Grammar *g, Production *p) {
  string n = p ? p.name : " _synthetic";
  string name = n ~ "__" ~ g.productions.length.to!string();
  Production *pp = new_production(g, name);
  pp.internal = InternalKind.INTERNAL_HIDDEN;
  pp.regex = p ? p.regex : 0;
  if (p) {
      bool found = false;
      Production *tp = null, ttp;
      for (int i = 0; i < g.productions.n; i++) {
          if (found) {
              ttp = g.productions.v[i];
              g.productions.v[i] = tp;
              tp = ttp;
          } else if (p == g.productions.v[i]) {
              found = true;
              tp = g.productions.v[i+1];
              g.productions.v[i+1] = pp;
              i++;
          }
      }
  }
  return pp;
}

void
conditional_EBNF(Grammar *g) {
  Production *pp = new_internal_production(g, g.p);
  pp.internal = InternalKind.INTERNAL_CONDITIONAL;
  Rule *rr = new_rule(g, pp);
  vec_add(&rr.elems, last_elem(g.r));
  last_elem(g.r).rule = rr;
  rr.elems.v[rr.elems.n - 1].rule = rr;
  vec_add(&pp.rules, rr);
  vec_add(&pp.rules, new_rule(g, pp));
  last_elem(g.r) = new_elem_nterm(pp, g.r);
}

void
star_EBNF(Grammar *g) {
  Production *pp = new_internal_production(g, g.p);
  pp.internal = InternalKind.INTERNAL_STAR;
  Rule *rr = new_rule(g, pp);
  if (!g.right_recursive_BNF) {
    vec_add(&rr.elems, new_elem_nterm(pp, rr));
    vec_add(&rr.elems, last_elem(g.r));
    last_elem(g.r) = new_elem_nterm(pp, g.r);
    last_elem(rr).rule = rr;
  } else {
    vec_add(&rr.elems, last_elem(g.r));
    last_elem(g.r) = new_elem_nterm(pp, g.r);
    last_elem(rr).rule = rr;
    vec_add(&rr.elems, new_elem_nterm(pp, rr));
  }
  vec_add(&pp.rules, rr);
  vec_add(&pp.rules, new_rule(g, pp));
}

void
plus_EBNF(Grammar *g) {
  Production *pp = new_internal_production(g, g.p);
  pp.internal = InternalKind.INTERNAL_PLUS;
  Rule *rr = new_rule(g, pp);
  Elem *elem = last_elem(g.r);
  if (!g.right_recursive_BNF) {
    vec_add(&rr.elems, new_elem_nterm(pp, rr));
    vec_add(&rr.elems, dup_elem(elem, rr));
    last_elem(g.r) = new_elem_nterm(pp, g.r);
    if (g.r.rule_priority) {
      rr.rule_priority = g.r.rule_priority;
      rr.rule_assoc = AssocKind.ASSOC_NARY_LEFT;
    }
  } else {
    vec_add(&rr.elems, dup_elem(elem, rr));
    last_elem(g.r) = new_elem_nterm(pp, g.r);
    vec_add(&rr.elems, new_elem_nterm(pp, rr));
    if (g.r.rule_priority) {
      rr.rule_priority = g.r.rule_priority;
      rr.rule_assoc = AssocKind.ASSOC_NARY_RIGHT;
    }
  }
  vec_add(&pp.rules, rr);
  rr = new_rule(g, pp);
  vec_add(&rr.elems, elem);
  elem.rule = rr;
  vec_add(&pp.rules, rr);
}

void
rep_EBNF(Grammar *g, int min, int max) {
  if (max < min) max = min;

  Production *pp = new_internal_production(g, g.p);
  Elem *elem = last_elem(g.r);
  for (int i = min; i <= max; i++) {
    Rule *rr = new_rule(g, pp);
    for (int j = 0; j < i; j++)
      vec_add(&rr.elems, dup_elem(elem, rr));
    vec_add(&pp.rules, rr);
  }
  last_elem(g.r) = new_elem_nterm(pp, g.r);
  FREE(elem);
}

void
initialize_productions(Grammar *g) {
  Production *pp = new_production(g, "0 Start");
  pp.internal = InternalKind.INTERNAL_HIDDEN;
}

void
finish_productions(Grammar *g) {
  Production *pp = g.productions.v[0];
  Rule *rr = new_rule(g, pp);
  vec_add(&rr.elems, new_elem_nterm(null, rr));
  vec_add(&pp.rules, rr);
  rr.elems.v[0].e.nterm = g.productions.v[1];
}

Production* lookup_production(Grammar* g, const(char)[] name)
{
    foreach(p; g.productions)
        if (p.name == name) return p;
    return null;
}

private Term *
lookup_token(Grammar *g, const(char)[] name) {
    foreach(t; g.terminals)
        if (t.kind == TermKind.TERM_TOKEN && t.string_[0 .. t.string_len] == name)
            return t;
    return null;
}

private Term *
unique_term(Grammar *g, Term *t) {
  int i;
  for (i = 0; i < g.terminals.n; i++) 
    if (t.kind == g.terminals.v[i].kind && 
	t.string_len == g.terminals.v[i].string_len &&
	t.term_priority == g.terminals.v[i].term_priority &&
	t.term_name == g.terminals.v[i].term_name &&
	(!g.set_op_priority_from_rule ||
	 (t.op_assoc == g.terminals.v[i].op_assoc &&
	  t.op_priority == g.terminals.v[i].op_priority)) &&
	!strncmp(t.string_, g.terminals.v[i].string_, t.string_len)) 
      return g.terminals.v[i];
  return t;
}

private void
compute_nullable(Grammar *g) {
    /* ensure that the trivial case is the first cause */
    foreach(p; g.productions) {
        foreach (r; p.rules)
            if (!r.elems.n) {
                p.nullable = r;
                break;
            }
    }

    bool changed = true;
    /* transitive closure */
    while (changed) {
        changed = false;
        for (int i = 0; i < g.productions.n; i++) {
            if (!g.productions.v[i].nullable)
                for (int j = 0; j < g.productions.v[i].rules.n; j++) {
                    for (int k = 0; k < g.productions.v[i].rules.v[j].elems.n; k++) {
                        Elem *e = g.productions.v[i].rules.v[j].elems.v[k];
                        if (e.kind != ElemKind.ELEM_NTERM || !e.e.nterm.nullable) 
                            goto Lnot_nullable;
                    }
                    changed = true;
                    g.productions.v[i].nullable = g.productions.v[i].rules.v[j];
                    break;
                }
Lnot_nullable:;
        }
    }
}

/*
  verify and cleanup the grammar datastructures
  - resolve non-terminals
  - set element indexes
*/
private void
resolve_grammar(Grammar *g) {
  Production *p, pp;
  Rule *r;
  Elem *e;
  Term *last_term, t;
  
  g.rule_index = 0;
  for (int i = 0; i < g.productions.n; i++) {
    p = g.productions.v[i];
    if (p != lookup_production(g, p.name))
      d_fail("duplicate production '%s'", p.name);
    p.index = i;
    foreach (r; p.rules) {
      r.index = g.rule_index++;
      last_term = null;
      for (int k = 0; k < r.elems.n; k++) {
	e = r.elems.v[k];
	e.index = k;
	if (e.kind == ElemKind.ELEM_UNRESOLVED) {
      pp = lookup_production(g, e.e.unresolved);
	  if (pp) {
	    e.e.unresolved = null;
	    e.kind = ElemKind.ELEM_NTERM;
	    e.e.nterm = pp;
	  } else
      {
          t = lookup_token(g, e.e.unresolved);
          if (t) {
              e.e.unresolved = null;
              e.kind = ElemKind.ELEM_TERM;
              e.e.term = t;
          } else {
              d_fail("unresolved identifier: '%s'", e.e.unresolved);
          }
      }
	}
	if (e.kind == ElemKind.ELEM_TERM)
	  last_term = e.e.term;
      }
      r.end.index = r.elems.n;
      if (g.set_op_priority_from_rule) {
	if (last_term && r.rule_assoc) {
	  last_term.op_assoc = r.rule_assoc;
	  last_term.op_priority = r.rule_priority;
	}
      }
    }
  }
  for (int i = 0; i < g.terminals.n; i++)
    g.terminals.v[i].index = i;
  compute_nullable(g);
}

private void
merge_identical_terminals(Grammar *g) {
    foreach (p; g.productions) {
        foreach (r; p.rules) {
            foreach (e; r.elems) {
                if (e.kind == ElemKind.ELEM_TERM)
                    e.e.term = unique_term(g, e.e.term);
            }
        }
    }
}

void
print_term(Term *t) {
  string s = t.string_ ? escape_string(t.string_[0 .. strlen(t.string_)]) : null;
  if (t.term_name)
    logf("term_name(\"%s\") ", t.term_name);
  else if (t.kind == TermKind.TERM_STRING) {
    if (!t.string_ || !*t.string_)
      logf("<EOF> ");
    else
      logf("string(\"%s\") ", s);
  } else if (t.kind == TermKind.TERM_REGEX) {
    logf("regex(\"%s\") ", s);
  } else if (t.kind == TermKind.TERM_CODE)
    logf("code(\"%s\") ", s);
  else if (t.kind == TermKind.TERM_TOKEN)
    logf("token(\"%s\") ", s);
  else
    d_fail("unknown token kind");
}

void
print_elem(Elem *ee) {
  if (ee.kind == ElemKind.ELEM_TERM)
    print_term(ee.e.term);
  else if (ee.kind == ElemKind.ELEM_UNRESOLVED)
    logf("%s ", ee.e.unresolved);
  else
    logf("%s ", ee.e.nterm.name);
}

private string
assoc_str(AssocKind e) {
    return [
        AssocKind.ASSOC_NONE : "none",
        AssocKind.ASSOC_NARY_LEFT: "left" ,
        AssocKind.ASSOC_NARY_RIGHT: "right" ,
        AssocKind.ASSOC_UNARY_LEFT: "unary_left" ,
        AssocKind.ASSOC_UNARY_RIGHT: "unary_right" ,
        AssocKind.ASSOC_BINARY_LEFT: "binary_left" ,
        AssocKind.ASSOC_BINARY_RIGHT: "binary_right" ,
        AssocKind.ASSOC_NO: "noassoc"][e];
}

void
print_rule(Rule *r) {
  int k;

  logf("%s: ", r.prod.name);
  for (k = 0; k < r.elems.n; k++)
    print_elem(r.elems.v[k]);
  if (r.speculative_code.code)
    logf("SPECULATIVE_CODE\n%s\nEND CODE\n", r.speculative_code.code);
  if (r.final_code.code)
    logf("FINAL_CODE\n%s\nEND CODE\n", r.final_code.code);
}

void
print_grammar(Grammar *g) {
  uint i, j, k;
  Production *pp;
  Rule *rr;

  if (!g.productions.n)
    return;
  logf("PRODUCTIONS\n\n");
  for (i = 0; i < g.productions.n; i++) {
    pp = g.productions.v[i];
    logf("%s (%d)\n", pp.name, i);
    for (j = 0; j < pp.rules.n; j++) {
      rr = pp.rules.v[j];
      if (!j) 
	logf("\t: ");
      else
	logf("\t| ");
      for (k = 0; k < rr.elems.n; k++)
	print_elem(rr.elems.v[k]);
      if (rr.op_priority)
	logf("op %d ", rr.op_priority);
      if (rr.op_assoc)
	logf("$%s ", assoc_str(rr.op_assoc));
      if (rr.rule_priority)
	logf("rule %d ", rr.rule_priority);
      if (rr.rule_assoc)
	logf("$%s ", assoc_str(rr.rule_assoc));
      if (rr.speculative_code.code)
	logf("%s ", rr.speculative_code.code);
      if (rr.final_code.code)
	logf("%s ", rr.final_code.code);
      logf("\n");
    }
    logf("\t;\n");
    logf("\n");
  }
  logf("TERMINALS\n\n");
  for (i = 0; i < g.terminals.n; i++) {
    logf("\t");
    print_term(g.terminals.v[i]);
    logf("(%d)\n", i + g.productions.n);
  }
  logf("\n");
}

private void
print_item(Item *i) {
  int j, end = 1;

  logf("\t%s: ", i.rule.prod.name);
  for (j = 0; j < i.rule.elems.n; j++) {
    Elem *e = i.rule.elems.v[j];
    if (i == e) {
      logf(". ");
      end = 0;
    }
    print_elem(e);
  }
  if (end)
    logf(". ");
  logf("\n");
}

private void
print_conflict(string kind, int *conflict) {
  if (!*conflict) {
    logf("  CONFLICT (before precedence and associativity)\n");
    *conflict = 1;
  }
  logf("\t%s conflict ", kind);
  logf("\n");
}

private void
print_state(State *s) {
  int j, conflict = 0;

  writefln("STATE %d (%d ITEMS)%s", s.index, s.items.n,
	 s.accept ? " ACCEPT" : "");
  for (j = 0; j < s.items.n; j++)
    print_item(s.items.v[j]);
  if (s.gotos.n)
    logf("  GOTO\n");
  for (j = 0; j < s.gotos.n; j++) {
    logf("\t");
    print_elem(s.gotos.v[j].elem);
    logf(" : %d\n", s.gotos.v[j].state.index);
  }
  logf("  ACTION\n");
  for (j = 0; j < s.reduce_actions.n; j++) {
    Action *a = s.reduce_actions.v[j];
    writefln("\t%s\t", action_types[a.kind]);
    print_rule(a.rule);
    logf("\n");
  }
  for (j = 0; j < s.shift_actions.n; j++) {
    Action *a = s.shift_actions.v[j];
    writefln("\t%s\t", action_types[a.kind]);
    if (a.kind == ActionKind.ACTION_SHIFT) {
      print_term(a.term);
      logf("%d", a.state.index);
    }
    logf("\n");
  }
  if (s.reduce_actions.n > 1)
    print_conflict("reduce/reduce", &conflict);
  if (s.reduce_actions.n && s.shift_actions.n)
    print_conflict("shift/reduce", &conflict);
  logf("\n");
}

void
print_states(Grammar *g) {
  int i;

  for (i = 0; i < g.states.n; i++)
    print_state(g.states.v[i]);
}

private bool
state_for_declaration(Grammar *g, int iproduction) {
  foreach (d; g.declarations)
    if (d.kind == DeclarationKind.DECLARE_STATE_FOR &&
	d.elem.e.nterm.index == iproduction)
      return true;
  return false;
}

private void
make_elems_for_productions(Grammar *g) {
  int i, j, k, l;
  Rule *rr;
  Production *ppp;

  Production *pp = g.productions.v[0];
  for (i = 0; i < g.productions.n; i++)
    if (!g.productions.v[i].internal) {
      if (g.states_for_all_nterms || 
	  state_for_declaration(g, i)) {
	/* try to find an existing elem */
	for (j = 0; j < g.productions.n; j++)
	  for (k = 0; k < g.productions.v[j].rules.n; k++) {
	    rr = g.productions.v[j].rules.v[k];
	    for (l = 0; l < rr.elems.n; l++)
	      if (rr.elems.v[l].e.term_or_nterm == g.productions.v[i]) {
		g.productions.v[i].elem = rr.elems.v[l];
		break;
	      }
	  }
	if (j >= g.productions.n) { /* not found */
	  g.productions.v[i].elem = 
	    new_elem_nterm(g.productions.v[i], new_rule(g, pp));
	  g.productions.v[i].elem.rule.index = g.rule_index++; /* fake */
	}
      }
    }
  if (!g.states_for_all_nterms &&
      g.states_for_whitespace)
  {
      ppp = lookup_production(g, "whitespace");
      if (ppp) 
      {
          ppp.elem = new_elem_nterm(ppp, new_rule(g, pp));
          ppp.elem.rule.index = g.rule_index++; /* fake */
      }
  }
}

private void
convert_regex_production_one(Grammar *g, Production *p) {
  int k, l;
  Production *pp;
  Rule *r, rr;
  Elem *e;
  char *buf = null, b;
  const(char)* s;
  int buf_len = 0;

  if (p.regex_term) /* already done */
    return;

  bool circular = false;

  if (p.in_regex)
    d_fail("circular regex production '%s'", p.name);
  p.in_regex = 1;
  foreach (r; p.rules) {
    if (r.final_code.code || (r.speculative_code.code && p.rules.n > 1))
      d_fail("final and/or multi-rule code not permitted in regex productions '%s'", p.name);
    foreach (e; r.elems) {
      if (e.kind == ElemKind.ELEM_NTERM) {
	if (!e.e.nterm.regex)
	  d_fail("regex production '%s' cannot invoke non-regex production '%s'",
	       p.name, e.e.nterm.name);
	pp = e.e.nterm;
	for (l = 0; l < pp.rules.n; l++)
	  if (pp.rules[l].speculative_code.code || pp.rules[l].final_code.code)
	    d_fail("code not permitted in rule %d of regex productions '%s'", l, p.name);
	if (p != pp) {
	  convert_regex_production_one(g, pp);
	  buf_len += pp.regex_term.string_len + 5;
	} else {
	  circular = true;
	  buf_len += 5;
	}
      } else { /* e.kind == ElemKind.ELEM_TERM */
	if (e.e.term.kind == TermKind.TERM_CODE || e.e.term.kind == TermKind.TERM_TOKEN)
	  d_fail("regex production '%s' cannot include scanners or tokens");
	buf_len += e.e.term.string_len + 5;
      }
    }
  }
  b = buf = cast(char*)MALLOC(buf_len + 1);
  Term *t = new_term();
  t.kind = TermKind.TERM_REGEX;
  t.string_ = buf;
  t.index = g.terminals.n;
  t.regex_production = p;
  vec_add(&g.terminals, t);
  p.regex_term = t;
  p.regex_term.term_name = p.name;
  if (circular) { /* attempt to match to regex operators */
    if (p.rules.n != 2)
      Lfail: d_fail("unable to resolve circular regex production: '%s'", p.name);
    l = p.rules[0].elems.n + p.rules[1].elems.n;
    if (l == 2 || l == 3) {
      if (p.rules[0].elems.n != 2 && p.rules[1].elems.n != 2)
	goto Lfail;
      r = p.rules[0].elems.n == 2 ? p.rules[0] : p.rules[1];
      rr = p.rules[0] == r ? p.rules[1] : p.rules[0];
      if (r.elems[0].e.nterm != p && r.elems[1].e.nterm != p)
	goto Lfail;
      e = r.elems[0].e.nterm == p ? r.elems[1] : r.elems[1];
      if (rr.elems.n && e.e.term_or_nterm != rr.elems[0].e.term_or_nterm)
	goto Lfail;
      t = e.kind == ElemKind.ELEM_TERM ? e.e.term : e.e.nterm.regex_term;
      *b++ = '('; 
      if (t.kind == TermKind.TERM_STRING)
	s = escape_string_for_regex(t.string_[0 .. strlen(t.string_)]).toStringz();
      else
	s = t.string_;
      memcpy(b, s, strlen(s)); b += strlen(s);
      *b++ = ')'; 
      if (l == 2) 
	*b++ = '*'; 
      else
	*b++ = '+'; 
      *b = 0;
      p.regex_term.string_len = cast(uint)strlen(p.regex_term.string_);
    } else
      goto Lfail;
  } else { /* handle the base case, p = (r | r'), r = (e e') */
    if (p.rules.n > 1)
      *b++ = '(';
    for (int j = 0; j < p.rules.n; j++) {
      r = p.rules[j];
      if (r.elems.n > 1)
	*b++ = '(';
      foreach (e; r.elems) {
	t = e.kind == ElemKind.ELEM_TERM ? e.e.term : e.e.nterm.regex_term;
	if (t.kind == TermKind.TERM_STRING)
	  s = escape_string_for_regex(t.string_[0 .. strlen(t.string_)]).toStringz();
	else
	  s = t.string_;
	memcpy(b, s, strlen(s)); b += strlen(s);
      }
      if (r.elems.n > 1)
	*b++ = ')';
      if (j != p.rules.n - 1)
	*b++ = '|';
    }
    if (p.rules.n > 1)
      *b++ = ')';
    *b = 0;
    p.regex_term.string_len = cast(int)strlen(p.regex_term.string_);
  }
  p.in_regex = 0;
}

private void
convert_regex_productions(Grammar *g) {
  int i, j, k;
  Production *p;
  Rule *r;

  for (i = 0; i < g.productions.n; i++) {
    p = g.productions.v[i];
    if (!p.regex)
      continue;
    convert_regex_production_one(g, p);
  }
  for (i = 0; i < g.productions.n; i++) {
    p = g.productions.v[i];
    for (j = 0; j < p.rules.n; j++) {
      r = p.rules.v[j];
      for (k = 0; k < r.elems.n; k++) {
	if (r.elems.v[k].kind == ElemKind.ELEM_NTERM && r.elems.v[k].e.nterm.regex_term) {
	  r.elems.v[k].e.term = r.elems.v[k].e.nterm.regex_term;
	  r.elems.v[k].kind = ElemKind.ELEM_TERM;
	}
      }
    }
  }
}

private void
check_default_actions(Grammar *g) {
  Production *pdefault;

  pdefault = lookup_production(g, "_");
  if (pdefault && pdefault.rules.n > 1)
    d_fail("number of rules in default action != 1");
}

struct EqState {
  State	*eq;
  Rule 	*diff_rule;
  State 	*diff_state;
}

void
build_eq(Grammar *g) {
  int i, j, k, changed = 1, x, xx;
  State *s, ss;
  EqState *e, ee;

  EqState[] eq = new EqState[g.states.length];
  while (changed) {
    changed = 0;
    for (i = 0; i < g.states.n; i++) {
      s = g.states.v[i];
      e = &eq[s.index];
      for (j = i + 1; j < g.states.n; j++) {
	ss = g.states.v[j];
	ee = &eq[ss.index];
	if (e.eq || ee.eq)
	  continue;
	if (s.same_shifts != ss.same_shifts && ss.same_shifts != s)
	  continue;
	/* check gotos */
	if (s.gotos.n != ss.gotos.n)
	  continue;
	for (k = 0; k < s.gotos.n; k++) {
	  if (elem_symbol(g, s.gotos.v[k].elem) != elem_symbol(g, ss.gotos.v[k].elem))
	    goto Lcontinue;
	  if (s.gotos.v[k].state != ss.gotos.v[k].state) {
	    EqState *ge = &eq[s.gotos.v[k].state.index];
	    EqState *gee = &eq[ss.gotos.v[k].state.index];
	    if (ge.eq != ss.gotos.v[k].state && gee.eq != s.gotos.v[k].state)
	      goto Lcontinue;
	    if ((ee.diff_state && ee.diff_state != eq[ss.gotos.v[k].state.index].eq) ||
		(e.diff_state && e.diff_state != eq[s.gotos.v[k].state.index].eq))
	      goto Lcontinue;
	    /* allow one different state */
	    ee.diff_state = ss.gotos.v[k].state;
	    e.diff_state = s.gotos.v[k].state;
	  }
	}
	/* check reductions */
	if (s.reduce_actions.n != ss.reduce_actions.n)
	  continue;
	for (k = 0; k < s.reduce_actions.n; k++) {
	  if (s.reduce_actions.v[k].rule == ss.reduce_actions.v[k].rule)
	    continue;
	  if (s.reduce_actions.v[k].rule.prod !=
	      ss.reduce_actions.v[k].rule.prod)
	    goto Lcontinue;
	  if ((x = s.reduce_actions.v[k].rule.elems.n) !=
	      (xx = ss.reduce_actions.v[k].rule.elems.n)) {
	    if ((ee.diff_rule && ee.diff_rule != ss.reduce_actions.v[k].rule) ||
		(e.diff_rule && e.diff_rule != s.reduce_actions.v[k].rule))
	      goto Lcontinue;
	    /* allow one different rule */
	    ee.diff_rule = ss.reduce_actions.v[k].rule;
	    e.diff_rule = s.reduce_actions.v[k].rule;
	  }
	}
	ee.eq = s;	
	changed = 1;
	Lcontinue:;
      }
    }
  }
  for (i = 0; i < g.states.n; i++) {
    s = g.states.v[i];	
    e = &eq[s.index];
    if (e.eq) {
      if (d_verbose_level > 2) {
	logf("eq %d %d ", s.index, e.eq.index); 
	if (e.diff_state)
	  logf("diff state (%d %d) ", 
		 e.diff_state.index,
		 eq[e.eq.index].diff_state.index);
	if (e.diff_rule) {
	  logf("diff rule ");
	  logf("[ ");
	  print_rule(e.diff_rule);
	  logf("][ ");
	  print_rule(eq[e.eq.index].diff_rule);
	  logf("]");
	}
	logf("\n");
      }
    }
  }
  for (i = 0; i < g.states.n; i++) {
    s = g.states.v[i];
    e = &eq[s.index];
    if (e.eq && e.diff_state) {
      if (eq[e.diff_state.index].diff_rule &&
	  eq[e.diff_state.index].diff_rule.elems.n == 2) 
      {
	s.reduces_to = e.eq;
	s.reduces_with = eq[e.eq.index].diff_rule;
	s.reduces_to_then_with = e.diff_rule;
      } else if (eq[eq[e.eq.index].diff_state.index].diff_rule &&
		 eq[eq[e.eq.index].diff_state.index].diff_rule.elems.n == 2) 
	{
	  e.eq.reduces_to = s;
	  s.reduces_with = e.diff_rule;
	  s.reduces_to_then_with = eq[e.eq.index].diff_rule;
	}
    }
  }
  for (i = 0; i < g.states.n; i++) {
    s = g.states.v[i];
    if (s.reduces_to)
      if (d_verbose_level)
	logf("reduces_to %d %d\n", s.index, s.reduces_to.index);
  }
}

Grammar *
new_D_Grammar() {
  return new Grammar();
}

private void
free_rule(Rule *r) {
  int i;
  FREE(r.end);
  if (r.final_code.code)
    FREE(r.final_code.code);
  if (r.speculative_code.code)
    FREE(r.speculative_code.code);
  vec_free(&r.elems);
  for (i = 0; i < r.pass_code.n; i++) {
    FREE(r.pass_code.v[i].code);
    FREE(r.pass_code.v[i]);
  }
  vec_free(&r.pass_code);
  FREE(r);
}

void
free_D_Grammar(Grammar *g) {
  int i, j, k;

  for (i = 0; i < g.productions.n; i++) {
    Production *p = g.productions.v[i];
    for (j = 0; j < p.rules.n; j++) {
      Rule *r = p.rules.v[j];
      if (r == g.r)
	g.r = null;
      for (k = 0; k < r.elems.n; k++) {
	Elem *e = r.elems.v[k];
	if (e == p.elem)
	  p.elem = null;
	FREE(e);
      }
      if (r.end == p.elem)
	p.elem = null;
      free_rule(r);
    }
    vec_free(&p.rules);
    if (p.elem) {
      free_rule(p.elem.rule);
      FREE(p.elem);
    }
    FREE(p);
  }
  vec_free(&g.productions);
  for (i = 0; i < g.terminals.n; i++) {
    Term *t = g.terminals.v[i];
    if (t.string_)
      FREE(t.string_);
  }
  vec_free(&g.terminals);
  for (i = 0; i < g.actions.n; i++)
    free_Action(g.actions.v[i]);
  vec_free(&g.actions);
  if (g.scanner.code)
    FREE(g.scanner.code);
  for (i = 0; i < g.states.n; i++) {
    State *s = g.states.v[i];
    vec_free(&s.items);
    vec_free(&s.items_hash);
    for (j = 0; j < s.gotos.n; j++) {
      FREE(s.gotos.v[j].elem);
      FREE(s.gotos.v[j]);
    }
    vec_free(&s.gotos);
    vec_free(&s.shift_actions);
    vec_free(&s.reduce_actions);
    for (j = 0; j < s.right_epsilon_hints.n; j++)
      FREE(s.right_epsilon_hints.v[j]);
    vec_free(&s.right_epsilon_hints);
    for (j = 0; j < s.error_recovery_hints.n; j++)
      FREE(s.error_recovery_hints.v[j]);
    vec_free(&s.error_recovery_hints);
    if (!s.same_shifts) {
      for (j = 0; j < s.scanner.states.n; j++) {
	vec_free(&s.scanner.states.v[j].accepts);
	vec_free(&s.scanner.states.v[j].live);
	FREE(s.scanner.states.v[j]);
      }
      vec_free(&s.scanner.states);
      for (j = 0; j < s.scanner.transitions.n; j++)
	if (s.scanner.transitions.v[j]) {
	  vec_free(&s.scanner.transitions.v[j].live_diff);
	  vec_free(&s.scanner.transitions.v[j].accepts_diff);
	  FREE(s.scanner.transitions.v[j]);
	}
      vec_free(&s.scanner.transitions);
    }
    FREE(s);
  }
  vec_free(&g.states);
  for (i = 0; i < g.declarations.n; i++) {
    FREE(g.declarations.v[i].elem);
    FREE(g.declarations.v[i]);
  }
  vec_free(&g.declarations);
  for (i = 0; i < g.passes.n; i++) {
    FREE(g.passes.v[i].name);
    FREE(g.passes.v[i]);
  }
  vec_free(&g.passes);
  FREE(g);
}

private int
scanner_declaration(Declaration *d) {
  switch (d.kind) {
    case DeclarationKind.DECLARE_TOKENIZE:
    case DeclarationKind.DECLARE_LONGEST_MATCH:
    case DeclarationKind.DECLARE_ALL_MATCHES:
      return 1;
    default:
      return 0;
  }
}

private void
set_declaration_group(Production *p, Production *root, Declaration *d) {
  int i, j;
  if (p.declaration_group[d.kind] == root)
    return;
  if (d.kind == DeclarationKind.DECLARE_TOKENIZE && p.declaration_group[d.kind]) {
    d_fail("shared tokenize subtrees");
    return;
  }
  p.declaration_group[d.kind] = root;
  p.last_declaration[d.kind] = d;
  for (i = 0; i < p.rules.n; i++) {
    for (j = 0; j < p.rules.v[i].elems.n; j++)
      if (p.rules.v[i].elems.v[j].kind == ElemKind.ELEM_NTERM)
	set_declaration_group(p.rules.v[i].elems.v[j].e.nterm, root, d);
  }
}

private void
propogate_declarations(Grammar *g) {
  Production *p, start = g.productions.v[0];

  /* global defaults */ 	 
   if (g.tokenizer) 	 
     new_declaration(g, new_elem_nterm(g.productions.v[0], null), DeclarationKind.DECLARE_TOKENIZE); 	 
   if (g.longest_match) 	 
     new_declaration(g, new_elem_nterm(g.productions.v[0], null), DeclarationKind.DECLARE_LONGEST_MATCH);
  /* resolve declarations */
   foreach (d; g.declarations) {
       Elem *e = d.elem;
       if (e.kind == ElemKind.ELEM_UNRESOLVED) {
           if (e.e.unresolved.length == 0)
               p = g.productions.v[0];
           else
           {
               p = lookup_production(g, e.e.unresolved);
               if (!p)
                   d_fail("unresolved declaration '%s'", e.e.unresolved);
           }
           e.e.unresolved = null;
           e.kind = ElemKind.ELEM_NTERM;
           e.e.nterm = p;
       }
   }
  /* build declaration groups (covering a production subtrees) */
  foreach (d; g.declarations) {
    if (scanner_declaration(d)) {
      p = d.elem.e.nterm;
      if (p == start) {
	foreach (p; g.productions) {
	  p.declaration_group[d.kind] = start;
	  p.last_declaration[d.kind] = d;
	}
      } else
	set_declaration_group(p, p, d);
    }
  }
  /* set terminal scan_kind */
  foreach (p; g.productions) {
    foreach (r; p.rules) {
      foreach (e; r.elems) {
	if (e.kind == ElemKind.ELEM_TERM) {
	  if (!p.declaration_group[DeclarationKind.DECLARE_LONGEST_MATCH] &&
	      !p.declaration_group[DeclarationKind.DECLARE_ALL_MATCHES])
	    e.e.term.scan_kind = D_SCAN_DEFAULT;
	  else if (p.declaration_group[DeclarationKind.DECLARE_LONGEST_MATCH] &&
	      !p.declaration_group[DeclarationKind.DECLARE_ALL_MATCHES])
	    e.e.term.scan_kind = D_SCAN_LONGEST;
	  else if (!p.declaration_group[DeclarationKind.DECLARE_LONGEST_MATCH] &&
		   p.declaration_group[DeclarationKind.DECLARE_ALL_MATCHES])
	    e.e.term.scan_kind = D_SCAN_ALL;
	  else {
	    if (p.last_declaration[DeclarationKind.DECLARE_LONGEST_MATCH].index > 
		p.last_declaration[DeclarationKind.DECLARE_ALL_MATCHES].index)
	      e.e.term.scan_kind = D_SCAN_LONGEST;
	    else
	      e.e.term.scan_kind = D_SCAN_ALL;
	  }
	}
      }
    }
  }
}

private void
merge_shift_actions(State *to, State *from) {
    int i, j;
    for (i = 0; i < from.shift_actions.n; i++) {
        for (j = 0; j < to.shift_actions.n; j++)
            if (from.shift_actions.v[i].term == to.shift_actions.v[j].term)
                goto Lnext;
        vec_add(&to.shift_actions, from.shift_actions.v[i]);
Lnext:;
    }
}

private void
compute_declaration_states(Grammar *g, Production *p, Declaration *d) {
  State *base_s = null;
  int scanner = scanner_declaration(d);

  foreach (s; g.states) {
    if (d.kind == DeclarationKind.DECLARE_TOKENIZE) {
      if (!base_s)
	base_s = s;
      else {
	s.same_shifts = base_s;
	merge_shift_actions(base_s, s);
      }
    }
    if (scanner) {
      foreach (k; s.items)
	if (k.kind == ElemKind.ELEM_TERM)
	  switch (k.e.term.scan_kind) {
	    case D_SCAN_LONGEST:
	      if (s.scan_kind == D_SCAN_RESERVED || 
		  s.scan_kind == D_SCAN_LONGEST)
		s.scan_kind = D_SCAN_LONGEST;
	      else
		s.scan_kind = D_SCAN_MIXED;
	      break;
	    case D_SCAN_ALL:
	      if (s.scan_kind == D_SCAN_RESERVED || 
		  s.scan_kind == D_SCAN_ALL)
		s.scan_kind = D_SCAN_ALL;
	      else
		s.scan_kind = D_SCAN_MIXED;
	      break;
	    default:
	      break;
	  }
    }
  }
}

private void
map_declarations_to_states(Grammar *g) {
  int i;
  State *s;
  
  for (i = 0; i < g.states.n; i++) {
    s = g.states.v[i];
    s.scan_kind = D_SCAN_RESERVED;
  }
  /* map groups to sets of states */
  for (i = 0; i < g.declarations.n; i++)
    if (scanner_declaration(g.declarations.v[i]))
      compute_declaration_states(g, g.declarations.v[i].elem.e.nterm, 
				 g.declarations.v[i]);
  for (i = 0; i < g.states.n; i++) {
    s = g.states.v[i];
    if (s.scan_kind == D_SCAN_RESERVED)
      s.scan_kind = D_SCAN_DEFAULT; /* set the default */
  }
}

int
build_grammar(Grammar *g) {
  resolve_grammar(g);
  convert_regex_productions(g);
  propogate_declarations(g);
  merge_identical_terminals(g);
  make_elems_for_productions(g);
  check_default_actions(g);
  build_LR_tables(g);
  map_declarations_to_states(g);
  if (d_verbose_level) {
    logf("%d productions %d terminals %d states %d declarations\n",
	   g.productions.n, g.terminals.n, g.states.n, 
	   g.declarations.n);
  }
  if (d_verbose_level > 1) {
    print_grammar(g);
    print_states(g);
  }
  build_scanners(g);
  build_eq(g);
  return 0;
}


/*   Wlodek Bzyl, <matwb@univ.gda.pl>  
 */

private void
print_term_escaped(Term *t, int double_escaped) {
  string s;
  if (t.term_name) {
    logf("%s ", t.term_name);
  } else if (t.kind == TermKind.TERM_STRING) {
    s = t.string_ ? escape_string_single_quote(t.string_[0 .. strlen(t.string_)]) : null;
    if (!t.string_ || !*t.string_)
      logf("<EOF> ");
    else {
      logf("'%s' ", double_escaped ? escape_string_single_quote(s) : s);
      if (t.ignore_case)
	logf("/i ");
      if (t.term_priority)
	writefln("%sterm %d ", double_escaped?"#":"$", t.term_priority);
    }
  } else if (t.kind == TermKind.TERM_REGEX) {
    s = t.string_ ? escape_string(t.string_[0 .. strlen(t.string_)]) : null;
    //char *s = t.string_; // ? escape_string(t.string_) : null;
    string quote = double_escaped ? "\\\"" : "\"";
    logf("%s%s%s ", quote, double_escaped ? escape_string(s) : s, quote);
    if (t.ignore_case)
      logf("/i ");
    if (t.term_priority)
      writefln("%sterm %d ", double_escaped?"#":"$", t.term_priority);
  } else if (t.kind == TermKind.TERM_CODE) {
    s = t.string_ ? escape_string(t.string_[0 .. strlen(t.string_)]) : null;
    logf("code(\"%s\") ", s);
  } else if (t.kind == TermKind.TERM_TOKEN) {
    s = t.string_ ? escape_string(t.string_[0 .. strlen(t.string_)]) : null;
    logf("%s ", s);
  } else
    d_fail("unknown token kind");
}

/* print_elem changed to call print_term_escaped */
private void
print_element_escaped(Elem *ee, int double_escaped) {
  if (ee.kind == ElemKind.ELEM_TERM)
    print_term_escaped(ee.e.term, double_escaped);
  else if (ee.kind == ElemKind.ELEM_UNRESOLVED)
    logf("%s ", ee.e.unresolved);
  else
    logf("%s ", ee.e.nterm.name);
}

private void
print_production(Production *p) {
  uint j, k;
  Rule *r;
  string opening[] = [ ""        , "\n\t  [ logf(\""    , "\n\t  { logf(\"" ];
  string closing[] = [ "\n"      , "\\n\"); ]\n"          , "\\n\"); }\n" ];
  string middle[]  = [ "\n\t:   ", "  <-  "               , "  <=  " ];
  string assoc[]   = [ "$"       , "#"                    , "#" ];
  string speculative_final_closing = "\\n\"); ]";  /* closing[1] without final newline */
  string next_or_rule = "\t|   ";
  //  char *regex_production = "  ::=  ";

  uint variant = 0;

  for (j = 0; j < p.rules.n; j++) {
  Lmore:
    r = p.rules.v[j];
    if (!j) {
      //      if (p.regex) {
      //	logf("%s%s%s", opening[variant], p.name, regex_production);
      //      } else {
      writefln("%s%s%s", opening[variant], p.name, middle[variant]);
      //      }
    } else {
      if (variant==0)
	writefln("%s", next_or_rule);
      else
	writefln("%s%s%s", opening[variant], p.name, middle[variant]);
    }

    for (k = 0; k < r.elems.n; k++)
      print_element_escaped(r.elems.v[k], variant);

    if (r.op_assoc)
      writefln(" %s%s ", assoc[variant], assoc_str(r.op_assoc));
    if (r.op_priority)
      writefln("%d ", r.op_priority);
    if (r.rule_assoc)
      writefln(" %s%s ", assoc[variant], assoc_str(r.rule_assoc));
    if (r.rule_priority)
      writefln("%d ", r.rule_priority);

    if ((d_rdebug_grammar_level == 1 && variant == 0) ||
	(d_rdebug_grammar_level == 3 && variant == 0)) {
      variant=1;
      goto Lmore;
    }
    if ((d_rdebug_grammar_level == 2 && variant == 0) ||
	(d_rdebug_grammar_level == 3 && variant == 1)) {
      if (variant==1)
	writefln("%s", speculative_final_closing);
      variant=2;
      goto Lmore;
    }

    writefln("%s", closing[variant]);
    variant = 0;
  }
  logf("\t;\n");
  logf("\n");
}

private void
print_productions(Grammar *g) {
  uint i;
  if (!g.productions.n) {
    logf("/*\n  There were no productions in the grammar\n*/\n");  
    return;
  }
  for (i = 1; i < g.productions.n; i++)
    print_production(g.productions.v[i]);
}

private void print_declare(string s, string n) {
    while(n.length && (n[0].isWhite() || n[0].isDigit())) n = n[1 .. $];
  logf(s, n);
}

private void
print_declarations(Grammar *g) {
  int i;

  if (g.tokenizer)
    logf("${declare tokenize}\n");
  for (i = 0; i < g.declarations.n; i++) {
    Declaration *dd = g.declarations.v[i];
    Elem *ee = dd.elem;
    switch (dd.kind) {
    case DeclarationKind.DECLARE_LONGEST_MATCH:
      if (g.longest_match)
        logf("${declare longest_match}\n");
      else
        print_declare("${declare longest_match %s}\n", ee.e.nterm.name);
      break;
    case DeclarationKind.DECLARE_ALL_MATCHES:
      if (!g.longest_match)
        logf("${declare all_matches}\n");
      else
        print_declare("${declare all_matches %s}\n", ee.e.nterm.name);
      break;
    default:
      logf("\n/*\nDeclaration.kind: %d", dd.kind);
      logf("\nElem.kind:        %d\n*/\n", ee.kind);
    }
  }
  if (g.set_op_priority_from_rule)
    logf("${declare set_op_priority_from_rule}\n");
  if (g.states_for_all_nterms)
    logf("${declare all_subparsers}\n");
  /* todo: DeclarationKind.DECLARE_STATE_FOR */
  if (g.default_white_space)
    logf("${declare whitespace %s}\n", g.default_white_space);
  if (g.save_parse_tree)
    logf("${declare save_parse_tree}\n");
  /* todo: DECLARE_NUM */

  if (g.scanner.code)
    logf("${scanner %s}\n", g.scanner.code);

  { int token_exists = 0;
    for (i = 0; i < g.terminals.n; i++) {
      Term *t = g.terminals.v[i];
      if (t.kind == TermKind.TERM_TOKEN) {
	writefln("%s %s", token_exists?"":"${token", t.string_);
	token_exists = 1;
      }
    }
    if (token_exists)
      logf("}\n");
  }

  logf("\n");
}

void
print_rdebug_grammar(Grammar *g) {
  logf("/*\n  Generated by Make DParser\n");  
  logf("  Available at http://dparser.sf.net\n*/\n\n");
  
  print_declarations(g);
  print_productions(g);
}

