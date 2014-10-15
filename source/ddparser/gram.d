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
import std.stdio;
import std.json;
import std.conv;
import ddparser.serialize;
import std.string;
import std.ascii;
import std.algorithm;
import std.array;

enum EOF_SENTINAL = "\377";
enum NO_PROD            =0xFFFFFFFF;


alias Item = Elem;

struct Code {
    union
    {
       string code;
       D_ReductionCode _f;
    }

    int line;
    void serialize(Serializer s)
    {
        //s.map(line, "line");
        //if (line != -1)
        //    s.mapCString(code, "code");
    }

    @property void f(D_ReductionCode c)
    {
        _f = c;
        line = -1;
    }

    @property D_ReductionCode f()
    {
        if (line == -1) return _f;
        return null;
    }
}

struct Goto {
    Elem    *elem;
    State   *state;
}

enum ActionKind {
    ACTION_ACCEPT, ACTION_SHIFT, ACTION_REDUCE, ACTION_SHIFT_TRAILING
}

struct Action {
    ActionKind      kind;
    Term        *term;
    Rule        *rule;
    State       *state;
    uint            index;
    string  temp_string;
}

alias VecAction = Vec!(Action*);

struct Hint {
    uint        depth;
    State   *state;
    Rule    *rule;
}
alias VecHint = Vec!(Hint*);

alias VecScanStateTransition = Vec!(ScanStateTransition*);

struct Scanner {
    ScanState*[] states;
    VecScanStateTransition  transitions;
}

struct State {
    uint        index;
    uint64  hash;
    Item*[] items;
    Vec!(Item*) items_hash;
    Goto*[] gotos;
    VecAction   shift_actions;
    VecAction   reduce_actions;
    VecHint right_epsilon_hints;
    VecHint error_recovery_hints;
    Scanner scanner;
    mixin(bitfields!(
                bool, "accept", 1,
                uint, "scanner_code", 1,
                uint, "goto_on_token", 1,
                uint, "scan_kind", 3,
                uint, "trailing_context", 1,
                uint, "", 25
                ));
    int     goto_table_offset;
    State   *same_shifts;
    State  *reduces_to;
    Rule   *reduces_with;
    Rule   *reduces_to_then_with;
}

enum ASSOC_LEFT     =0x0001;
enum ASSOC_RIGHT    =0x0002;
enum ASSOC_NARY     =0x0004;
enum ASSOC_UNARY    =0x0008;
enum ASSOC_BINARY   =0x0010;

enum AssocKind {
    ASSOC_NONE      = 0,
    ASSOC_NARY_LEFT     = (ASSOC_NARY|ASSOC_LEFT),
    ASSOC_NARY_RIGHT    = (ASSOC_NARY|ASSOC_RIGHT),
    ASSOC_UNARY_LEFT    = (ASSOC_UNARY|ASSOC_LEFT),
    ASSOC_UNARY_RIGHT   = (ASSOC_UNARY|ASSOC_RIGHT),
    ASSOC_BINARY_LEFT   = (ASSOC_BINARY|ASSOC_LEFT),
    ASSOC_BINARY_RIGHT  = (ASSOC_BINARY|ASSOC_RIGHT),
    ASSOC_NO        = 0x0020
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
    uint            index;
    Production  *prod;
    int         op_priority;
    AssocKind       op_assoc;
    int         rule_priority;
    AssocKind       rule_assoc;
    Elem*[]     elems;
    Elem        *end;
    Code            speculative_code;
    Code            final_code;
    Vec!(Code*)     pass_code;
    int         action_index;
    Rule        *same_reduction;

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

    @property ref Elem* lastElem()
    {
        return elems[$ - 1];
    }
}

enum TermKind {
    TERM_STRING, TERM_REGEX, TERM_CODE, TERM_TOKEN
}
struct Term {
    TermKind        kind;
    uint            index;
    int         term_priority;
    string      term_name;
    AssocKind       op_assoc;
    int         op_priority;
    string string_;
    mixin(bitfields!(
                uint, "scan_kind", 3,
                uint, "ignore_case", 1,
                uint, "trailing_context", 1,
                uint, "", 27
                ));
    Production  *regex_production;

    void serialize(Serializer s)
    {
        s.map(kind, "kind");
        s.map(index, "index");
        s.map(term_priority, "term_priority");
        //s.map(term_name, "term_name");
        s.map(op_assoc, "op_assoc");
        s.map(op_priority, "op_priority");
        //s.map(string_, "string_");

        mixin(fieldMap!("scan_kind", s));
        mixin(fieldMap!("ignore_case", s));
        mixin(fieldMap!("trailing_context", s));
        s.map(regex_production, "regex_production");
    }

}

enum DeclarationKind {
    DECLARE_TOKENIZE, DECLARE_LONGEST_MATCH, DECLARE_ALL_MATCHES,
    DECLARE_SET_OP_PRIORITY, DECLARE_STATES_FOR_ALL_NTERMS,
    DECLARE_STATE_FOR, DECLARE_WHITESPACE, DECLARE_SAVE_PARSE_TREE, DECLARE_NUM
}

alias DECLARE_NUM = DeclarationKind.DECLARE_NUM;

struct Declaration {
    Elem *  elem;
    DeclarationKind    kind;
    uint        index;

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
    string          name;
    Rule*[]         rules;
    uint            index;
    mixin(bitfields!(
                uint, "regex", 1,
                uint, "in_regex", 1,
                uint, "internal", 3,    /* production used for EBNF */
                uint, "live", 1,
                uint, "", 26
                ));
    Rule            *nullable;  /* shortest rule for epsilon reduction */
    Production  *declaration_group[DECLARE_NUM];
    Declaration     *last_declaration[DECLARE_NUM];
    State           *state; /* state for independent parsing of this productions*/
    Elem        *elem;  /* base elem for the item set of the above state */
    Term        *regex_term;    /* regex production terminal */
    Production  *next;

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
    ElemKind    kind;
    uint        index;
    Rule        *rule;
    union E {
        Production  *nterm;
        Term    *term;
        void    *term_or_nterm;
        string unresolved;
    }
    E e;

    @property inout(Term)* term() inout @trusted
    {
        assert(kind == ElemKind.ELEM_TERM);
        return e.term;
    }

    @property void term(Term* t) @trusted
    {
        e.term = t;
        kind = ElemKind.ELEM_TERM;
    }

    @property inout(Production)* nterm() inout @trusted
    {
        assert(kind == ElemKind.ELEM_NTERM);
        return e.nterm;
    }

    @property void nterm(Production* p)
    {
        kind = ElemKind.ELEM_NTERM;
        e.nterm = p;
    }

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
    Production*[]   productions;
    Term*[]         terminals;
    State*[]        states;
    Action*[]       actions;
    Code            scanner;
    /* Code         *code; */
    /* int          ncode; */
    Declaration*[]  declarations; 
    Vec!(D_Pass *)      passes;
    string          default_white_space;
    /* grammar construction options */
    int         set_op_priority_from_rule;
    int         right_recursive_BNF;
    int         states_for_whitespace;
    int         states_for_all_nterms;
    int         tokenizer;
    int         longest_match;
    int         save_parse_tree;
    /* grammar writing options */
    int         scanner_blocks;
    int         scanner_block_size;
    /* temporary variables for grammar construction */
    Production *    p;
    Rule *      r;
    Elem *      e;
    int         action_index;
    int         action_count;
    int         pass_index;
    int         rule_index;

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
    }
}

alias D_Grammar = Grammar;

/*
   Copyright 2002-2004 John Plevyak, All Rights Reserved
 */


immutable string action_types[] = [ "ACCEPT", "SHIFT", "REDUCE" ];


Production* new_production(Grammar *g, string name) @safe {
    Production *p  = lookup_production(g, name);
    if (p) {
        return p;
    }
    p = new Production();
    g.productions ~= p;
    p.name = name;
    return p;
}

Rule *
new_rule(Grammar *g, Production *p) @safe {
    Rule *r = new Rule();
    r.prod = p;
    r.end = new Elem();
    r.end.kind = ElemKind.ELEM_END;
    r.end.rule = r;
    r.action_index = g.action_index;
    return r;
}

private Elem *
new_elem_term(Term *t, Rule *r) @safe {
    Elem *e = new Elem();
    e.term = t;
    e.rule = r;
    r.elems ~= e;
    return e;
}

Elem *
new_elem_nterm(Production *p, Rule *r) @safe {
    Elem *e = new Elem();
    e.kind = ElemKind.ELEM_NTERM;
    e.e.nterm = p;
    e.rule = r;
    return e;
}

private Elem *
new_term_string(Grammar *g, string s, Rule *r) @safe
{
    Term *t = new Term();
    t.string_ = s;
    g.terminals ~= t;
    return new_elem_term(t, r);
}

string escape_string_for_regex(const char[] s) @safe
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

private string unescapeTermString(const(char)[] termString, TermKind kind) @safe
{
    string result;

    uint start = 0;
    int length;
    uint base = 0;


    for (uint i = 0; i < termString.length; ++i) {
        if (termString[i] == '\\') {
            switch (termString[i + 1]) {
                case '\\':
                    if (kind == TermKind.TERM_STRING)
                    { result ~= '\\'; i++; break; }
                    else
                        goto default;
                case 'b': result ~= '\b'; i++; break;
                case 'f': result ~= '\f'; i++; break;
                case 'n': result ~= '\n'; i++; break;
                case 'r': result ~= '\r'; i++; break;
                case 't': result ~= '\t'; i++; break;
                case 'v': result ~= '\v'; i++; break;
                case 'a': result ~= '\a'; i++; break;
                case 'c': assert(false); //return;
                case '\"':
                          if (kind == TermKind.TERM_REGEX)
                          { result ~= '\"'; i++; break; }
                          else
                              goto default;
                case '\'':
                          if (kind == TermKind.TERM_STRING)
                          { result ~= '\''; i++; break; }
                          else
                              goto default;
                case 'x':
                          length = 0;
                          if (termString[i + 2].isHexDigit()) {
                              base = 16;
                              start = i + 2;
                              length++;
                              if (termString[i + 3].isHexDigit())
                                  length++;
                          }
                          i += length + 1;
                          goto Lncont;
                case 'd':
                          length = 0;
                          if (termString[i + 2].isDigit()) {
                              base = 10;
                              start = i + 2;
                              length++;
                              if ((termString[i + 3]).isDigit()) {
                                  length++;
                                  if (termString[i + 4].isDigit() && ((termString[i + 2] < '2') || ((termString[i + 2] == '2') && ((termString[i + 3] < '5') ||
                                                      ((termString[i + 3] == '5') && (termString[i + 4] < '6'))))))
                                      length++;
                              }
                          }
                          i += length + 1;
                          goto Lncont;
                case '0': case '1': case '2': case '3':
                case '4': case '5': case '6': case '7':
                          length = 1;
                          base = 8;
                          start = i + 1;
                          if (termString[i + 2].isDigit() && (termString[i + 2] != '8') && (termString[i + 2] != '9')) {
                              length++;
                              if (termString[i + 3].isDigit() && (termString[i + 3] != '8') && (termString[i + 3] != '9')) {
                                  length++;
                              }
                          }
                          i += length;
                          /* fall through */
Lncont:
                          if (length > 0) {
                              result ~= cast(char)termString[start .. start + length].to!ubyte(base);
                              if (i < termString.length)
                                  break;
                              d_fail("encountered an escaped null while processing '%s'", termString);
                          } else
                              goto next;
                          break;
                default:
                          result ~= termString[i];
                          result ~= termString[i + 1];
                          i ++;
                          break;
            }
        }
        else
        {
            result ~= termString[i];
        }
next:;
    }

    if (!result.length)
        d_fail("empty string after unescape '%s'", termString);

    return result;
}

private void
unescape_term_string(Term *t) {
    t.string_ = unescapeTermString(t.string_, t.kind);
}

Elem * new_string(Grammar *g, string s, Rule *r)
{
    Elem *x = new_term_string(g, s[1 .. $ - 1], r);
    x.e.term.kind = (s[0] == '"') ? TermKind.TERM_REGEX : TermKind.TERM_STRING;
    unescape_term_string(x.e.term);
    return x;
}

Elem *
new_utf8_char(Grammar *g, const(char) *s, const(char) *e, Rule *r) {
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
new_ident(string s, Rule *r) @safe
{
    Elem *x = new Elem();
    x.kind = ElemKind.ELEM_UNRESOLVED;
    x.e.unresolved = s;
    x.rule = r;
    if (r)
        r.elems ~= x;
    return x;
}

void
new_token(Grammar *g, string s) @safe {
    Term *t = new Term();
    t.string_ = s;
    g.terminals ~= t;
    t.kind = TermKind.TERM_TOKEN;
}

Elem *
new_code(Grammar *g, string s, Rule *r) {
    Elem *x = new_term_string(g, s, r);
    x.e.term.kind = TermKind.TERM_CODE;
    return x;
}

Elem *
dup_elem(Elem *e, Rule *r) @safe {
    Elem *ee = new Elem();
    *ee = *e;
    ee.rule = r;
    return ee;
}

void
new_declaration(Grammar *g, Elem *e, DeclarationKind kind) {
    Declaration *d = new Declaration();
    d.elem = e;
    d.kind = kind;
    d.index = cast(uint)g.declarations.length;
    g.declarations ~= d;
}

void
add_declaration(Grammar *g, string s, DeclarationKind kind, uint line) {
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
find_pass(Grammar *g, const(char)[] name) {
    name = name.stripLeft();
    foreach (p; g.passes)
        if (p.name == name)
            return p;
    return null;
}

void
add_pass(Grammar *g, string name, uint kind, uint line) {
    if (find_pass(g, name))
        d_fail("duplicate pass '%s' line %d", name, line);
    else {
        D_Pass *p = new D_Pass();
        p.name = name;
        p.kind = kind;
        p.index = g.pass_index++;
        vec_add(&g.passes, p);
    }
}

    void
add_pass_code(Grammar *g, Rule *r, string name,
        string code, uint pass_line, uint code_line)
{
    D_Pass *p = find_pass(g, name);
    if (!p)
        d_fail("unknown pass '%s' line %d", name, pass_line);
    while (r.pass_code.length <= p.index) vec_add(&r.pass_code, null);
    r.pass_code[p.index] = new Code();
    r.pass_code[p.index].code = code;
    r.pass_code[p.index].line = code_line;
}


Production *
new_internal_production(Grammar *g, Production *p) @safe {
    string n = p ? p.name : " _synthetic";
    string name = n ~ "__" ~ g.productions.length.to!string();
    Production *pp = new_production(g, name);
    pp.internal = InternalKind.INTERNAL_HIDDEN;
    pp.regex = p ? p.regex : 0;
    if (p) {
        bool found = false;
        Production *tp = null, ttp;
        for (int i = 0; i < g.productions.length; i++) {
            if (found) {
                ttp = g.productions[i];
                g.productions[i] = tp;
                tp = ttp;
            } else if (p == g.productions[i]) {
                found = true;
                tp = g.productions[i+1];
                g.productions[i+1] = pp;
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
    rr.elems ~= g.r.lastElem;
    g.r.lastElem.rule = rr;
    rr.lastElem.rule = rr;
    pp.rules ~= rr;
    pp.rules ~= new_rule(g, pp);
    g.r.lastElem = new_elem_nterm(pp, g.r);
}

void
star_EBNF(Grammar *g) {
    Production *pp = new_internal_production(g, g.p);
    pp.internal = InternalKind.INTERNAL_STAR;
    Rule *rr = new_rule(g, pp);
    if (!g.right_recursive_BNF) {
        rr.elems ~= new_elem_nterm(pp, rr);
        rr.elems ~= g.r.lastElem;
        g.r.lastElem = new_elem_nterm(pp, g.r);
        rr.lastElem.rule = rr;
    } else {
        rr.elems ~= g.r.lastElem;
        g.r.lastElem = new_elem_nterm(pp, g.r);
        rr.lastElem.rule = rr;
        rr.elems ~= new_elem_nterm(pp, rr);
    }
    pp.rules ~= rr;
    pp.rules ~= new_rule(g, pp);
}

void
plus_EBNF(Grammar *g) {
    Production *pp = new_internal_production(g, g.p);
    pp.internal = InternalKind.INTERNAL_PLUS;
    Rule *rr = new_rule(g, pp);
    Elem *elem = g.r.lastElem;
    if (!g.right_recursive_BNF) {
        rr.elems ~= new_elem_nterm(pp, rr);
        rr.elems ~= dup_elem(elem, rr);
        g.r.lastElem = new_elem_nterm(pp, g.r);
        if (g.r.rule_priority) {
            rr.rule_priority = g.r.rule_priority;
            rr.rule_assoc = AssocKind.ASSOC_NARY_LEFT;
        }
    } else {
        rr.elems ~= dup_elem(elem, rr);
        g.r.lastElem = new_elem_nterm(pp, g.r);
        rr.elems ~= new_elem_nterm(pp, rr);
        if (g.r.rule_priority) {
            rr.rule_priority = g.r.rule_priority;
            rr.rule_assoc = AssocKind.ASSOC_NARY_RIGHT;
        }
    }
    pp.rules ~= rr;
    rr = new_rule(g, pp);
    rr.elems ~= elem;
    elem.rule = rr;
    pp.rules ~= rr;
}

void
rep_EBNF(Grammar *g, int min, int max) {
    if (max < min) max = min;

    Production *pp = new_internal_production(g, g.p);
    Elem *elem = g.r.lastElem;
    for (int i = min; i <= max; i++) {
        Rule *rr = new_rule(g, pp);
        for (int j = 0; j < i; j++)
            rr.elems ~= dup_elem(elem, rr);
        pp.rules ~= rr;
    }
    g.r.lastElem = new_elem_nterm(pp, g.r);
    FREE(elem);
}

void
initialize_productions(Grammar *g) @safe {
    Production *pp = new_production(g, "0 Start");
    pp.internal = InternalKind.INTERNAL_HIDDEN;
}

void
finish_productions(Grammar *g) {
    Production *pp = g.productions[0];
    Rule *rr = new_rule(g, pp);
    rr.elems ~= new_elem_nterm(null, rr);
    pp.rules ~= rr;
    rr.elems[0].e.nterm = g.productions[1];
}

Production* lookup_production(Grammar* g, const(char)[] name) @safe
{
    foreach(p; g.productions)
        if (p.name == name) return p;
    return null;
}

private Term *
lookup_token(Grammar *g, const(char)[] name) @safe {
    foreach(t; g.terminals)
        if (t.kind == TermKind.TERM_TOKEN && t.string_ == name)
            return t;
    return null;
}

private Term *
unique_term(Grammar *g, Term *t) @safe {
    foreach (i; g.terminals)
        if (t.kind == i.kind &&
                t.term_priority == i.term_priority &&
                t.term_name == i.term_name &&
                (!g.set_op_priority_from_rule ||
                 (t.op_assoc == i.op_assoc &&
                  t.op_priority == i.op_priority)) &&
                t.string_ == i.string_)
            return i;
    return t;
}

private void
compute_nullable(Grammar *g) {
    /* ensure that the trivial case is the first cause */
    foreach(p; g.productions) {
        foreach (r; p.rules)
            if (!r.elems.length) {
                p.nullable = r;
                break;
            }
    }

    bool changed = true;
    /* transitive closure */
    while (changed) {
        changed = false;
        foreach (p; g.productions) {
            if (!p.nullable)
                foreach (r; p.rules) {
                    foreach (e; r.elems) {
                        if (e.kind != ElemKind.ELEM_NTERM || !e.e.nterm.nullable)
                            goto Lnot_nullable;
                    }
                    changed = true;
                    p.nullable = r;
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
    Elem *e;
    Term *last_term, t;

    g.rule_index = 0;
    for (int i = 0; i < g.productions.length; i++) {
        p = g.productions[i];
        if (p != lookup_production(g, p.name))
            d_fail("duplicate production '%s'", p.name);
        p.index = i;
        foreach (r; p.rules) {
            r.index = g.rule_index++;
            last_term = null;
            for (int k = 0; k < r.elems.length; k++) {
                e = r.elems[k];
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
            r.end.index = cast(uint)r.elems.length;
            if (g.set_op_priority_from_rule) {
                if (last_term && r.rule_assoc) {
                    last_term.op_assoc = r.rule_assoc;
                    last_term.op_priority = r.rule_priority;
                }
            }
        }
    }
    for (int i = 0; i < g.terminals.length; i++)
        g.terminals[i].index = i;
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
    string s = t.string_ ? escape_string(t.string_) : null;
    if (t.term_name)
        logf("term_name(\"%s\") ", t.term_name);
    else if (t.kind == TermKind.TERM_STRING) {
        if (!t.string_.length)
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
    for (k = 0; k < r.elems.length; k++)
        print_elem(r.elems[k]);
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

    if (!g.productions.length)
        return;
    logf("PRODUCTIONS\n\n");
    for (i = 0; i < g.productions.length; i++) {
        pp = g.productions[i];
        logf("%s (%d)\n", pp.name, i);
        for (j = 0; j < pp.rules.length; j++) {
            rr = pp.rules[j];
            if (!j)
                logf("\t: ");
            else
                logf("\t| ");
            for (k = 0; k < rr.elems.length; k++)
                print_elem(rr.elems[k]);
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
    for (i = 0; i < g.terminals.length; i++) {
        logf("\t");
        print_term(g.terminals[i]);
        logf("(%d)\n", i + g.productions.length);
    }
    logf("\n");
}

private void
print_item(Item *i) {
    int j, end = 1;

    logf("\t%s: ", i.rule.prod.name);
    for (j = 0; j < i.rule.elems.length; j++) {
        Elem *e = i.rule.elems[j];
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

    writefln("STATE %d (%d ITEMS)%s", s.index, s.items.length,
            s.accept ? " ACCEPT" : "");
    for (j = 0; j < s.items.length; j++)
        print_item(s.items[j]);
    if (s.gotos.length)
        logf("  GOTO\n");
    for (j = 0; j < s.gotos.length; j++) {
        logf("\t");
        print_elem(s.gotos[j].elem);
        logf(" : %d\n", s.gotos[j].state.index);
    }
    logf("  ACTION\n");
    for (j = 0; j < s.reduce_actions.length; j++) {
        Action *a = s.reduce_actions[j];
        writefln("\t%s\t", action_types[a.kind]);
        print_rule(a.rule);
        logf("\n");
    }
    for (j = 0; j < s.shift_actions.length; j++) {
        Action *a = s.shift_actions[j];
        writefln("\t%s\t", action_types[a.kind]);
        if (a.kind == ActionKind.ACTION_SHIFT) {
            print_term(a.term);
            logf("%d", a.state.index);
        }
        logf("\n");
    }
    if (s.reduce_actions.length > 1)
        print_conflict("reduce/reduce", &conflict);
    if (s.reduce_actions.length && s.shift_actions.length)
        print_conflict("shift/reduce", &conflict);
    logf("\n");
}

void
print_states(Grammar *g) {
    int i;

    for (i = 0; i < g.states.length; i++)
        print_state(g.states[i]);
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

    Production *pp = g.productions[0];
    for (i = 0; i < g.productions.length; i++)
        if (!g.productions[i].internal) {
            if (g.states_for_all_nterms ||
                    state_for_declaration(g, i)) {
                /* try to find an existing elem */
                for (j = 0; j < g.productions.length; j++)
                    for (k = 0; k < g.productions[j].rules.length; k++) {
                        rr = g.productions[j].rules[k];
                        for (l = 0; l < rr.elems.length; l++)
                            if (rr.elems[l].e.nterm == g.productions[i]) {
                                g.productions[i].elem = rr.elems[l];
                                break;
                            }
                    }
                if (j >= g.productions.length) { /* not found */
                    g.productions[i].elem =
                        new_elem_nterm(g.productions[i], new_rule(g, pp));
                    g.productions[i].elem.rule.index = g.rule_index++; /* fake */
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
    size_t l;
    Rule *rr;

    if (p.regex_term) /* already done */
        return;

    bool circular = false;

    if (p.in_regex)
        d_fail("circular regex production '%s'", p.name);
    p.in_regex = 1;
    foreach (r; p.rules) {
        if (r.final_code.code || (r.speculative_code.code && p.rules.length > 1))
            d_fail("final and/or multi-rule code not permitted in regex productions '%s'", p.name);
        foreach (e; r.elems) {
            if (e.kind == ElemKind.ELEM_NTERM) {
                if (!e.e.nterm.regex)
                    d_fail("regex production '%s' cannot invoke non-regex production '%s'",
                            p.name, e.e.nterm.name);
                Production *pp = e.e.nterm;
                for (l = 0; l < pp.rules.length; l++)
                    if (pp.rules[l].speculative_code.code || pp.rules[l].final_code.code)
                        d_fail("code not permitted in rule %d of regex productions '%s'", l, p.name);
                if (p != pp) {
                    convert_regex_production_one(g, pp);
                } else {
                    circular = true;
                }
            } else { /* e.kind == ElemKind.ELEM_TERM */
                if (e.e.term.kind == TermKind.TERM_CODE || e.e.term.kind == TermKind.TERM_TOKEN)
                    d_fail("regex production '%s' cannot include scanners or tokens");
            }
        }
    }
    string buffer;
    Term *t = new Term();
    t.kind = TermKind.TERM_REGEX;
    t.index = cast(uint)g.terminals.length;
    t.regex_production = p;
    g.terminals ~= t;
    p.regex_term = t;
    p.regex_term.term_name = p.name;
    //Elem *e;
    if (circular) { /* attempt to match to regex operators */
        if (p.rules.length != 2)
            Lfail: d_fail("unable to resolve circular regex production: '%s'", p.name);
        l = p.rules[0].elems.length + p.rules[1].elems.length;
        if (l == 2 || l == 3) {
            if (p.rules[0].elems.length != 2 && p.rules[1].elems.length != 2)
                goto Lfail;
            Rule *r = p.rules[0].elems.length == 2 ? p.rules[0] : p.rules[1];
            rr = p.rules[0] == r ? p.rules[1] : p.rules[0];
            if (r.elems[0].e.nterm != p && r.elems[1].e.nterm != p)
                goto Lfail;
            Elem *e = r.elems[0].e.nterm == p ? r.elems[1] : r.elems[1];
            if (rr.elems.length && e.e.term_or_nterm != rr.elems[0].e.term_or_nterm)
                goto Lfail;
            t = e.kind == ElemKind.ELEM_TERM ? e.e.term : e.e.nterm.regex_term;
            buffer ~= '(';
            if (t.kind == TermKind.TERM_STRING)
                buffer ~= escape_string_for_regex(t.string_);
            else
                buffer ~= t.string_;
            buffer ~= ')';
            if (l == 2)
            {   buffer ~= '*'; }
            else
            {   buffer ~= '+'; }
            p.regex_term.string_ = buffer;
        } else
            goto Lfail;
    } else { /* handle the base case, p = (r | r'), r = (e e') */
        if (p.rules.length > 1)
        {  buffer ~= '('; }
        for (int j = 0; j < p.rules.length; j++) {
            Rule *r = p.rules[j];
            if (r.elems.length > 1)
            { buffer ~= '('; }
            foreach (e; r.elems) {
                t = e.kind == ElemKind.ELEM_TERM ? e.e.term : e.e.nterm.regex_term;
                if (t.kind == TermKind.TERM_STRING)
                    buffer ~= escape_string_for_regex(t.string_);
                else
                    buffer ~= t.string_;
            }
            if (r.elems.length > 1)
            { buffer ~= ')'; }
            if (j != p.rules.length - 1)
            { buffer ~= '|'; }
        }
        if (p.rules.length > 1)
        {  buffer ~= ')'; }
        p.regex_term.string_ = buffer;
    }
    p.in_regex = 0;
}

private void
convert_regex_productions(Grammar *g) {
    foreach (p; g.productions) {
        if (!p.regex)
            continue;
        convert_regex_production_one(g, p);
    }
    foreach (p; g.productions) {
        foreach (r; p.rules) {
            foreach (e; r.elems) {
                if (e.kind == ElemKind.ELEM_NTERM && e.e.nterm.regex_term) {
                    e.e.term = e.e.nterm.regex_term;
                    e.kind = ElemKind.ELEM_TERM;
                }
            }
        }
    }
}

private void
check_default_actions(Grammar *g) {
    Production *pdefault;

    pdefault = lookup_production(g, "_");
    if (pdefault && pdefault.rules.length > 1)
        d_fail("number of rules in default action != 1");
}

struct EqState {
    State   *eq;
    Rule    *diff_rule;
    State   *diff_state;
}

void
build_eq(Grammar *g) {
    int i, j, k, changed = 1;
    State *s, ss;
    EqState *e, ee;

    EqState[] eq = new EqState[g.states.length];
    while (changed) {
        changed = 0;
        for (i = 0; i < g.states.length; i++) {
            s = g.states[i];
            e = &eq[s.index];
            for (j = i + 1; j < g.states.length; j++) {
                ss = g.states[j];
                ee = &eq[ss.index];
                if (e.eq || ee.eq)
                    continue;
                if (s.same_shifts != ss.same_shifts && ss.same_shifts != s)
                    continue;
                /* check gotos */
                if (s.gotos.length != ss.gotos.length)
                    continue;
                for (k = 0; k < s.gotos.length; k++) {
                    if (elem_symbol(g, s.gotos[k].elem) != elem_symbol(g, ss.gotos[k].elem))
                        goto Lcontinue;
                    if (s.gotos[k].state != ss.gotos[k].state) {
                        EqState *ge = &eq[s.gotos[k].state.index];
                        EqState *gee = &eq[ss.gotos[k].state.index];
                        if (ge.eq != ss.gotos[k].state && gee.eq != s.gotos[k].state)
                            goto Lcontinue;
                        if ((ee.diff_state && ee.diff_state != eq[ss.gotos[k].state.index].eq) ||
                                (e.diff_state && e.diff_state != eq[s.gotos[k].state.index].eq))
                            goto Lcontinue;
                        /* allow one different state */
                        ee.diff_state = ss.gotos[k].state;
                        e.diff_state = s.gotos[k].state;
                    }
                }
                /* check reductions */
                if (s.reduce_actions.length != ss.reduce_actions.length)
                    continue;
                for (k = 0; k < s.reduce_actions.length; k++) {
                    if (s.reduce_actions[k].rule == ss.reduce_actions[k].rule)
                        continue;
                    if (s.reduce_actions[k].rule.prod !=
                            ss.reduce_actions[k].rule.prod)
                        goto Lcontinue;
                    if (s.reduce_actions[k].rule.elems.length !=
                            ss.reduce_actions[k].rule.elems.length) {
                        if ((ee.diff_rule && ee.diff_rule != ss.reduce_actions[k].rule) ||
                                (e.diff_rule && e.diff_rule != s.reduce_actions[k].rule))
                            goto Lcontinue;
                        /* allow one different rule */
                        ee.diff_rule = ss.reduce_actions[k].rule;
                        e.diff_rule = s.reduce_actions[k].rule;
                    }
                }
                ee.eq = s;
                changed = 1;
Lcontinue:;
            }
        }
    }
    for (i = 0; i < g.states.length; i++) {
        s = g.states[i];
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
    for (i = 0; i < g.states.length; i++) {
        s = g.states[i];
        e = &eq[s.index];
        if (e.eq && e.diff_state) {
            if (eq[e.diff_state.index].diff_rule &&
                    eq[e.diff_state.index].diff_rule.elems.length == 2)
            {
                s.reduces_to = e.eq;
                s.reduces_with = eq[e.eq.index].diff_rule;
                s.reduces_to_then_with = e.diff_rule;
            } else if (eq[eq[e.eq.index].diff_state.index].diff_rule &&
                    eq[eq[e.eq.index].diff_state.index].diff_rule.elems.length == 2)
            {
                e.eq.reduces_to = s;
                s.reduces_with = e.diff_rule;
                s.reduces_to_then_with = eq[e.eq.index].diff_rule;
            }
        }
    }
    for (i = 0; i < g.states.length; i++) {
        s = g.states[i];
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
    vec_free(&r.pass_code);
    FREE(r);
}

void
free_D_Grammar(Grammar *g) {
    int i, j, k;

    for (i = 0; i < g.productions.length; i++) {
        Production *p = g.productions[i];
        for (j = 0; j < p.rules.length; j++) {
            Rule *r = p.rules[j];
            if (r == g.r)
                g.r = null;
            for (k = 0; k < r.elems.length; k++) {
                Elem *e = r.elems[k];
                if (e == p.elem)
                    p.elem = null;
                FREE(e);
            }
            if (r.end == p.elem)
                p.elem = null;
            free_rule(r);
        }
        if (p.elem) {
            free_rule(p.elem.rule);
            FREE(p.elem);
        }
        FREE(p);
    }
    for (i = 0; i < g.actions.length; i++)
        free_Action(g.actions[i]);
    for (i = 0; i < g.states.length; i++) {
        State *s = g.states[i];
        vec_free(&s.items_hash);
        for (j = 0; j < s.gotos.length; j++) {
            FREE(s.gotos[j].elem);
            FREE(s.gotos[j]);
        }
        for (j = 0; j < s.right_epsilon_hints.length; j++)
            FREE(s.right_epsilon_hints[j]);
        vec_free(&s.right_epsilon_hints);
        for (j = 0; j < s.error_recovery_hints.length; j++)
            FREE(s.error_recovery_hints[j]);
        vec_free(&s.error_recovery_hints);
        if (!s.same_shifts) {
            for (j = 0; j < s.scanner.states.length; j++) {
                vec_free(&s.scanner.states[j].accepts);
                vec_free(&s.scanner.states[j].live);
                FREE(s.scanner.states[j]);
            }
            for (j = 0; j < s.scanner.transitions.length; j++)
                if (s.scanner.transitions[j]) {
                    vec_free(&s.scanner.transitions[j].live_diff);
                    vec_free(&s.scanner.transitions[j].accepts_diff);
                    FREE(s.scanner.transitions[j]);
                }
            vec_free(&s.scanner.transitions);
        }
        FREE(s);
    }
    for (i = 0; i < g.declarations.length; i++) {
        FREE(g.declarations[i].elem);
        FREE(g.declarations[i]);
    }
    for (i = 0; i < g.passes.length; i++) {
        FREE(g.passes[i]);
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
    if (p.declaration_group[d.kind] == root)
        return;
    if (d.kind == DeclarationKind.DECLARE_TOKENIZE && p.declaration_group[d.kind]) {
        d_fail("shared tokenize subtrees");
        return;
    }
    p.declaration_group[d.kind] = root;
    p.last_declaration[d.kind] = d;
    foreach (r; p.rules) {
        foreach (e; r.elems)
            if (e.kind == ElemKind.ELEM_NTERM)
                set_declaration_group(e.e.nterm, root, d);
    }
}

private void
propogate_declarations(Grammar *g) {
    Production *start = g.productions[0];

    /* global defaults */
    if (g.tokenizer)
        new_declaration(g, new_elem_nterm(g.productions[0], null), DeclarationKind.DECLARE_TOKENIZE);
    if (g.longest_match)
        new_declaration(g, new_elem_nterm(g.productions[0], null), DeclarationKind.DECLARE_LONGEST_MATCH);
    /* resolve declarations */
    foreach (d; g.declarations) {
        Elem *e = d.elem;
        if (e.kind == ElemKind.ELEM_UNRESOLVED) {
            Production *p;
            if (e.e.unresolved.length == 0)
                p = g.productions[0];
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
            Production *p = d.elem.e.nterm;
            if (p == start) {
                foreach (pp; g.productions) {
                    pp.declaration_group[d.kind] = start;
                    pp.last_declaration[d.kind] = d;
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
    foreach (f; from.shift_actions) {
        foreach (t; to.shift_actions)
            if (f.term == t.term)
                goto Lnext;
        to.shift_actions ~= f;
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
    foreach (s; g.states) {
        s.scan_kind = D_SCAN_RESERVED;
    }
    /* map groups to sets of states */
    foreach (d; g.declarations)
        if (scanner_declaration(d))
            compute_declaration_states(g, d.elem.e.nterm, d);
    foreach (s; g.states) {
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
                g.productions.length, g.terminals.length, g.states.length,
                g.declarations.length);
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
        s = t.string_ ? escape_string_single_quote(t.string_) : null;
        if (!t.string_)
            logf("<EOF> ");
        else {
            logf("'%s' ", double_escaped ? escape_string_single_quote(s) : s);
            if (t.ignore_case)
                logf("/i ");
            if (t.term_priority)
                writefln("%sterm %d ", double_escaped?"#":"$", t.term_priority);
        }
    } else if (t.kind == TermKind.TERM_REGEX) {
        s = t.string_ ? escape_string(t.string_) : null;
        //char *s = t.string_; // ? escape_string(t.string_) : null;
        string quote = double_escaped ? "\\\"" : "\"";
        logf("%s%s%s ", quote, double_escaped ? escape_string(s) : s, quote);
        if (t.ignore_case)
            logf("/i ");
        if (t.term_priority)
            writefln("%sterm %d ", double_escaped?"#":"$", t.term_priority);
    } else if (t.kind == TermKind.TERM_CODE) {
        s = t.string_ ? escape_string(t.string_) : null;
        logf("code(\"%s\") ", s);
    } else if (t.kind == TermKind.TERM_TOKEN) {
        s = t.string_ ? escape_string(t.string_) : null;
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

    for (j = 0; j < p.rules.length; j++) {
Lmore:
        r = p.rules[j];
        if (!j) {
            //      if (p.regex) {
            //  logf("%s%s%s", opening[variant], p.name, regex_production);
            //      } else {
            writefln("%s%s%s", opening[variant], p.name, middle[variant]);
            //      }
        } else {
            if (variant==0)
                writefln("%s", next_or_rule);
            else
                writefln("%s%s%s", opening[variant], p.name, middle[variant]);
        }

        for (k = 0; k < r.elems.length; k++)
            print_element_escaped(r.elems[k], variant);

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
    if (!g.productions.length) {
        logf("/*\n  There were no productions in the grammar\n*/\n");
        return;
    }
    for (i = 1; i < g.productions.length; i++)
        print_production(g.productions[i]);
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
    for (i = 0; i < g.declarations.length; i++) {
        Declaration *dd = g.declarations[i];
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
        for (i = 0; i < g.terminals.length; i++) {
            Term *t = g.terminals[i];
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

