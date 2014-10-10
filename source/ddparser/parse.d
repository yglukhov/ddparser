/*
  Copyright 2002-2008 John Plevyak, All Rights Reserved
*/

//#include "d.h"
module ddparser.parse;

import std.stdio;
import std.ascii;
import std.algorithm;

import ddparser.util;
import ddparser.dparse_tables;
import ddparser.dparse;
import ddparser.scan;
import ddparser.gram;
import ddparser.symtab;
//import core.stdc.stdio;
import core.stdc.string;
import core.stdc.stdlib;

enum NO_DPN = cast(D_ParseNode*)0x1;
PNode* DPN_TO_PN(D_ParseNode* dpn)
{
    return (cast(PNode *)((cast(char*)dpn)-cast(intptr_t)(&(cast(PNode*)0).parse_node)));
}

bool is_epsilon_PNode(PNode* _pn)
{
    return (_pn.parse_node.start_loc.s == _pn.parse_node.end);
}


alias VecZNode = Vec!(ZNode*);
alias VecVecZNode = Vec!(VecZNode*);
alias VecSNode = Vec!(SNode*);
alias VecPNode = Vec!(PNode*);

struct PNodeHash {
  PNode	**v;
  uint		i;	/* size index (power of 2) */
  uint  	m;	/* max size (highest prime < i ** 2) */
  uint  	n;	/* size */
  PNode  *all;
}

struct SNodeHash {
  SNode  **v;
  uint		i;	/* size index (power of 2) */
  uint  	m;	/* max size (highest prime < i ** 2) */
  uint  	n;	/* size */
  SNode  *all;
  SNode  *last_all;
}

struct Reduction {
  ZNode		*znode;
  SNode		*snode;
  D_Reduction	*reduction;
  SNode		*new_snode;
  int			new_depth;
  Reduction	*next;
}

struct Shift {
  SNode		*snode;
  Shift		*next;
}

struct Parser {
  D_ParseNode_Globals	*initial_globals;		/* global values */
  D_WhiteSpaceFn 	initial_white_space_fn;
  D_Scope 	*initial_scope;
  D_SyntaxErrorFn 	syntax_error_fn;
  D_AmbiguityFn 	ambiguity_fn;
  D_FreeNodeFn          free_node_fn;
  d_loc_t 		loc; 		/* initial location, set on error */
  int			start_state; // do not move or change without fixing copy_user_configurables()
  /* user configurables */
  int 			sizeof_user_parse_node;
  int 			save_parse_tree;
  int			dont_compare_stacks;
  int 			dont_fixup_internal_productions;
  int 			fixup_EBNF_productions;
  int			dont_merge_epsilon_trees;
  int			dont_use_height_for_disambiguation;
  int			dont_use_greediness_for_disambiguation;
  int 			commit_actions_interval; /* 0 is immediate */
  int 			error_recovery;
  bool			partial_parses;
  /* parse results */
  int 			syntax_errors; // do not move or change without fixing copy_user_configurables()

  /* string to parse */
  char *start, end;

private:
  D_ParserTables *t;
  /* statistics */
  int states, pnodes, scans, shifts, reductions, compares, ambiguities;
  /* parser state */
  PNodeHash pnode_hash;
  SNodeHash snode_hash;
  Reduction *reductions_todo;
  Shift *shifts_todo;
  D_Scope *top_scope;
  SNode *accept;
  int last_syntax_error_line;
  /* memory management */
  Reduction *free_reductions;
  Shift *free_shifts;
  int live_pnodes;
  PNode *free_pnodes;
  SNode *free_snodes;
  ZNode *free_znodes;
  Vec!(D_Reduction *) error_reductions;
  ShiftResult[] shift_results;
  D_Shift[] code_shifts;
  /* comments */
  Parser *whitespace_parser;
}

alias D_Parser = Parser;

/*
  Parse Node - the 'symbol' and the constructed parse subtrees.
*/
struct PNode {
  uint			hash;
  AssocKind		assoc;
  int			priority;
  AssocKind		op_assoc;
  int			op_priority;
  D_Reduction		*reduction;
  const(D_Shift)* shift;
  VecPNode		children;
  uint			height;		/* max tree height */
  uint8			evaluated;
  uint8			error_recovery;
  PNode		*all_next;
  PNode		*bucket_next;
  PNode		*ambiguities;
  PNode		*latest;	/* latest version of this PNode */
  char			*ws_before;
  char			*ws_after;
  D_Scope               *initial_scope;
  void                  *initial_globals;
  D_ParseNode		parse_node;	/* public fields */
}

/*
  State Node - the 'state'.
*/
struct SNode {
  D_Scope	*initial_scope;
  void		*initial_globals;
  d_loc_t	loc;
  PNode		*last_pn;
  VecZNode	zns;
  SNode  *bucket_next;
  SNode	*all_next;
  uint stateIndex;
  uint		depth;	     	/* max stack depth (less loops) */
}

/*
  (Z)Symbol Node - holds one of the symbols associated with a state.
*/
struct ZNode {
  PNode		*pn;
  VecSNode	sns;
}

ZNode* znode_next(ZNode* _z)
{
    assert((*cast(ZNode**)&(_z.pn)) == cast(ZNode*)_z.pn);
    return  (*cast(ZNode**)&(_z.pn));
}


/* tunables */
enum DEFAULT_COMMIT_ACTIONS_INTERVAL =         100;
enum PNODE_HASH_INITIAL_SIZE_INDEX =          10;
enum SNODE_HASH_INITIAL_SIZE_INDEX  =         8;
enum ERROR_RECOVERY_QUEUE_SIZE       =        10000;


void LATEST(ref PNode * _pn)
{
 while (_pn.latest != _pn.latest.latest) {
    PNode *t = _pn.latest.latest;
    _pn.latest = t; 
  }
 _pn = _pn.latest;
}


alias StackPNode = Stack!(PNode*);
alias StackSNode = Stack!(SNode*);
alias StackInt = Stack!int;


private void
print_paren(Parser *pp, PNode *p) {
  int i;
  char *c;
  LATEST(p);
  if (!p.error_recovery) {
    if (p.children.n) {
      if (p.children.n > 1)
        logf("(");
      for (i = 0; i < p.children.n; i++)
        print_paren(pp, p.children.v[i]);
      if (p.children.n > 1)
        logf(")");
    } else if (p.parse_node.start_loc.s != p.parse_node.end_skip) {
      logf(" ");
      for (c = p.parse_node.start_loc.s; c < p.parse_node.end_skip; c++)
        logf("%c", *c);
      logf(" ");
    }
  }
}

void
xprint_paren(Parser *pp, PNode *p) {
  int i;
  char *c;
  LATEST(p);
  if (!p.error_recovery) {
    logf("[%X %s]", p, pp.t.symbols[p.parse_node.symbol].name);
    if (p.children.n) {
      logf("(");
      for (i = 0; i < p.children.n; i++)
        xprint_paren(pp, p.children.v[i]);
      logf(")");
    } else if (p.parse_node.start_loc.s != p.parse_node.end_skip) {
      logf(" ");
      for (c = p.parse_node.start_loc.s; c < p.parse_node.end_skip; c++)
        logf("%c", *c);
      logf(" ");
    }
    if (p.ambiguities) {
      logf(" |OR| ");
      xprint_paren(pp, p.ambiguities);
    }
  }
}

auto D_ParseNode_to_PNode(D_ParseNode* _apn)
{
    return (cast(PNode*)(D_PN(_apn, -cast(intptr_t)&(cast(PNode*)null).parse_node)));
}

auto PNode_to_D_ParseNode(PNode* _apn)
{
    return (cast(D_ParseNode*)&_apn.parse_node);
}

D_ParseNode *
d_get_child(D_ParseNode *apn, int child) {
  PNode *pn = D_ParseNode_to_PNode(apn);
  if (child < 0 || child >= pn.children.n)
    return null;
  return &pn.children.v[child].parse_node;
}

int
d_get_number_of_children(D_ParseNode *apn) {
  PNode *pn = D_ParseNode_to_PNode(apn);
  return pn.children.n;
}

D_ParseNode *
d_find_in_tree(D_ParseNode *apn, int symbol) {
  PNode *pn = D_ParseNode_to_PNode(apn);
  D_ParseNode *res;
  int i;

  if (pn.parse_node.symbol == symbol)
    return apn;
  for (i = 0; i < pn.children.n; i++)
  {
    res = d_find_in_tree(&pn.children.v[i].parse_node, symbol);
    if (res)
      return res;
  }
  return null;
}

char *
d_ws_before(D_Parser *ap, D_ParseNode *apn) {
  PNode *pn = D_ParseNode_to_PNode(apn);
  return pn.ws_before;
}

char *
d_ws_after(D_Parser *ap, D_ParseNode *apn) {
  PNode *pn = D_ParseNode_to_PNode(apn);
  return pn.ws_after;
}

uint SNODE_HASH(uint _s, D_Scope* _sc, void* _g)
{
    return  cast(uint)(((cast(uintptr_t)(_s)) << 12) + ((cast(uintptr_t)(_sc))) + (cast(uintptr_t)(_g)));
}

private SNode *
find_SNode(Parser *p, uint stateIndex, D_Scope *sc, void *g) {
  SNodeHash *ph = &p.snode_hash;
  SNode *sn;
  uint h = SNODE_HASH(stateIndex, sc, g);
  
  if (ph.v)
    for (sn = ph.v[h % ph.m]; sn; sn = sn.bucket_next)
      if (stateIndex == sn.stateIndex &&
          sn.initial_scope == sc &&
          sn.initial_globals == g)
        return sn;
  return null;
}

private void
insert_SNode_internal(Parser *p, SNode *sn) {
  SNodeHash *ph = &p.snode_hash;
  uint h = SNODE_HASH(sn.stateIndex, sn.initial_scope, sn.initial_globals), i;
  SNode *t;

  if (ph.n + 1 > ph.m) {
    SNode **v = ph.v;
    int m = ph.m;
    ph.i++;
    ph.m = d_prime2[ph.i];
    ph.v = cast(SNode**)MALLOC(ph.m * (*ph.v).sizeof);
    for (i = 0; i < m; i++)
    {
        t = v[i];
      while (t) {
        v[i] = v[i].bucket_next;
        insert_SNode_internal(p, t);
        t = v[i];
      }
    }
  }
  sn.bucket_next = ph.v[h % ph.m];
  assert(sn.bucket_next != sn);
  ph.v[h % ph.m] = sn;
  ph.n++;
}

private void
insert_SNode(Parser *p, SNode *sn) {
  insert_SNode_internal(p, sn);
  sn.all_next = p.snode_hash.all;
  p.snode_hash.all = sn;
}

private SNode *
new_SNode(Parser *p, uint stateIndex, d_loc_t *loc, D_Scope *sc, void *g) {
  SNode *sn = p.free_snodes;
  if (!sn)
    sn = cast(SNode*)MALLOC((*sn).sizeof);
  else
    p.free_snodes = sn.all_next;
  sn.depth = 0;
  vec_clear(&sn.zns);
  sn.all_next = null;
  p.states++;
  sn.stateIndex = stateIndex;
  sn.initial_scope = sc;
  sn.initial_globals = g;
  sn.last_pn = null;
  sn.loc = *loc;
  insert_SNode(p, sn);
  if (p.t.states[stateIndex].accept) {
    if (!p.accept) {
      p.accept = sn;
    } else if (sn.loc.s > p.accept.loc.s) {
      p.accept = sn;
    }
  }
  return sn;
}

private ZNode *
new_ZNode(Parser *p, PNode *pn) {
    auto z = new ZNode();
    z.pn = pn;
    vec_clear(&z.sns);
  return z;
}

private void
free_PNode(Parser *p, PNode *pn) {
  int i;
  if (p.free_node_fn)
    p.free_node_fn(&pn.parse_node);
  vec_free(&pn.children);
}

uint PNODE_HASH(char* _si, char* _ei, int _s, D_Scope* _sc, void* _g)
{
    return cast(uint)(((cast(uintptr_t)_si) << 8) + ((cast(uintptr_t)_ei) << 16) + ((cast(uintptr_t)_s)) + ((cast(uintptr_t)_sc)) + ((cast(uintptr_t)_g)));
}

private PNode *
find_PNode(Parser *p, char *start, char *end_skip, int symbol, D_Scope *sc, void *g, uint *hash) {
  PNodeHash *ph = &p.pnode_hash;
  PNode *pn;
  uint h = PNODE_HASH(start, end_skip, symbol, sc, g);
  *hash = h;
  if (ph.v)
    for (pn = ph.v[h % ph.m]; pn; pn = pn.bucket_next)
      if (pn.hash == h &&
          pn.parse_node.symbol == symbol &&
          pn.parse_node.start_loc.s == start &&
          pn.parse_node.end_skip == end_skip &&
          pn.initial_scope == sc &&
          pn.initial_globals == g) {
        LATEST(pn);
        return pn;
      }
  return null;
}

private void
insert_PNode_internal(Parser *p, PNode *pn) {
  PNodeHash *ph = &p.pnode_hash;
  uint h = PNODE_HASH(pn.parse_node.start_loc.s, pn.parse_node.end_skip,
                      pn.parse_node.symbol, pn.initial_scope, pn.initial_globals), i;

  if (ph.n + 1 > ph.m) {
    PNode **v = ph.v;
    int m = ph.m;
    ph.i++;
    ph.m = d_prime2[ph.i];
    ph.v = cast(PNode**)MALLOC(ph.m * (*ph.v).sizeof);
    for (i = 0; i < m; i++)
    {
      PNode *t = v[i];
      while (t) {
        v[i] = v[i].bucket_next;
        insert_PNode_internal(p, t);
        t = v[i];
      }
    }
  }
  pn.bucket_next = ph.v[h % ph.m];
  ph.v[h % ph.m] = pn;
  ph.n++;
}

private void
insert_PNode(Parser *p, PNode *pn) {
  insert_PNode_internal(p, pn);
  pn.all_next = p.pnode_hash.all;
  p.pnode_hash.all = pn;
}

private void
free_old_nodes(Parser *p) {
  uint h;
  PNode *pn = p.pnode_hash.all;
  PNode* tpn;
  SNode *sn = p.snode_hash.all;
  SNode*tsn;
  while (sn) {
    h = SNODE_HASH(sn.stateIndex, sn.initial_scope, sn.initial_globals);
    SNode** lsn = &p.snode_hash.v[h % p.snode_hash.m];
    tsn = sn; sn = sn.all_next;
    while (*lsn != tsn) lsn = &(*lsn).bucket_next;
    *lsn = (*lsn).bucket_next;
  }
  sn = p.snode_hash.last_all;
  p.snode_hash.last_all = null;
  while (sn) {
    tsn = sn; sn = sn.all_next;
  }
  p.snode_hash.last_all = p.snode_hash.all;
  p.snode_hash.all = null;
  while (pn) {
    foreach (ref c; pn.children) {
      while (c != c.latest) {
        tpn = c.latest;
        c = tpn;
      }
    }
    h = PNODE_HASH(pn.parse_node.start_loc.s, pn.parse_node.end_skip,
                   pn.parse_node.symbol, pn.initial_scope, pn.initial_globals);
    PNode** lpn = &p.pnode_hash.v[h % p.pnode_hash.m];
    tpn = pn; pn = pn.all_next;
    while (*lpn != tpn) lpn = &(*lpn).bucket_next;
    *lpn = (*lpn).bucket_next;
  }
  p.pnode_hash.n = 0;
  p.pnode_hash.all = null;
}

private void
alloc_parser_working_data(Parser *p) {
  p.pnode_hash.i = PNODE_HASH_INITIAL_SIZE_INDEX;
  p.pnode_hash.m = d_prime2[p.pnode_hash.i];
  p.pnode_hash.v =
    cast(PNode**)MALLOC(p.pnode_hash.m * (*p.pnode_hash.v).sizeof);
  p.snode_hash.i = SNODE_HASH_INITIAL_SIZE_INDEX;
  p.snode_hash.m = d_prime2[p.snode_hash.i];
  p.snode_hash.v =
    cast(SNode**)MALLOC(p.snode_hash.m * (*p.snode_hash.v).sizeof);
  p.shift_results = null;
  p.code_shifts = null;
}

private void
free_parser_working_data(Parser *p) {
  int i;

  free_old_nodes(p);
  free_old_nodes(p); /* to catch SNodes saved for error repair */
  memset(&p.pnode_hash, 0, (p.pnode_hash).sizeof);
  memset(&p.snode_hash, 0, (p.snode_hash).sizeof);
  while (p.reductions_todo) {
    Reduction *r = p.free_reductions.next;
    FREE(p.free_reductions); p.free_reductions = r;
  }
  while (p.shifts_todo) {
    Shift *s = p.free_shifts.next;
    FREE(p.free_shifts); p.free_shifts = s;
  }
  while (p.free_reductions) {
    Reduction *r = p.free_reductions.next;
    FREE(p.free_reductions); p.free_reductions = r;
  }
  while (p.free_shifts) {
    Shift *s = p.free_shifts.next;
    FREE(p.free_shifts); p.free_shifts = s;
  }
  while (p.free_pnodes) {
    PNode *pn = p.free_pnodes.all_next;
    FREE(p.free_pnodes); p.free_pnodes = pn;
  }
  while (p.free_znodes) {
    ZNode *zn = znode_next(p.free_znodes);
    FREE(p.free_znodes); p.free_znodes = zn;
  }
  while (p.free_snodes) {
    SNode *sn = p.free_snodes.all_next;
    FREE(p.free_snodes); p.free_snodes = sn;
  }
  vec_free(&p.error_reductions);
  if (p.whitespace_parser)
    free_parser_working_data(p.whitespace_parser);
  p.shift_results = null;
  p.code_shifts = null;
}

private int
znode_depth(ZNode *z) {
  int i, d = 0;
  if (!z)
    return int.max;
  for (i = 0; i < z.sns.n; i++)
    d = d < z.sns.v[i].depth ? z.sns.v[i].depth : d;
  return d;
}

private Reduction *
add_Reduction(Parser *p, ZNode *z, SNode *sn, D_Reduction *reduction) {
  Reduction **l = &p.reductions_todo;
  int d = znode_depth(z), dd;
  for (Reduction* x = p.reductions_todo; x; l = &x.next, x = x.next) {
    if (sn.loc.s < x.snode.loc.s)
      break;
    dd = znode_depth(x.znode);
    if ((sn.loc.s == x.snode.loc.s && d >= dd)) {
      if (d == dd)
        while (x) {
          if (sn == x.snode && z == x.znode && reduction == x.reduction)
            return null;
          x = x.next;
        }
      break;
    }
  }
  {
    Reduction *r = p.free_reductions;
    if (!r)
      r = cast(Reduction*)MALLOC((*r).sizeof);
    else
      p.free_reductions = r.next;
    r.znode = z;
    r.snode = sn;
    r.new_snode = null;
    r.reduction = reduction;
    r.next = *l;
    *l = r;
    return r;
  }
}

private void
add_Shift(Parser *p, SNode *snode) {
  Shift **l = &p.shifts_todo;
  Shift *s = p.free_shifts;
  if (!s)
    s = cast(Shift*)MALLOC((*s).sizeof);
  else
    p.free_shifts = s.next;
  s.snode = snode;
  for (Shift* x = p.shifts_todo; x; l = &x.next, x = x.next)
    if (snode.loc.s <= x.snode.loc.s) break;
  s.next = *l;
  *l = s;
}

private SNode *
add_SNode(Parser *p, uint stateIndex, d_loc_t *loc, D_Scope *sc, void *g) {
  SNode *sn = find_SNode(p, stateIndex, sc, g);
  if (sn)
    return sn;
  sn = new_SNode(p, stateIndex, loc, sc, g);
  auto state = &p.t.states[stateIndex];
  if (state.shifts)
    add_Shift(p, sn);
  foreach(r; state.reductions)
    if (!r.nelements)
      add_Reduction(p, null, sn, r);
  return sn;
}

private int
reduce_actions(Parser *p, PNode *pn, D_Reduction *r) {
  int height = 0;

  foreach (c; pn.children) {
    if (c.op_assoc) {
      pn.assoc = c.op_assoc;
      pn.priority = c.op_priority;
    }
    if (c.height >= height)
      height = c.height + 1;
  }

  pn.op_assoc = cast(AssocKind)r.op_assoc;
  pn.op_priority = r.op_priority;
  pn.height = height;
  if (r.rule_assoc) {
    pn.assoc = cast(AssocKind)r.rule_assoc;
    pn.priority = r.rule_priority;
  }
  if (r.speculative_code)
    return r.speculative_code(
      pn, cast(void**)&pn.children.v[0], cast(int)pn.children.n,
      cast(int)&(cast(PNode*)null).parse_node, p);
  return 0;
}

enum x  = 666; /* impossible */
private int child_table[4][3][6] = [
[
/* binary parent, child on left */
  /* priority of child vs parent, or = with child|parent associativity
     > < =LL =LR =RL =RR
   */
  [ 1, 0, 1, 1, 0, 0], /* binary child */
  [ 1, 1, 1, 1, x, x], /* left unary child */
  [ 1, 0, x, x, 1, 1]  /* right unary child */
],
[ /* binary parent, child on right */
  [ 1, 0, 0, 0, 1, 1], /* binary child */
  [ 1, 0, 1, 1, x, x], /* left unary child */
  [ 1, 1, x, x, 1, 1]  /* right unary child */
],
[ /* left unary parent */
  [ 1, 0, 0, x, 0, x], /* binary child */
  [ 1, 1, 1, x, x, x], /* left unary child */
  [ 1, 0, x, x, 1, x]  /* right unary child */
],
[ /* right unary parent */
  [ 1, 0, x, 0, x, 0], /* binary child */
  [ 1, 0, x, 1, x, x], /* left unary child */
  [ 1, 1, x, x, x, 1]  /* right unary child */
]
];

/* returns 1 if legal for child reduction and illegal for child shift */
private int
check_child(int ppri, AssocKind passoc, int cpri, AssocKind cassoc,
            int left, int right)
{
  int p = IS_BINARY_NARY_ASSOC(passoc) ? (right ? 1 : 0) :
          (passoc == AssocKind.ASSOC_UNARY_LEFT ? 2 : 3);
  int c = IS_BINARY_NARY_ASSOC(cassoc) ? 0 :
          (cassoc == AssocKind.ASSOC_UNARY_LEFT ? 1 : 2);
  int r =
    cpri > ppri ? 0 : (
      cpri < ppri ? 1 : ( 2 + (
        (IS_RIGHT_ASSOC(cassoc) ? 2 : 0) +
        (IS_RIGHT_ASSOC(passoc) ? 1 : 0))));
  return child_table[p][c][r];
}

/* check assoc/priority legality, 0 is OK, -1 is bad */
private int
check_assoc_priority(PNode *pn0, PNode *pn1, PNode *pn2) {
  if (!IS_UNARY_BINARY_ASSOC(pn0.op_assoc)) {
    if (IS_UNARY_BINARY_ASSOC(pn1.op_assoc)) { /* second token is operator */
      /* check expression pn0 (child of pn1) */
      if (pn0.assoc) {
        if (!check_child(pn1.op_priority, pn1.op_assoc,
                         pn0.priority, pn0.assoc, 0, 1))
        return -1;
      }
    }
  } else { /* pn0 is an operator */
    if (pn1.op_assoc) {
      /* check pn0 (child of operator pn1) */
      if (!check_child(pn1.op_priority, pn1.op_assoc,
                       pn0.op_priority, pn0.op_assoc, 0, 1))
        return -1;
    } else if (pn2) {
      /* check pn0 (child of operator pn2) */
      if (pn2.op_assoc &&
          !check_child(pn2.op_priority, pn2.op_assoc,
                       pn0.op_priority, pn0.op_assoc, 0, 1))
        return -1;
    }
    /* check expression pn1 (child of pn0)  */
    if (pn1.assoc) {
      if (!check_child(pn0.op_priority, pn0.op_assoc,
                       pn1.priority, pn1.assoc, 1, 0))
        return -1;
    }
  }
  return 0;
}

/* check to see if a path is legal with respect to
   the associativity and priority of its operators */
private int
check_path_priorities_internal(VecZNode *path) {
  bool one = false;

  if (path.length == 0) return 0;

  int i = 0;
  PNode *pn0 = path.v[i].pn;
  if (!pn0.op_assoc) { /* deal with top expression directly */
    i = 1;
    if (path.n < i + 1)
      return 0;
    PNode *pn1 = path.v[i].pn;
    if (!pn1.op_assoc)
      return 0;
    if (pn0.assoc) {
      if (!check_child(pn1.op_priority, pn1.op_assoc,
                       pn0.priority, pn0.assoc, 0, 1))
        return -1;
    }
    pn0 = pn1;
  }
  if (path.n > i + 1) { /* entirely in the path */
    PNode *pn1 = path.v[i + 1].pn;
    if (path.n > i + 2)
      return check_assoc_priority(pn0, pn1, path.v[i + 2].pn);
    else { /* one level from the stack beyond the path */
      foreach (k; (*path)[i + 1].sns)
        foreach (zz; k.zns) {
          one = true;
          if (zz && !check_assoc_priority(pn0, pn1, zz.pn))
            return 0;
        }
      if (!one)
        return check_assoc_priority(pn0, pn1, null);
    }
  } else { /* two levels from the stack beyond the path */
    foreach (k; (*path)[i].sns)
      foreach (zz; k.zns) {
        if (zz)
          foreach (kk; zz.sns)
            foreach (zzz; kk.zns) {
              if (zzz && !check_assoc_priority(pn0, zz.pn, zzz.pn))
                return 0;
            }
      }
    return 0;
  }
  return -1;
}

/* avoid cases without operator priorities */
bool check_path_priorities(VecZNode * _p)
{
    return (_p.n > 1                  && 
   (_p.v[0].pn.op_assoc || _p.v[1].pn.op_assoc)       && 
   check_path_priorities_internal(_p));
}

private int
compare_priorities(int* xpri, int xn, int* ypri, int yn) {
  int i = 0;

  while (i < xn && i < yn) {
    if (xpri[i] > ypri[i])
      return -1;
    if (xpri[i] < ypri[i])
      return 1;
    i++;
  }
  return 0;
}

private void
intreverse(int *xp, int n) {
  int *a = xp, b = xp + n -1;
  while (a < b) {
    int t = *a;
    *a = *b;
    *b = t;
    a++; b--;
  }
}

/* sort by deepest, then by location */
private void
priority_insert(StackPNode *psx, PNode *x) {
  PNode *t;
  PNode** start, cur;

  stack_push(psx, x);
  start = psx.start;
  cur = psx.cur;
  for (;cur > start + 1;cur--) {
    if (cur[-1].height > cur[-2].height)
      continue;
    if (cur[-1].height == cur[-2].height &&
        cur[-1].parse_node.start_loc.s > cur[-2].parse_node.start_loc.s)
      continue;
    t = cur[-1];
    cur[-1] = cur[-2];
    cur[-2] = t;
  }
}

private void
get_exp_all(Parser *p, PNode *x, StackInt *psx) {
  if (x.assoc)
    stack_push(psx, x.priority);
  foreach (pn; x.children) {
    LATEST(pn);
    get_exp_all(p, pn, psx);
  }
}

private void
get_exp_one(Parser *p, PNode *x, StackPNode *psx, StackInt *isx) {
  LATEST(x);
  if (!IS_NARY_ASSOC(x.assoc))
    priority_insert(psx, x);
  else {
    stack_push(isx, x.priority);
    foreach (i; x.children)
      if (i.assoc)
        get_exp_one(p, i, psx, isx);
  }
}

private void
get_exp_one_down(Parser *p, PNode *x, StackPNode *psx, StackInt *isx) {
  LATEST(x);
  stack_push(isx, x.priority);
  foreach (i; x.children)
    if (i.assoc)
      get_exp_one(p, i, psx, isx);
}

/* get the set of priorities for unshared nodes,
   eliminating shared subtrees via priority queues */
private void
get_unshared_priorities(Parser *p, StackPNode *psx, StackPNode *psy,
                        StackInt *isx, StackInt *isy)
{
  StackPNode *psr;
  while (1) {
    if (is_stack_empty(psx)) {
      psr = psy;
      break;
    } else if (is_stack_empty(psy)) {
      psr = psx;
      break;
    }
    if (stack_head(psx).height > stack_head(psy).height)
      psr = psx;
    else if (stack_head(psx).height < stack_head(psy).height)
      psr = psy;
    else if (stack_head(psx) > stack_head(psy))
      psr = psx;
    else if (stack_head(psx) < stack_head(psy))
      psr = psy;
    else {
      stack_pop(psx);
      stack_pop(psy);
      continue;
    }
    PNode *t = stack_pop(psr);
    if (psr == psx)
      get_exp_one_down(p, t, psx, isx);
    else
      get_exp_one_down(p, t, psy, isy);
  }
  while (!is_stack_empty(psr)) {
    PNode *t = stack_pop(psr);
    if (psr == psx)
      get_exp_all(p, t, isx);
    else
      get_exp_all(p, t, isy);
  }
  return;
}

/* compare the priorities of operators in two trees
   while eliminating common subtrees for efficiency.
*/
private int
cmp_priorities(Parser *p, PNode *x, PNode *y) {
  StackPNode psx, psy;
  StackInt isx, isy;

  stack_clear(&psx); stack_clear(&psy); stack_clear(&isx); stack_clear(&isy);
  get_exp_one(p, x, &psx, &isx);
  get_exp_one(p, y, &psy, &isy);
  get_unshared_priorities(p, &psx, &psy, &isx, &isy);
  intreverse(isx.start, stack_depth(&isx));
  intreverse(isy.start, stack_depth(&isy));
  int r = compare_priorities(isx.start, stack_depth(&isx),
                     isy.start, stack_depth(&isy));
  stack_free(&psx); stack_free(&psy); stack_free(&isx); stack_free(&isy);
  return r;
}

private void
get_all(Parser *p, PNode *x, VecPNode *vx) {
  int i;
  if (set_add(vx, x)) {
    foreach (pn; x.children) {
      LATEST(pn);
      get_all(p, pn, vx);
    }
  }
}

private void
get_unshared_pnodes(Parser *p, PNode *x, PNode *y, ref PNode*[] pvx, ref PNode*[] pvy) {
  VecPNode vx, vy;
  vec_clear(&vx); vec_clear(&vy);
  LATEST(x); LATEST(y);
  get_all(p, x, &vx);
  get_all(p, y, &vy);
  foreach (i; vx)
    if (i && !set_find(&vy, i))
      pvx ~= i;
  foreach (i; vy)
    if (i && !set_find(&vx, i))
      pvy ~= i;
}

int
greedycmp(PNode* x, PNode* y) {
  // first by start
  if (x.parse_node.start_loc.s < y.parse_node.start_loc.s)
    return -1;
  if (x.parse_node.start_loc.s > y.parse_node.start_loc.s)
    return 1;
  // second by symbol
  if (x.parse_node.symbol < y.parse_node.symbol)
    return -1;
  if (x.parse_node.symbol > y.parse_node.symbol)
    return 1;
  // third by length
  if (x.parse_node.end < y.parse_node.end)
    return -1;
  if (x.parse_node.end > y.parse_node.end)
    return 1;
  return 0;
}

bool PNodeIsLessGreedyThanPNode(PNode* x, PNode* y)
{
    return greedycmp(x, y) < 0;
}

private int
cmp_greediness(Parser *p, PNode *x, PNode *y) {
  PNode*[] pvx, pvy;
  get_unshared_pnodes(p, x, y, pvx, pvy);
  /* set_to_vec(&pvx); set_to_vec(&pvy); */
  pvx.sort!(PNodeIsLessGreedyThanPNode)();
  pvy.sort!(PNodeIsLessGreedyThanPNode)();

  int ix = 0, iy = 0;
  while (1) {
      if (pvx.length <= ix || pvy.length <= iy)
          return(0);
      x = pvx[ix]; y = pvy[iy];
      if (x == y) {
          ix++;
          iy++;
      } else if (x.parse_node.start_loc.s < y.parse_node.start_loc.s)
          ix++;
      else if (x.parse_node.start_loc.s > y.parse_node.start_loc.s)
          iy++;
      else if (x.parse_node.symbol < y.parse_node.symbol)
          ix++;
      else if (x.parse_node.symbol > y.parse_node.symbol)
          iy++;
      else if (x.parse_node.end > y.parse_node.end)
          return(-1);
      else if (x.parse_node.end < y.parse_node.end)
          return(1);
      else if (x.children.n < y.children.n)
          return(-1);
      else if (x.children.n > y.children.n)
          return(1);
      else {
          ix++;
          iy++;
      }
  }
}

int
resolve_amb_greedy(D_Parser *dp, int n, D_ParseNode **v) {
  int i, result, selected_node = 0;

  for(i=1; i<n; i++) {
     result = cmp_greediness(dp, D_ParseNode_to_PNode(v[i]),D_ParseNode_to_PNode( v[selected_node]) );
     if ( result < 0 ||
         (result == 0 && D_ParseNode_to_PNode(v[i]).height < D_ParseNode_to_PNode(v[selected_node]).height) )
        selected_node = i;
  }
  return selected_node;
}

/* return -1 for x, 1 for y and 0 if they are ambiguous */
private int
cmp_pnodes(Parser *p, PNode *x, PNode *y) {
  int r = 0;
  if (x.assoc && y.assoc) {
    r = cmp_priorities(p, x, y);
    if (r)
      return r;
  }
  if (!p.dont_use_greediness_for_disambiguation)
  {
    r = cmp_greediness(p, x, y);
    if (r)
      return r;
  }
  if (!p.dont_use_height_for_disambiguation) {
    if (x.height < y.height)
      return -1;
    if (x.height > y.height)
      return 1;
  }
  return r;
}

private PNode *
make_PNode(Parser *p, uint hash, int symbol, d_loc_t *start_loc, char *e, PNode *pn,
           D_Reduction *r, VecZNode *path, const D_Shift *sh, D_Scope *scope_)
{
  int l = cast(int)((PNode).sizeof - (d_voidp).sizeof + p.sizeof_user_parse_node);
  PNode *new_pn = p.free_pnodes;
  if (!new_pn)
    new_pn = cast(PNode*)MALLOC(l);
  else
    p.free_pnodes = new_pn.all_next;
  p.pnodes++;
  memset(new_pn, 0, l);
  new_pn.hash = hash;
  new_pn.parse_node.symbol = symbol;
  new_pn.parse_node.start_loc = *start_loc;
  new_pn.ws_before = start_loc.ws;
  if (!r || !path) /* end of last parse node of path for non-epsilon reductions */
    new_pn.parse_node.end = e;
  else
    new_pn.parse_node.end = pn.parse_node.end;
  new_pn.parse_node.end_skip = e;
  new_pn.shift = sh;
  new_pn.reduction = r;
  new_pn.parse_node.scope_ = pn.parse_node.scope_;
  new_pn.initial_scope = scope_;
  new_pn.parse_node.globals = pn.parse_node.globals;
  new_pn.initial_globals = new_pn.parse_node.globals;
  new_pn.parse_node.white_space = pn.parse_node.white_space;
  new_pn.latest = new_pn;
  new_pn.ws_after = e;
  if (sh) {
    new_pn.op_assoc = cast(AssocKind)sh.op_assoc;
    new_pn.op_priority = sh.op_priority;
    if (sh.speculative_code && sh.action_index != -1) {
      D_Reduction dummy;
      dummy.action_index = sh.action_index;
      new_pn.reduction = &dummy;
      if (sh.speculative_code(
        new_pn, cast(void**)&new_pn.children.v[0], new_pn.children.n,
        cast(int)&(cast(PNode*)(null)).parse_node, p))
      {
        free_PNode(p, new_pn);
        return null;
      }
      new_pn.reduction = null;
    }
  } else if (r) {
    if (path)
      foreach_reverse (i; *path) {
        PNode *latest = i.pn;
        LATEST(latest);
        vec_add(&new_pn.children, latest);
      }
    if (reduce_actions(p, new_pn, r)) {
      free_PNode(p, new_pn);
      return null;
    }
    if (path && path.n > 1) {
      int n = path.n;
      for (int i = 0; i < n; i += n-1) {
        PNode *child = new_pn.children[i];
        if (child.assoc && new_pn.assoc &&
            !check_child(new_pn.priority, new_pn.assoc,
                         child.priority, child.assoc, i == 0, i == n - 1))
        {
          free_PNode(p, new_pn);
          return null;
        }
      }
    }
  }
  return new_pn;
}

private int
PNode_equal(Parser *p, PNode *pn, D_Reduction *r, VecZNode *path, const D_Shift* sh) {
  int i, n = pn.children.n;
  if (sh)
    return sh == pn.shift;
  if (r != pn.reduction)
    return 0;
  if (!path && !n)
    return 1;
  if (n == path.n) {
    for (i = 0; i < n; i++) {
      PNode *x = pn.children[i], y = (*path)[n - i - 1].pn;
      LATEST(x);
      LATEST(y);
      if (x != y)
        return 0;
    }
    return 1;
  }
  return 0;
}

/* find/create PNode */
private PNode *
add_PNode(Parser *p, int symbol, d_loc_t *start_loc, char *e, PNode *pn,
          D_Reduction *r, VecZNode *path, const D_Shift* sh)
{
  D_Scope *scope_ = equiv_D_Scope(pn.parse_node.scope_);
  uint hash;
  PNode *old_pn = find_PNode(p, start_loc.s, e, symbol, scope_, pn.parse_node.globals, &hash);
  if (old_pn) {
    if (PNode_equal(p, old_pn, r, path, sh))
      return old_pn;
    for (PNode *amb = old_pn.ambiguities; amb; amb = amb.ambiguities) {
      if (PNode_equal(p, amb, r, path, sh))
        return old_pn;
    }
  }
  PNode *new_pn = make_PNode(p, hash, symbol, start_loc, e, pn, r, path, sh, scope_);
  if (!old_pn) {
    old_pn = new_pn;
    if (!new_pn)
      return null;
    insert_PNode(p, new_pn);
    return old_pn;
  }
  if (!new_pn)
    return old_pn;
  p.compares++;
  switch (cmp_pnodes(p, new_pn, old_pn)) {
    case 0:
      new_pn.ambiguities = old_pn.ambiguities;
      old_pn.ambiguities = new_pn;
      break;
    case -1:
      insert_PNode(p, new_pn);
      LATEST(old_pn);
      old_pn.latest = new_pn;
      old_pn = new_pn;
      break;
    case 1:
      free_PNode(p, new_pn);
      break;
    default:
  }
  return old_pn;
}

/* The set of znodes associated with a state can be very large
   because of cascade reductions (for example, large expression trees).
   Use an adaptive data structure starting with a short list and
   then falling back to a direct map hash table.
*/

private void
set_union_znode(VecZNode *v, VecZNode *vv) {
  foreach(z; *vv)
    if (z)
      set_add_znode(v, z);
}

private ZNode *
set_find_znode(VecZNode *v, PNode *pn) {
  uint i, j, n = v.n, h;
  if (n <= INTEGRAL_VEC_SIZE) {
    for (i = 0; i < n; i++)
      if (v.v[i].pn == pn)
        return v.v[i];
    return null;
  }
  h = (cast(uintptr_t)pn) % n;
  for (i = h, j = 0;
       i < v.n && j < SET_MAX_SEQUENTIAL;
       i = ((i + 1) % n), j++)
  {
    if (!v.v[i])
      return null;
    else if (v.v[i].pn == pn)
      return v.v[i];
  }
  return null;
}

private void
set_add_znode_hash(VecZNode *v, ZNode *z) {
  VecZNode vv;
  vec_clear(&vv);
  int i, j, n = v.n;
  if (n) {
    uint h = cast(uint)((cast(uintptr_t)z.pn) % n);
    for (i = h, j = 0;
         i < v.n && j < SET_MAX_SEQUENTIAL;
         i = ((i + 1) % n), j++)
    {
      if (!v.v[i]) {
        v.v[i] = z;
        return;
      }
    }
  }
  if (!n) {
    vv.v = null;
    v.i = INITIAL_SET_SIZE_INDEX;
  } else {
    vv.v = cast(ZNode**)v.v;
    vv.n = v.n;
    v.i = v.i + 2;
  }
  v.n = d_prime2[v.i];
  v.v = cast(ZNode**)MALLOC(v.n * (void *).sizeof);
  if (vv.v) {
    set_union_znode(v, &vv);
  }
  set_add_znode(v, z);
}

private void
set_add_znode(VecZNode *v, ZNode *z) {
  VecZNode vv;
  vec_clear(&vv);
  int i, n = v.n;
  if (n < INTEGRAL_VEC_SIZE) {
    vec_add(v, z);
    return;
  }
  if (n == INTEGRAL_VEC_SIZE) {
    vv = *v;
    vec_clear(v);
    for (i = 0; i < n; i++)
      set_add_znode_hash(v, vv.v[i]);
  }
  set_add_znode_hash(v, z);
}

private int GOTO_STATE(Parser* _p, PNode* _pn, SNode* _ps)
{
    auto offset = _p.t.states[_ps.stateIndex].goto_table_offset;
    return _p.t.goto_table[_pn.parse_node.symbol - offset] - 1;
}

private SNode *
goto_PNode(Parser *p, d_loc_t *loc, PNode *pn, SNode *ps) {
  int i;

  if (!IS_BIT_SET(p.t.states[ps.stateIndex].goto_valid, pn.parse_node.symbol))
    return null;
  int state_index = GOTO_STATE(p, pn, ps);
  D_State *state = &p.t.states[state_index];
  SNode *new_ps = add_SNode(p, state_index, loc, pn.parse_node.scope_, pn.parse_node.globals);
  new_ps.last_pn = pn;

  debug(trace) logf("goto %d (%s) . %d %X\n",
             ps.stateIndex,
             p.t.symbols[pn.parse_node.symbol].name,
             state_index, new_ps);
  if (ps != new_ps && new_ps.depth < ps.depth + 1)
    new_ps.depth = ps.depth + 1;
  /* find/create ZNode */
  ZNode *z = set_find_znode(&new_ps.zns, pn);
  if (!z) { /* not found */
    set_add_znode(&new_ps.zns, (z = new_ZNode(p, pn)));
    foreach(r; state.reductions)
      if (r.nelements)
        add_Reduction(p, z, new_ps, r);
    if (!pn.shift)
      foreach(h; state.right_epsilon_hints) {
        SNode *pre_ps = find_SNode(p, h.preceeding_state, new_ps.initial_scope, new_ps.initial_globals);
        if (!pre_ps) continue;
        foreach (k; pre_ps.zns)
          if (k) {
            Reduction *r = add_Reduction(p, k, pre_ps, h.reduction);
            if (r) {
              r.new_snode = new_ps;
              r.new_depth = h.depth;
            }
          }
      }
  }
  for (i = 0; i < z.sns.n; i++)
    if (z.sns.v[i] == ps) break;
  if (i >= z.sns.n) { /* not found */
    vec_add(&z.sns, ps);
  }
  return new_ps;
}

void
parse_whitespace(D_Parser *ap, d_loc_t *loc, void **p_globals) {
  Parser *pp = ap.whitespace_parser;
  pp.start = loc.s;
  if (!exhaustive_parse(pp, ap.t.whitespace_state)) {
    if (pp.accept) {
      int old_col = loc.col, old_line = loc.line;
      *loc = pp.accept.loc;
      if (loc.line == 1)
        loc.col = old_col + loc.col;
      loc.line = old_line + (pp.accept.loc.line - 1);
      pp.accept = null;
    }
  }
}

private void
shift_all(Parser *p, const char *pos) {
    int i, j, nshifts = 0, ncode = 0;
    d_loc_t loc, skip_loc;
    D_WhiteSpaceFn skip_fn = null;
    ShiftResult *r;
    Shift *saved_s = p.shifts_todo, s = saved_s, ss;

    loc = s.snode.loc;
    skip_loc.s = null;

    s = p.shifts_todo;
    while (s && s.snode.loc.s == pos) {
        if (p.shift_results.length - nshifts < p.t.symbols.length * 2) {
            p.shift_results.length = nshifts + p.t.symbols.length * 3;
        }
        p.shifts_todo = p.shifts_todo.next;
        p.scans++;
        D_State *state = &p.t.states[s.snode.stateIndex];
        if (state.scanner_code) {
            if (p.code_shifts.length < ncode + 1) {
                p.code_shifts.length = ncode + 2;
            }
            p.code_shifts[ncode].shift_kind = D_SCAN_ALL;
            p.code_shifts[ncode].term_priority = 0;
            p.code_shifts[ncode].op_assoc = 0;
            p.code_shifts[ncode].action_index = 0;
            p.code_shifts[ncode].speculative_code = null;
            p.shift_results[nshifts].loc = loc;
            if ((state.scanner_code(
                            &p.shift_results[nshifts].loc,
                            &p.code_shifts[ncode].symbol, &p.code_shifts[ncode].term_priority,
                            &p.code_shifts[ncode].op_assoc, &p.code_shifts[ncode].op_priority)))
            {
                p.shift_results[nshifts].snode = s.snode;
                p.shift_results[nshifts++].shift = &p.code_shifts[ncode++];
            }
        }
        if (state.scanner_table) {
            int n = scan_buffer(loc, *state, p.shift_results[nshifts .. $]);
            for (i = 0; i < n; i++)
                p.shift_results[nshifts + i].snode = s.snode;
            nshifts += n;
        }
        s = p.shifts_todo;
    }
    for (i = 0; i < nshifts; i++) {
        r = &p.shift_results[i];
        if (!r.shift)
            continue;
        if (r.shift.shift_kind == D_SCAN_TRAILING) {
            int symbol = r.shift.symbol;
            SNode *sn = r.snode;
            r.shift = null;
            for (j = i + 1; j < nshifts; j++) {
                if (p.shift_results[j].shift &&
                        sn == p.shift_results[j].snode &&
                        symbol == p.shift_results[j].shift.symbol) {
                    r.shift = p.shift_results[j].shift;
                    p.shift_results[j].shift = null;
                }
            }
        }
        if (r.shift && r.shift.term_priority) {
            /* potentially n^2 but typically small */
            for (j = 0; j < nshifts; j++) {
                if (!p.shift_results[j].shift)
                    continue;
                if (r.loc.s == p.shift_results[j].loc.s && j != i) {
                    if (r.shift.term_priority < p.shift_results[j].shift.term_priority) {
                        r.shift = null;
                        break;
                    }
                    if (r.shift.term_priority > p.shift_results[j].shift.term_priority)
                        p.shift_results[j].shift = null;
                }
            }
        }
    }
    for (i = 0; i < nshifts; i++) {
        r = &p.shift_results[i];
        if (!r.shift)
            continue;
        p.shifts++;
        debug(trace) logf("shift %d %X %d (%s)\n",
                r.snode.stateIndex, r.snode, r.shift.symbol,
                p.t.symbols[r.shift.symbol].name);
        PNode *new_pn = add_PNode(p, r.shift.symbol, &r.snode.loc, r.loc.s,
                r.snode.last_pn, null, null, r.shift);
        if (new_pn) {
            if (!skip_loc.s || skip_loc.s != r.loc.s || skip_fn != new_pn.parse_node.white_space) {
                skip_loc = r.loc;
                skip_fn = new_pn.parse_node.white_space;
                new_pn.parse_node.white_space(
                        p, &skip_loc, cast(void**)&new_pn.parse_node.globals);
                skip_loc.ws = r.loc.s;
                new_pn.ws_before = loc.ws;
                new_pn.ws_after = skip_loc.s;
            }
            goto_PNode(p, &skip_loc, new_pn, r.snode);
        }
    }
    for (s = saved_s; s && s.snode.loc.s == pos;) {
        ss = s;
        s = s.next;
        ss.next = p.free_shifts;
        p.free_shifts = ss;
    }
}

private VecZNode path1; /* static first path for speed */

private VecZNode *
new_VecZNode(VecVecZNode *paths, int n, int parent) {
  int i;
  VecZNode *pv;

  if (!paths.n)
    pv = &path1;
  else
    pv = cast(VecZNode*)MALLOC((*pv).sizeof);
  vec_clear(pv);
  if (parent >= 0)
    for (i = 0; i < n; i++)
      vec_add(pv,  paths.v[parent].v[i]);
  return pv;
}

private void
build_paths_internal(ZNode *z, VecVecZNode *paths, int parent,
                     int n, int n_to_go)
{
  int j, k, l;

  vec_add(paths.v[parent], z);
  if (n_to_go <= 1)
    return;
  for (k = 0; k < z.sns.n; k++)
    for (j = 0, l = 0; j < z.sns.v[k].zns.n; j++) {
      if (z.sns.v[k].zns.v[j]) {
        if (k + l) {
          vec_add(paths, new_VecZNode(paths, n - (n_to_go - 1), parent));
          parent = paths.n - 1;
        }
        build_paths_internal(z.sns.v[k].zns.v[j], paths, parent,
                             n, n_to_go - 1);
        l++;
      }
    }
}

private void
build_paths(ZNode *z, VecVecZNode *paths, int nchildren_to_go) {
  if (!nchildren_to_go)
    return;
  vec_add(paths, new_VecZNode(paths, 0, -1));
  build_paths_internal(z, paths, 0, nchildren_to_go, nchildren_to_go);
}

private void
reduce_one(Parser *p, Reduction *r) {
  SNode *sn = r.snode;
  PNode *pn;
  int j, n = r.reduction.nelements;

  if (!r.znode) { /* epsilon reduction */
    pn = add_PNode(p, r.reduction.symbol, &sn.loc,
                        sn.loc.s, sn.last_pn, r.reduction, null, null);
    if (pn)
      goto_PNode(p, &sn.loc, pn, sn);
  } else {
    debug(trace) logf("reduce %d %X %d\n", r.snode.stateIndex, sn, n);
    VecVecZNode paths;
    vec_clear(&paths);
    build_paths(r.znode, &paths, n);
    foreach (path; paths) {
      if (r.new_snode) { /* prune paths by new right epsilon node */
        for (j = 0; j < path.v[r.new_depth].sns.n; j++)
          if (path.v[r.new_depth].sns.v[j] == r.new_snode)
            break;
        if (j >= path.v[r.new_depth].sns.n)
          continue;
      }
      if (check_path_priorities(path))
        continue;
      p.reductions++;
      PNode *last_pn = path.v[0].pn;
      ZNode *first_z = path.v[n - 1];
      pn = add_PNode(p, r.reduction.symbol,
                     &first_z.pn.parse_node.start_loc,
                     sn.loc.s, last_pn, r.reduction, path, null);
      if (pn)
        foreach (j; first_z.sns)
          goto_PNode(p, &sn.loc, pn, j);
    }
  }
  r.next = p.free_reductions;
  p.free_reductions = r;
}

private int
VecSNode_equal(const ref VecSNode vsn1, const ref VecSNode vsn2) @nogc @safe nothrow pure {
  if (vsn1.n != vsn2.n)
    return 0;
  for (int i = 0; i < vsn1.n; i++) {
    int j;
    for (j = 0; j < vsn2.n; j++) {
      if (vsn1[i] == vsn2[j])
        break;
    }
    if (j >= vsn2.n)
      return 0;
  }
  return 1;
}

private ZNode *
binary_op_ZNode(SNode *sn) @nogc @safe nothrow pure {
  ZNode *z;
  if (sn.zns.n != 1)
    return null;
  z = sn.zns.v[0];
  if (z.pn.op_assoc == AssocKind.ASSOC_UNARY_RIGHT) {
    if (z.sns.n != 1)
      return null;
    sn = z.sns.v[0];
    if (sn.zns.n != 1)
      return null;
    z = sn.zns.v[0];
  }
  if (!IS_BINARY_ASSOC(z.pn.op_assoc))
    return null;
  return z;
}


debug(trace) private const char *spaces = "                                                                                                  ";
debug(trace) private void
print_stack(Parser *p, SNode *s, int indent) {
  logf("%d", s.stateIndex);
  indent += 2;
  foreach (i; s.zns) {
    if (!i)
      continue;
    if (s.zns.n > 1)
      logf("\n%s[", &spaces[99-indent]);
    logf("(%s:", p.t.symbols[i.pn.parse_node.symbol].name);
    print_paren(p, i.pn);
    logf(")");
    foreach (j; i.sns) {
      if (i.sns.n > 1)
        logf("\n%s[", &spaces[98-indent]);
      print_stack(p, j, indent);
      if (i.sns.n > 1)
        logf("]");
    }
    if (s.zns.n > 1)
      logf("]");
  }
}

/* compare two stacks with operators on top of identical substacks
   eliminating the stack with the lower priority binary operator
   - used to eliminate unnecessary stacks created by the
     (empty) application binary operator
*/
private void
cmp_stacks(Parser *p) {
  char *pos;
  Shift *a, b;
  Shift **al, bl;
  ZNode *az, bz;

  pos = p.shifts_todo.snode.loc.s;
  debug(trace) {
    int i = 0;
    for (al = &p.shifts_todo, a = *al; a && a.snode.loc.s == pos;
         al = &a.next, a = a.next)
  {
    if (++i < 2) logf("\n");
    print_stack(p, a.snode, 0);
    logf("\n");
  }}
  for (al = &p.shifts_todo, a = *al; a && a.snode.loc.s == pos;
       al = &a.next, a = a.next)
  {
    az = binary_op_ZNode(a.snode);
    if (!az)
      continue;
    for (bl = &a.next, b = a.next; b && b.snode.loc.s == pos;
         bl = &b.next, b = b.next) {
      bz = binary_op_ZNode(b.snode);
      if (!bz)
        continue;
      if (!VecSNode_equal(az.sns, bz.sns))
        continue;
      if ((p.t.states[a.snode.stateIndex].reduces_to != b.snode.stateIndex) &&
          (p.t.states[b.snode.stateIndex].reduces_to != a.snode.stateIndex))
        continue;
      if (az.pn.op_priority > bz.pn.op_priority) {
        debug(trace){logf("DELETE ");
            print_stack(p, b.snode, 0);
            logf("\n");}
        *bl = b.next;
        b = *bl;
        break;
      }
      if (az.pn.op_priority < bz.pn.op_priority) {
        debug(trace){logf("DELETE ");
            print_stack(p, a.snode, 0);
            logf("\n");}
        *al = a.next;
        a = *al;
        goto Lbreak2;
      }
    }
  Lbreak2:;
  }
}

private void
free_ParseTreeBelow(Parser *p, PNode *pn) {
  int i;
  PNode *amb;

  vec_free(&pn.children);
    amb = pn.ambiguities;
  if (amb) {
    pn.ambiguities = null;
    free_PNode(p, amb);
  }
}

void
free_D_ParseTreeBelow(D_Parser *p, D_ParseNode *dpn) {
  free_ParseTreeBelow(p, DPN_TO_PN(dpn));
}

D_ParseNode *
ambiguity_count_fn(D_Parser *p, int n, D_ParseNode **v) {
  p.ambiguities += n - 1;
  return v[0];
}

D_ParseNode *
ambiguity_abort_fn(D_Parser *pp, int n, D_ParseNode **v) {
  int i;
  if (d_verbose_level) {
    for (i = 0; i < n; i++) {
      print_paren(pp, D_ParseNode_to_PNode(v[i]));
      logf("\n");
    }
  }
  d_fail("unresolved ambiguity line %d file %s", v[0].start_loc.line,
         v[0].start_loc.pathname);
  return v[0];
}

private int
final_actionless(PNode *pn) {
  int i;
  if (pn.reduction && pn.reduction.final_code)
    return 0;
  for (i = 0; i < pn.children.n; i++)
    if (!final_actionless(pn.children.v[i]))
      return 0;
  return 1;
}

private PNode *
resolve_ambiguities(Parser *p, PNode *pn) {
  PNode *amb;
  D_ParseNode *res;
  Vec!(D_ParseNode*) pns;

  vec_clear(&pns);
  bool efa = is_epsilon_PNode(pn) && final_actionless(pn);
  vec_add(&pns, &pn.parse_node);
  for (amb = pn.ambiguities; amb; amb = amb.ambiguities) {
    int i, found = 0;
    LATEST(amb);
    if (!p.dont_merge_epsilon_trees)
      if (efa && is_epsilon_PNode(amb) && final_actionless(amb))
        continue;
    for (i = 0; i < pns.n; i++)
      if (pns.v[i] == &amb.parse_node)
        found = 1;
    if (!found)
      vec_add(&pns, &amb.parse_node);
  }
  if (pns.n == 1) {
    res = pns.v[0];
    goto Ldone;
  }
  res = p.ambiguity_fn(cast(D_Parser *)p, pns.n, pns.v);
 Ldone:
  vec_free(&pns);
  return D_ParseNode_to_PNode(res);
}

private void
fixup_internal_symbol(Parser *p, PNode *pn, int ichild) {
  PNode *child = pn.children.v[ichild];
  int j, n, pnn;
  n = child.children.n, pnn = pn.children.n;
  if (pn == child)
    d_fail("circular parse: unable to fixup internal symbol");
  if (n == 0) {
    for (j = ichild; j < pnn - 1; j++)
      pn.children.v[j] = pn.children.v[j + 1];
    pn.children.n--;
  } else if (n == 1) {
    pn.children.v[ichild] = child.children.v[0];
  } else {
    for (j = 0; j < n - 1; j++) /* expand children vector */
      vec_add(&pn.children, null);
    for (j = pnn - 1; j >= ichild + 1; j--) /* move to new places */
      pn.children.v[j - 1 + n] = pn.children.v[j];
    for (j = 0; j < n; j++) {
      pn.children.v[ichild + j] = child.children.v[j];
    }
  }
}

bool is_symbol_internal_or_EBNF(Parser* _p, PNode* _pn)
{
    return (_p.t.symbols[_pn.parse_node.symbol].kind == D_SymbolKind.D_SYMBOL_INTERNAL ||
 _p.t.symbols[_pn.parse_node.symbol].kind == D_SymbolKind.D_SYMBOL_EBNF);
}

bool is_symbol_internal(Parser* _p, PNode* _pn)
{
    return _p.t.symbols[_pn.parse_node.symbol].kind == D_SymbolKind.D_SYMBOL_INTERNAL;
}

bool is_unreduced_epsilon_PNode(PNode* _pn)
{
    return is_epsilon_PNode(_pn) && (_pn.reduction && _pn.reduction.final_code);
}

private PNode *
commit_tree(Parser *p, PNode *pn) {
  int i, fixup_ebnf = 0, fixup = 0, internal = 0;
  LATEST(pn);
  if (pn.evaluated)
    return pn;
  if (!is_unreduced_epsilon_PNode(pn))
    pn.evaluated = 1;
  if (pn.ambiguities)
    pn = resolve_ambiguities(p, pn);
  fixup_ebnf = p.fixup_EBNF_productions;
  internal = is_symbol_internal_or_EBNF(p, pn);
  fixup = !p.dont_fixup_internal_productions && internal;
  for (i = 0; i < pn.children.n; i++) {
    PNode *tpn = commit_tree(p, pn.children.v[i]);
    if (tpn != pn.children.v[i]){
      pn.children.v[i] = tpn;
    }
    if (fixup &&
        (fixup_ebnf ? is_symbol_internal_or_EBNF(p, pn.children.v[i]) :
         is_symbol_internal(p, pn.children.v[i])))
    {
      fixup_internal_symbol(p, pn, i);
      i -= 1;
      continue;
    }
  }
  if (pn.reduction)
    debug(trace) logf("commit %X (%s)\n", pn, p.t.symbols[pn.parse_node.symbol].name);
  if (pn.reduction && pn.reduction.final_code)
    pn.reduction.final_code(
      pn, cast(void**)&pn.children.v[0], pn.children.n,
      cast(int)&(cast(PNode*)(null)).parse_node, p);
  if (pn.evaluated) {
    if (!p.save_parse_tree && !internal)
      free_ParseTreeBelow(p, pn);
  }
  return pn;
}

private int
commit_stack(Parser *p, SNode *sn) {
  int res = 0;

  if (sn.zns.n != 1)
    return -1;
  if (sn.zns.v[0].sns.n > 1)
    return -2;
  if (is_unreduced_epsilon_PNode(sn.zns.v[0].pn)) /* wait till reduced */
    return -3;
  if (sn.zns.v[0].sns.n)
    if ((res = commit_stack(p, sn.zns.v[0].sns.v[0])) < 0)
      return res;
  PNode *tpn = commit_tree(p, sn.zns.v[0].pn);
  if (tpn != sn.zns.v[0].pn){
      sn.zns.v[0].pn = tpn;
  }
  return res;
}

private const (char) *
find_substr(const (char) *str, const(char)[] s) {
  auto len = s.length;
  if (len == 1) {
    while (*str && *str != s[0]) str++;
    if (*str == s[0])
      return str + 1;
  } else
    while (*str) {
      if (!strncmp(s.ptr, str, len))
        return str + len;
      str++;
    }
  return null;
}

private void
syntax_error_report_fn(D_Parser *p) {
  char *_fn = d_dup_pathname_str(p.loc.pathname);
  const(char)[] fn = _fn[0 .. strlen(_fn)];
  const(char)[] after;
  ZNode *z = p.snode_hash.last_all ? p.snode_hash.last_all.zns.v[0] : null;
  while (z && z.pn.parse_node.start_loc.s == z.pn.parse_node.end)
    z = (z.sns.v && z.sns.v[0].zns.v) ? z.sns.v[0].zns.v[0] : null;
  if (z && z.pn.parse_node.start_loc.s != z.pn.parse_node.end)
    after = z.pn.parse_node.start_loc.s[0 .. z.pn.parse_node.end - z.pn.parse_node.start_loc.s];
  if (after)
    stderr.writefln("%s:%d: syntax error after '%s'", fn, p.loc.line, after);
    /* fprintf(stderr, "%s:%d: syntax error after '%s'\n", fn, p.loc.line, after); */
  else
    stderr.writefln("%s:%d: syntax error", fn, p.loc.line);
    /* fprintf(stderr, "%s:%d: syntax error\n", fn, p.loc.line); */
}

private void
update_line(const (char) *s, const char *e, int *line) {
  for (;s < e; s++) if (*s == '\n') (*line)++;
}

private int
error_recovery(Parser *p) {
  SNode *sn, best_sn = null;
  const (char) *best_s = null, ss, s;
  int head = 0, tail = 0, res = 1;
  D_ErrorRecoveryHint *best_er = null;

  if (!p.snode_hash.last_all)
    return res;
  p.loc = p.snode_hash.last_all.loc;
  if (!p.error_recovery)
    return res;
  SNode*[] q = new SNode*[ERROR_RECOVERY_QUEUE_SIZE];
  if (p.loc.line > p.last_syntax_error_line) {
    p.last_syntax_error_line = p.loc.line;
    p.syntax_errors++;
    p.syntax_error_fn(p);
  }
  for (sn = p.snode_hash.last_all; sn; sn = sn.all_next) {
    if (tail < ERROR_RECOVERY_QUEUE_SIZE - 1)
      q[tail++] = sn;
  }
  s = p.snode_hash.last_all.loc.s;
  while (tail > head) {
    sn = q[head++];
      foreach(ref rer; p.t.states[sn.stateIndex].error_recovery_hints) {
        ss = find_substr(s, rer.str);
        if (ss) {
          if (!best_sn || ss < best_s ||
              (best_sn && ss == best_s &&
               (best_sn.depth < sn.depth ||
                 (best_sn.depth == sn.depth &&
                   best_er.depth < rer.depth))))
          {
            best_sn = sn;
            best_s = ss;
            best_er = &rer;
          }
        }
      }
    foreach (i; sn.zns)
      if (i)
        foreach (j; i.sns) {
          if (tail < ERROR_RECOVERY_QUEUE_SIZE - 1)
            q[tail++] = j;
        }
  }
  if (best_sn) {
    d_loc_t best_loc = p.loc;

    D_Reduction *rr = new D_Reduction();
    vec_add(&p.error_reductions, rr);
    rr.nelements = cast(ushort)(best_er.depth + 1);
    rr.symbol = best_er.symbol;
    update_line(best_loc.s, best_s, &best_loc.line);
    best_loc.s = cast(char*)best_s;
    PNode *best_pn;
    foreach (i; best_sn.zns)
      if (i) {
        best_pn = i.pn;
        break;
      }
    best_pn.parse_node.white_space(
      p, &best_loc, cast(void**)&best_pn.parse_node.globals);
    PNode *new_pn = add_PNode(p, 0, &p.loc, best_loc.s, best_pn, null, null, null);
    SNode *new_sn = new_SNode(p, best_sn.stateIndex, &best_loc, new_pn.initial_scope, new_pn.initial_globals);
    new_sn.last_pn = new_pn;
    ZNode *z = new_ZNode(p, new_pn);
    set_add_znode(&new_sn.zns, z);
    vec_add(&z.sns, best_sn);
    Reduction *r = new Reduction();
    r.znode = z;
    r.snode = new_sn;
    r.reduction = rr;
    r.new_snode = null;
    r.next = null;
    free_old_nodes(p);
    free_old_nodes(p);
    reduce_one(p, r);
    for (int i = 0; i < p.snode_hash.m; i++)
      for (sn = p.snode_hash.v[i]; sn; sn = sn.bucket_next)
        foreach (z; sn.zns)
        {
          if (z)
            if (z.pn.reduction == rr) {
              z.pn.evaluated = 1;
              z.pn.error_recovery = 1;
            }
        }
    if (p.shifts_todo || p.reductions_todo)
      res = 0;
  }
  return res;
}

bool PASS_CODE_FOUND(D_Pass* _p, PNode* _pn)
{
    return (_pn.reduction && _pn.reduction.pass_code.length > _p.index &&
                                  _pn.reduction.pass_code[_p.index]);
}

private void
pass_call(Parser *p, D_Pass *pp, PNode *pn) {
  if (PASS_CODE_FOUND(pp, pn))
    pn.reduction.pass_code[pp.index](
      pn, cast(void**)&pn.children.v[0], pn.children.n,
      cast(int)&(cast(PNode*)(null)).parse_node, p);
}

private void
pass_preorder(Parser *p, D_Pass *pp, PNode *pn) {
  int found = PASS_CODE_FOUND(pp, pn), i;
  pass_call(p, pp, pn);
  if ((pp.kind & D_PASS_FOR_ALL) ||
      ((pp.kind & D_PASS_FOR_UNDEFINED) && !found))
    for (i = 0; i < pn.children.n; i++)
      pass_preorder(p, pp, pn.children.v[i]);
}

private void
pass_postorder(Parser *p, D_Pass *pp, PNode *pn) {
  int found = PASS_CODE_FOUND(pp, pn);
  if ((pp.kind & D_PASS_FOR_ALL) ||
      ((pp.kind & D_PASS_FOR_UNDEFINED) && !found))
    foreach (i; pn.children)
      pass_postorder(p, pp, i);
  pass_call(p, pp, pn);
}

void
d_pass(D_Parser *p, D_ParseNode *apn, int pass_number) {
  PNode *pn = D_ParseNode_to_PNode(apn);
  D_Pass *pp;

  if (pass_number >= p.t.passes.length)
    d_fail("bad pass number: %d\n", pass_number);
  pp = &p.t.passes[pass_number];
  if (pp.kind & D_PASS_MANUAL)
    pass_call(p, pp, pn);
  else if (pp.kind & D_PASS_PRE_ORDER)
    pass_preorder(p, pp, pn);
  else if (pp.kind & D_PASS_POST_ORDER)
    pass_postorder(p, pp, pn);
}

private int
exhaustive_parse(Parser *p, int state) {
  char *pos, epos = null;
  PNode tpn;
  int progress = 0;
  d_loc_t loc;

  pos = p.loc.ws = p.loc.s = p.start;
  loc = p.loc;
  p.initial_white_space_fn(p, &loc, &p.initial_globals);
  /* initial state */
  SNode *sn = add_SNode(p, state, &loc, p.top_scope, p.initial_globals);
  tpn.parse_node.white_space = p.initial_white_space_fn;
  tpn.parse_node.globals = p.initial_globals;
  tpn.initial_scope = tpn.parse_node.scope_ = p.top_scope;
  tpn.parse_node.end = loc.s;
  PNode *pn = add_PNode(p, 0, &loc, loc.s, &tpn, null, null, null);
  sn.last_pn = pn;
  set_add_znode(&sn.zns, new_ZNode(p, pn));
  while (1) {
    /* reduce all */
    while (p.reductions_todo) {
      pos = p.reductions_todo.snode.loc.s;
      if (p.shifts_todo && p.shifts_todo.snode.loc.s < pos)
        break;
      if (pos > epos) {
        epos = pos;
        free_old_nodes(p);
      }
      Reduction* r = p.reductions_todo;
      while (r && r.snode.loc.s == pos) {
        p.reductions_todo = p.reductions_todo.next;
        reduce_one(p, r);
        r = p.reductions_todo;
      }
    }
    /* done? */
    if (!p.shifts_todo) {
      if (p.accept &&
          (p.accept.loc.s == p.end || p.partial_parses))
        return 0;
      else {
        if (error_recovery(p))
          return 1;
        continue;
      }
    } else if (!p.dont_compare_stacks && p.shifts_todo.next)
      cmp_stacks(p);
    /* shift all */
    pos = p.shifts_todo.snode.loc.s;
    if (pos > epos) {
      epos = pos;
      free_old_nodes(p);
    }
    progress++;
    int ready = progress > p.commit_actions_interval;
    if (ready && !p.shifts_todo.next && !p.reductions_todo) {
      commit_stack(p, p.shifts_todo.snode);
      ready = progress = 0;
    }
    shift_all(p, pos);
    if (ready && p.reductions_todo && !p.reductions_todo.next) {
      commit_stack(p, p.reductions_todo.snode);
      progress = 0;
    }
  }
}

private bool wspace(char _x) // doesn't include nl
{
    switch (_x)
    {
        case '\t', '\v', '\f', '\r', ' ': return true;
        default: return false;
    }
}

void
white_space(D_Parser *p, d_loc_t *loc, void **p_user_globals) {
  int rec = 0;
  char *s = loc.s, scol = null;

  if (*s == '#' && loc.col == 0) {
  Ldirective:
    {
      char *save = s;
      s++;
      while (wspace(*s)) s++;
      if (!strncmp("line", s, 4)) {
        if (wspace(s[4])) {
          s += 5;
          while (wspace(*s)) s++;
        }
      }
      if (isDigit(*s)) {
        loc.line = atoi(s) - 1;
        while (isDigit(*s)) s++;
        while (wspace(*s)) s++;
        if (*s == '"')
          loc.pathname = s;
      } else {
        s = save;
        goto Ldone;
      }
    }
    while (*s && *s != '\n') s++;
  }
 Lmore:
  while (wspace(*s)) s++;
  if (*s == '\n') {
    loc.line++;
    scol = s + 1;
    s++;
    if (*s == '#')
      goto Ldirective;
    else
      goto Lmore;
  }
  if (s[0] == '/') {
    if (s[1] == '/') {
      while (*s && *s != '\n') { s++; }
      goto Lmore;
    }
    if (s[1] == '*') {
      s += 2;
    LnestComment:
      rec++;
    LmoreComment:
      while (*s) {
        if (s[0] == '*' && s[1] == '/') {
          s += 2;
          rec--;
          if (!rec)
            goto Lmore;
          goto LmoreComment;
        }
        if (s[0] == '/' && s[1] == '*') {
          s += 2;
          goto LnestComment;
        }
        if (*s == '\n') {
          loc.line++;
          scol = s + 1;
        }
        s++;
      }
    }
  }
 Ldone:
  if (scol)
    loc.col = cast(uint)(s - scol);
  else
    loc.col += s - loc.s;
  loc.s = s;
  return;
}

void null_white_space(D_Parser *p, d_loc_t *loc, void **p_globals) { }

D_Parser *
new_D_Parser(D_ParserTables *t, int sizeof_ParseNode_User) {
  Parser* p = new Parser();
  p.t = t;
  p.loc.line = 1;
  p.sizeof_user_parse_node = sizeof_ParseNode_User;
  p.commit_actions_interval = DEFAULT_COMMIT_ACTIONS_INTERVAL;
  p.syntax_error_fn = &syntax_error_report_fn;
  p.ambiguity_fn = &ambiguity_abort_fn;
  p.error_recovery = 1;
  p.save_parse_tree = t.save_parse_tree;
  if (p.t.default_white_space)
    p.initial_white_space_fn = p.t.default_white_space;
  else if (p.t.whitespace_state)
    p.initial_white_space_fn = &parse_whitespace;
  else
    p.initial_white_space_fn = &white_space;
  return p;
}

void
free_D_Parser(D_Parser *p) {
  if (p.top_scope && !p.initial_scope)
    free_D_Scope(p.top_scope, 0);
  if (p.whitespace_parser)
    free_D_Parser(p.whitespace_parser);
}

void
free_D_ParseNode(D_Parser * p, D_ParseNode *dpn) {
  if (dpn != NO_DPN) {
    free_parser_working_data(p);
  }
}

private void
copy_user_configurables(Parser *pp, Parser *p) {
  memcpy((cast(char*)&pp.start_state) + (pp.start_state).sizeof,
         (cast(char*)&p.start_state) + (p.start_state).sizeof,
         (cast(char*)&pp.syntax_errors - cast(char*)&pp.start_state));
}

Parser *
new_subparser(Parser *p) {
  Parser * pp = new_D_Parser(p.t, p.sizeof_user_parse_node);
  copy_user_configurables(pp, p);
  pp.end = p.end;
  alloc_parser_working_data(pp);
  return pp;
}

private void
initialize_whitespace_parser(Parser *p) {
  if (p.t.whitespace_state) {
    p.whitespace_parser = new_subparser(p);
    p.whitespace_parser.initial_white_space_fn = &null_white_space;
    p.whitespace_parser.error_recovery = 0;
    p.whitespace_parser.partial_parses = true;
    p.whitespace_parser.free_node_fn = p.free_node_fn;
  }
}

private void
free_whitespace_parser(Parser *p) {
  if (p.whitespace_parser) {
    free_D_Parser(p.whitespace_parser);
    p.whitespace_parser = null;
  }
}

private PNode *
handle_top_level_ambiguities(Parser *p, SNode *sn) {
  ZNode *z = null;
  PNode *pn = null, last = null;
  foreach (i; sn.zns) {
    if (i) {
      PNode *x = i.pn;
      LATEST(x);
      if (!pn) {
        z = i;
        pn = x;
      } else  {
        if (x != pn && !x.ambiguities && x != last) {
          x.ambiguities = pn.ambiguities;
          pn.ambiguities = x;
          if (!last) last = x;
        }
      }
    }
  }
  sn.zns.v[0] = z;
  sn.zns.n = 1;
  sn.zns.i = 0;
  return pn;
}

D_ParseNode *
dparse(D_Parser *p, char *buf, int buf_len) {
    PNode *pn;
    D_ParseNode *res = null;

    p.states = p.scans = p.shifts = p.reductions = p.compares = 0;
    p.start = buf;
    p.end = buf + buf_len;

    initialize_whitespace_parser(p);
    alloc_parser_working_data(p);
    if (p.initial_scope)
        p.top_scope = p.initial_scope;
    else {
        if (p.top_scope)
            free_D_Scope(p.top_scope, 0);
        p.top_scope = new_D_Scope(null);
        p.top_scope.kind = D_SCOPE_SEQUENTIAL;
    }
    int r = exhaustive_parse(p, p.start_state);
    if (!r) {
        SNode *sn = p.accept;
        if (sn.zns.n != 1)
            pn = handle_top_level_ambiguities(p, sn);
        else
            pn = sn.zns.v[0].pn;
        pn = commit_tree(p, pn);

        if (d_verbose_level) {
            logf(
                    "%d states %d scans %d shifts %d reductions "
                    "%d compares %d ambiguities\n",
                    p.states, p.scans, p.shifts, p.reductions,
                    p.compares, p.ambiguities);
            if (p.save_parse_tree) {
                if (d_verbose_level > 1)
                    xprint_paren(p, pn);
                else
                    print_paren(p, pn);
                logf("\n");
            }
        }
        if (p.save_parse_tree) {
            res = &pn.parse_node;
        } else
            res = NO_DPN;
    }

    p.accept = null;
    free_parser_working_data(p);
    free_whitespace_parser(p);
    return res;
}

