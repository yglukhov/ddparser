/*
  Copyright 2002-2008 John Plevyak, All Rights Reserved
*/

#include "d.h"

/* tunables */
#define DEFAULT_COMMIT_ACTIONS_INTERVAL         100
#define PNODE_HASH_INITIAL_SIZE_INDEX           10
#define SNODE_HASH_INITIAL_SIZE_INDEX           8
#define ERROR_RECOVERY_QUEUE_SIZE               10000

#define LATEST(_p, _pn) do { \
  while ((_pn)->latest != (_pn)->latest->latest) { \
    PNode *t = (_pn)->latest->latest; \
    ref_pn(t); \
    unref_pn((_p), (_pn)->latest); \
    (_pn)->latest = t; \
  }\
  (_pn) = (_pn)->latest; \
} while (0)

#ifndef USE_GC
static void free_SNode(struct Parser *p, struct SNode *s);
#define ref_pn(_pn) do { (_pn)->refcount++; } while (0)
#define ref_sn(_sn) do { (_sn)->refcount++; } while (0)
#define unref_pn(_p, _pn) do { if (!--(_pn)->refcount) free_PNode(_p, _pn); } while (0)
#define unref_sn(_p, _sn) do { if (!--(_sn)->refcount) free_SNode(_p, _sn); } while (0)
#else
#define ref_pn(_pn)
#define ref_sn(_sn)
#define unref_pn(_p, _pn)
#define unref_sn(_p, _sn)
#endif

typedef Stack(struct PNode*) StackPNode;
typedef Stack(struct SNode*) StackSNode;
typedef Stack(int) StackInt;

static void free_PNode(Parser *p, PNode *pn);

void
print_paren(Parser *pp, PNode *p) {
  int i;
  char *c;
  LATEST(pp, p);
  if (!p->error_recovery) {
    if (p->children.n) {
      if (p->children.n > 1)
        printf("(");
      for (i = 0; i < p->children.n; i++)
        print_paren(pp, p->children.v[i]);
      if (p->children.n > 1)
        printf(")");
    } else if (p->parse_node.start_loc.s != p->parse_node.end_skip) {
      printf(" ");
      for (c = p->parse_node.start_loc.s; c < p->parse_node.end_skip; c++)
        printf("%c", *c);
      printf(" ");
    }
  }
}

void
xprint_paren(Parser *pp, PNode *p) {
  int i;
  char *c;
  LATEST(pp, p);
  if (!p->error_recovery) {
    printf("[%p %s]", p, pp->t->symbols[p->parse_node.symbol].name);
    if (p->children.n) {
      printf("(");
      for (i = 0; i < p->children.n; i++)
        xprint_paren(pp, p->children.v[i]);
      printf(")");
    } else if (p->parse_node.start_loc.s != p->parse_node.end_skip) {
      printf(" ");
      for (c = p->parse_node.start_loc.s; c < p->parse_node.end_skip; c++)
        printf("%c", *c);
      printf(" ");
    }
    if (p->ambiguities) {
      printf(" |OR| ");
      xprint_paren(pp, p->ambiguities);
    }
  }
}

void xPP(Parser *pp, PNode *p) { xprint_paren(pp, p); printf("\n"); }
void PP(Parser *pp, PNode *p) { print_paren(pp, p); printf("\n"); }

#define D_ParseNode_to_PNode(_apn) \
((PNode*)(D_PN(_apn, -(intptr_t)&((PNode*)(NULL))->parse_node)))


void
alloc_parser_working_data(Parser *p);

void
free_parser_working_data(Parser *p);


#ifdef D_DEBUG

static const char *spaces = "                                                                                                  ";
static void
print_stack(Parser *p, SNode *s, int indent) {
  int i,j;

  printf("%d", (int)(s->state - p->t->state));
  indent += 2;
  for (i = 0; i < s->zns.n; i++) {
    if (!s->zns.v[i])
      continue;
    if (s->zns.n > 1)
      printf("\n%s[", &spaces[99-indent]);
    printf("(%s:", p->t->symbols[s->zns.v[i]->pn->parse_node.symbol].name);
    print_paren(p, s->zns.v[i]->pn);
    printf(")");
    for (j = 0; j < s->zns.v[i]->sns.n; j++) {
      if (s->zns.v[i]->sns.n > 1)
        printf("\n%s[", &spaces[98-indent]);
      print_stack(p, s->zns.v[i]->sns.v[j], indent);
      if (s->zns.v[i]->sns.n > 1)
        printf("]");
    }
    if (s->zns.n > 1)
      printf("]");
  }
}
#endif



 PNode *
commit_tree(Parser *p, PNode *pn);

/*
#define PASS_CODE_FOUND(_p, _pn) ((_pn)->reduction && (_pn)->reduction->npass_code > (_p)->index && \
                                  (_pn)->reduction->pass_code[(_p)->index])

static void
pass_call(Parser *p, D_Pass *pp, PNode *pn) {
  if (PASS_CODE_FOUND(pp, pn))
    pn->reduction->pass_code[pp->index](
      pn, (void**)&pn->children.v[0], pn->children.n,
      (intptr_t)&((PNode*)(NULL))->parse_node, (D_Parser*)p);
}

static void
pass_preorder(Parser *p, D_Pass *pp, PNode *pn) {
  int found = PASS_CODE_FOUND(pp, pn), i;
  pass_call(p, pp, pn);
  if ((pp->kind & D_PASS_FOR_ALL) ||
      ((pp->kind & D_PASS_FOR_UNDEFINED) && !found))
    for (i = 0; i < pn->children.n; i++)
      pass_preorder(p, pp, pn->children.v[i]);
}

static void
pass_postorder(Parser *p, D_Pass *pp, PNode *pn) {
  int found = PASS_CODE_FOUND(pp, pn), i;
  if ((pp->kind & D_PASS_FOR_ALL) ||
      ((pp->kind & D_PASS_FOR_UNDEFINED) && !found))
    for (i = 0; i < pn->children.n; i++)
      pass_postorder(p, pp, pn->children.v[i]);
  pass_call(p, pp, pn);
}

void
d_pass(D_Parser *ap, D_ParseNode *apn, int pass_number) {
  PNode *pn = D_ParseNode_to_PNode(apn);
  Parser *p = (Parser*)ap;
  D_Pass *pp;

  if (pass_number >= p->t->npasses)
    d_fail("bad pass number: %d\n", pass_number);
  pp = &p->t->passes[pass_number];
  if (pp->kind & D_PASS_MANUAL)
    pass_call(p, pp, pn);
  else if (pp->kind & D_PASS_PRE_ORDER)
    pass_preorder(p, pp, pn);
  else if (pp->kind & D_PASS_POST_ORDER)
    pass_postorder(p, pp, pn);
}
*/
int
exhaustive_parse(Parser *p, int state)
;

void
free_D_ParseNode(D_Parser * p, D_ParseNode *dpn) {
  if (dpn != NO_DPN) {
    unref_pn((Parser*)p, DPN_TO_PN(dpn));
    free_parser_working_data((Parser*)p);
  }
#ifdef TRACK_PNODES
  if (((Parser*)p)->xall)
    printf("tracked pnodes\n");
#endif
}

void initialize_whitespace_parser(Parser* p);
void
free_whitespace_parser(Parser *p);

PNode *
handle_top_level_ambiguities(Parser *p, SNode *sn);

D_ParseNode *
dparse(D_Parser *ap, char *buf, int buf_len);
/* { */
/*   int r; */
/*   Parser *p = (Parser *)ap; */
/*   SNode *sn; */
/*   PNode *pn; */
/*   D_ParseNode *res = NULL; */
/*   printf("CPARSE\n"); */
/*  */
/*   p->states = p->scans = p->shifts = p->reductions = p->compares = 0; */
/*   p->start = buf; */
/*   p->end = buf + buf_len; */
/*  */
/*   initialize_whitespace_parser(p); */
/*   alloc_parser_working_data(p); */
/*   if (p->user.initial_scope) */
/*     p->top_scope = p->user.initial_scope; */
/*   else { */
/*     if (p->top_scope) */
/*       free_D_Scope(p->top_scope, 0); */
/*     p->top_scope = new_D_Scope(NULL); */
/*     p->top_scope->kind = D_SCOPE_SEQUENTIAL; */
/*   } */
/*   r = exhaustive_parse(p, p->user.start_state); */
/*   if (!r) { */
/*     sn = p->accept; */
/*     if (sn->zns.n != 1) */
/*       pn = handle_top_level_ambiguities(p, sn); */
/*     else */
/*       pn = sn->zns.v[0]->pn; */
/*     pn = commit_tree(p, pn); */
/*     if (d_verbose_level) { */
/*       printf( */
/*         "%d states %d scans %d shifts %d reductions " */
/*         "%d compares %d ambiguities\n", */
/*         p->states, p->scans, p->shifts, p->reductions, */
/*         p->compares, p->ambiguities); */
/*       if (p->user.save_parse_tree) { */
/*         if (d_verbose_level > 1) */
/*           xprint_paren(p, pn); */
/*         else */
/*           print_paren(p, pn); */
/*         printf("\n"); */
/*       } */
/*     } */
/*     if (p->user.save_parse_tree) { */
/*       ref_pn(pn); */
/*       res = &pn->parse_node; */
/*     } else */
/*       res = NO_DPN; */
/*     unref_sn(p, p->accept); */
/*     p->accept = NULL; */
/*   } else */
/*     p->accept = NULL; */
/*   free_parser_working_data(p); */
/*   free_whitespace_parser(p); */
/*   return res; */
/* } */
