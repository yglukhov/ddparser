module ddparser.lex;

import ddparser.gram;
import ddparser.util;
import ddparser.lr;
import core.stdc.string;
import core.stdc.stdlib;
import core.stdc.ctype;
import core.stdc.stdio;

enum LIVE_DIFF_IN_TRANSITIONS = false;

struct ScanStateTransition {
  uint			index;
  VecAction		live_diff;
  VecAction		accepts_diff;
}

struct ScanState {
  uint			index;
  ScanState 	*chars[256];
  VecAction		accepts;
  VecAction		live;
  ScanStateTransition	*transition[256];
}


struct NFAState {
  uint			index;
  Vec!(NFAState*)	chars[256];
  Vec!(NFAState*)	epsilon;
  Vec!(Action*)		accepts;
  Vec!(Action*)		live;
}

struct DFAState {
  Vec!(NFAState*)	states;
  DFAState	*chars[256];	
  ScanState		*scan;
}

alias VecDFAState = Vec!(DFAState *);
alias VecNFAState = Vec!(NFAState *);

struct LexState {
  uint nfa_index;
  VecNFAState allnfas;
  uint transitions;
  uint scanners;
  uint ignore_case;
}

private NFAState *
new_NFAState(LexState *ls) {
  NFAState *n = new NFAState();
  n.index = ls.nfa_index++;
  vec_add(&ls.allnfas, n);
  return n;
}

private DFAState *
new_DFAState() {
  return new DFAState();
}

private void
free_DFAState(DFAState *y) {
  vec_free(&y.states);
  FREE(y);
}

private void
free_VecDFAState(VecDFAState *dfas) {
  int i;
  for (i = 0; i < dfas.n; i++)
    free_DFAState(dfas.v[i]);
  vec_free(dfas);
}

private void
free_NFAState(NFAState *y) {
  int i;
  for (i = 0; i < 256; i++)
    vec_free(&y.chars[i]);
  vec_free(&y.epsilon);
  vec_free(&y.accepts);
  FREE(y);
}

private void
free_VecNFAState(VecNFAState *nfas) {
  int i;
  for (i = 0; i < nfas.n; i++)
    free_NFAState(nfas.v[i]);
  vec_free(nfas);
}

private ScanState *
new_ScanState() {
  return new ScanState();
}

extern(C) private int 
nfacmp(const void *ai, const void *aj) {
  uint32 i = (*cast(NFAState**)ai).index;
  uint32 j = (*cast(NFAState**)aj).index;
  return (i > j) ? 1 : ((i < j) ? -1 : 0);
}

private void
nfa_closure(DFAState *x) {
    int j, k;

    for (int i = 0; i < x.states.length; ++i) // x.states may change
        foreach (j; x.states[i].epsilon) {
            foreach (k; x.states)
                if (j == k)
                    goto Lbreak;
            vec_add(&x.states, j);
Lbreak:;
        }
    qsort(x.states.v, x.states.n, (x.states.v[0]).sizeof, &nfacmp);
}

private bool
eq_dfa_state(DFAState *x, DFAState *y) {
  if (x.states.n != y.states.n)
    return false;
  for (int i = 0; i < x.states.n; i++)
    if (x.states[i] != y.states[i])
      return false;
  return true;
}

private void
dfa_to_scanner(ref VecDFAState alldfas, ref VecScanState scanner) {
  vec_clear(&scanner);
  for (int i = 0; i < alldfas.n; i++) {
    alldfas[i].scan = new_ScanState();
    alldfas[i].scan.index = i;
    vec_add(&scanner, alldfas[i].scan);
  }
  foreach (i; alldfas) {
    for (int j = 0; j < 256; j++)
      if (i.chars[j])
	i.scan.chars[j] = i.chars[j].scan;
    int highest = int.min;
    foreach (j; i.states)
      foreach (k; j.accepts) {
	int p = k.term.term_priority;
	if (highest < p)
	  highest = p;
      }
    foreach (j; i.states)
      foreach (k; j.accepts) {
	if (k.term.term_priority == highest)
	  vec_add(&i.scan.accepts, k);
      }
  }
}

private void
nfa_to_scanner(NFAState *n, Scanner *s) {
  DFAState *x = new_DFAState(), y;
  VecDFAState alldfas;

  vec_add(&x.states, n);
  nfa_closure(x);
  vec_add(&alldfas, x);
  for (int i = 0; i < alldfas.length; i++) { // alldfas may change while iterating
      for (int i_char = 0; i_char < 256; i_char++) {
          y = null;
          foreach (i_state; alldfas[i].states) {
              foreach (i; i_state.chars[i_char]) {
                  if (!y)
                      y = new_DFAState();
                  set_add(&y.states, i);
              }
          }
          if (y) {
              set_to_vec(&y.states);
              nfa_closure(y);
              foreach (i; alldfas)
                  if (eq_dfa_state(y, i)) {
                      free_DFAState(y);
                      y = i;
                      goto Lnext;
                  }
              vec_add(&alldfas, y);
Lnext:
              alldfas[i].chars[i_char] = y;
          }
      }
  }
  dfa_to_scanner(alldfas, s.states);
  free_VecDFAState(&alldfas);
}

T popFront(T)(ref T[] v)
{
    if (!v.length) return 0;
    scope(exit) v = v[1 .. $];
    return v[0];
}

/* build a NFA for the regular expression */
private int
build_regex_nfa(LexState *ls, ref const(uint8)[] areg, NFAState *pp, NFAState *nn, Action *trailing) {
  uint8 c;
  const(ubyte)[] reg = areg;
  NFAState *p = pp, s, x, n = nn;
  int i, has_trailing = 0;
  uint8 mark[256];

  s = p;
  while ((c = reg.popFront()) != 0) {
      switch(c) {
          case '(':
              x = new_NFAState(ls);
              has_trailing = build_regex_nfa(ls, reg, s, x, trailing) ||
                  has_trailing;
              p = s;
              s = x;
              break;
          case ')':
              goto Lreturn;
          case '|':
              vec_add(&s.epsilon, nn);
              vec_add(&pp.epsilon, (s = new_NFAState(ls)));
              break;
          case '[':
              bool reversed = false;
              if (reg[0] == '^') {
                  reg.popFront();
                  reversed = true;
              }
              memset(mark.ptr, 0, mark.sizeof);
              ubyte pc = ubyte.max;
              while ((c = reg.popFront()) != 0) {
                  switch(c) {
                      case ']':
                          goto Lsetdone;
                      case '-':
                          c = reg.popFront();
                          if (!c)
                              goto Lerror;
                          if (c == '\\')
                              c = reg.popFront();
                          if (!c)
                              goto Lerror;
                          for (;pc <= c; pc++)
                              mark[pc] = 1;
                          break;
                      case '\\':
                          c = reg.popFront();
                          goto default;
                      default:
                          pc = c;
                          mark[c] = 1;
                          break;
                  }
              }
Lsetdone:
              x = new_NFAState(ls);
              for (i = 1; i < 256; i++)
                  if ((!reversed && mark[i]) || (reversed && !mark[i]))
                      vec_add(&s.chars[i], x);
              p = s;
              s = x;
              break;
          case '?':
              vec_add(&p.epsilon, s);
              break;
          case '*':
              vec_add(&p.epsilon, s);
              vec_add(&s.epsilon, p);
              break;
          case '+':
              vec_add(&s.epsilon, p);
              break;
          case '/':
              vec_add(&s.accepts, trailing);
              has_trailing = 1;
              break;
          case '\\':
              c = reg.popFront();
              if (!c)	
                  goto Lerror;
              goto default;
          default:
              if (!ls.ignore_case || !isalpha(c))
                  vec_add(&s.chars[c], (x = new_NFAState(ls))); 
              else {
                  vec_add(&s.chars[tolower(c)], (x = new_NFAState(ls))); 
                  vec_add(&s.chars[toupper(c)], x);
              }
              p = s;
              s = x;
              break;
      }
  }
 Lreturn:
  vec_add(&s.epsilon, n);
  areg = reg;
  return has_trailing;
 Lerror:
  d_fail("bad (part of) regex: %s\n", areg);
  return has_trailing;
}

private void
action_diff(ref VecAction a, ref VecAction b, ref VecAction c) {
  int bb = 0, cc = 0;
  while (1) {
    if (bb >= b.n)
      break;
  Lagainc:
    if (cc >= c.n) {
      while (bb < b.n)
	vec_add(&a, b[bb++]);
      break;
    }
  Lagainb:
    if (b[bb].index == c[cc].index) {
      bb++;
      cc++;
      continue;
    }
    if (b[bb].index < c[cc].index) {
      vec_add(&a, b[bb++]);
      if (bb >= b.n)
	break;
      goto Lagainb;
    }
    cc++;
    goto Lagainc;
  }
}

private void
action_intersect(ref VecAction a, ref VecAction b, ref VecAction c) {
  int bb = 0, cc = 0;
  while (1) {
    if (bb >= b.n)
      break;
  Lagainc:
    if (cc >= c.n)
      break;
  Lagainb:
    if (b[bb].index == c[cc].index) {
      vec_add(&a, b[bb++]);
      cc++;
      continue;
    }
    if (b[bb].index < c[cc].index) {
      bb++;
      if (bb >= b.n)
	break;
      goto Lagainb;
    }
    cc++;
    goto Lagainc;
  }
}

private void
compute_liveness(Scanner *scanner) {
  /* basis */
  foreach (ss; scanner.states) {
    set_union(&ss.live, &ss.accepts);
  }
  bool changed = true;
  while (changed) {
    changed = false;
    foreach (ss; scanner.states) {
      for (int j = 0; j < 256; j++) {
          ScanState* sss = ss.chars[j];
          if (sss) {
              if (ss != sss)
                  if (set_union(&ss.live, &sss.live))
                      changed = true;
          }
      }
    }
  }
  foreach (ss; scanner.states) {
    set_to_vec(&ss.live);
    sort_VecAction(&ss.live);
  }
}

extern(C) private uint32
trans_hash_fn(ScanStateTransition *a, hash_fns_t *fns) {
  uint h = 0;

  if (!fns.data[0])
    foreach (i; a.live_diff)
      h += 3 * i.index;
  foreach (i; a.accepts_diff)
    h += 3 * i.index;
  return h;
}

extern(C) private int
trans_cmp_fn(ScanStateTransition *a, ScanStateTransition *b, hash_fns_t *fns) {
  int i;
  
  if (!fns.data[0])
    if (a.live_diff.n != b.live_diff.n)
      return 1;
  if (a.accepts_diff.n != b.accepts_diff.n)
    return 1;
  if (!fns.data[0])
    for (i = 0; i < a.live_diff.n; i++)
      if (a.live_diff[i] != b.live_diff[i])
	return 1;
  for (i = 0; i < a.accepts_diff.n; i++)
    if (a.accepts_diff[i] != b.accepts_diff[i])
      return 1;
  return 0;
}

private hash_fns_t trans_hash_fns;

static this()
{
    trans_hash_fns = hash_fns_t(
        cast(hash_fn_t)&trans_hash_fn,
        cast(cmp_fn_t)&trans_cmp_fn,
          [ null, null ]
    );
}

private void
build_transitions(LexState *ls, Scanner *s) {
  int j;
  ScanStateTransition *trans = null, x;
  VecScanState *states = &s.states;

  if (LIVE_DIFF_IN_TRANSITIONS)
  {
  trans_hash_fns.data[0] = cast(void*)0;
  }
  else
  {
  trans_hash_fns.data[0] = cast(void*)1;
  }
  foreach (ss; *states) {
    for (j = 0; j < 256; j++) {
      if (!trans) {
    trans = new ScanStateTransition();
      }
      if (ss.chars[j]) {
	action_diff(trans.live_diff, ss.live, ss.chars[j].live);
	action_intersect(trans.accepts_diff, ss.accepts, 
			 trans.live_diff);
      }
      if ((x = cast(ScanStateTransition*)set_add_fn(&s.transitions, trans, &trans_hash_fns)) == trans)
	trans = null;
      else {
	vec_free(&trans.live_diff); 
	vec_free(&trans.accepts_diff);
      }
      ss.transition[j] = x;
    }
  }
  if (trans)
    FREE(trans);
  set_to_vec(&s.transitions);
  for (int i = 0; i < s.transitions.n; i++)
    s.transitions[i].index = i;
  ls.transitions += s.transitions.n;
}

private void
compute_transitions(LexState *ls, Scanner *s) {
  compute_liveness(s);
  build_transitions(ls, s);
}

private void
build_state_scanner(Grammar *g, LexState *ls, State *s) {
  NFAState *nn, nnn;

  bool one = false;
  NFAState *n = new_NFAState(ls);
  /* first strings since they can be trivially combined as a tree */
  foreach (a; s.shift_actions) {
    if (a.kind == ActionKind.ACTION_ACCEPT) {
      one = true;
      if (!n.chars[0].n) 
	vec_add(&n.chars[0], (nnn = new_NFAState(ls)));
      else
	nnn = n.chars[0][0];
      vec_add(&nnn.accepts, a);
    } else if (a.kind == ActionKind.ACTION_SHIFT && a.term.kind == TermKind.TERM_STRING) {
      one = true;
      nn = n;
      if (!a.term.ignore_case) {
	foreach (uint8 c; cast(const uint8[])a.term.string_) {
	  if (!nn.chars[c].n) 
	    vec_add(&nn.chars[c], (nnn = new_NFAState(ls)));
	  else
	    nnn = nn.chars[c][0];
	  nn = nnn;
	}
      } else { /* use new states */
	foreach (uint8 c; cast(const uint8[])a.term.string_) {
      nnn = new_NFAState(ls);
	  if (isalpha(c)) {
	    vec_add(&nn.chars[toupper(c)], nnn);
	    vec_add(&nn.chars[tolower(c)], nnn);
	  } else
	    vec_add(&nn.chars[c], nnn);
	  nn = nnn;
	}
      }
      vec_add(&nn.accepts, a);
    }
  }
  /* now regexes */
  foreach (a; s.shift_actions) {
    if (a.kind == ActionKind.ACTION_SHIFT && a.term.kind == TermKind.TERM_REGEX) {
      Action *trailing_context = new Action();
      *trailing_context = *a;
      trailing_context.kind = ActionKind.ACTION_SHIFT_TRAILING;
      trailing_context.index = g.action_count++;
      one = true;
      const(uint8)[] reg = cast(const uint8[])a.term.string_;
      nnn = new_NFAState(ls);
      vec_add(&n.epsilon, nnn);
      nn = new_NFAState(ls);
      ls.ignore_case = a.term.ignore_case;
      if (build_regex_nfa(ls, reg, nnn, nn, trailing_context)) {
	a.term.trailing_context = 1;
	s.trailing_context = 1;
	vec_add(&g.actions, trailing_context);
      } else
	FREE(trailing_context);
      vec_add(&nn.accepts, a);
    }
  }
  if (one) {
    nfa_to_scanner(n, &s.scanner);
    compute_transitions(ls, &s.scanner);
  }
  free_VecNFAState(&ls.allnfas);
  ls.scanners++;
}

private LexState *
new_LexState() {
  return new LexState();
}

void 
build_scanners(Grammar *g) {
  int k;
  LexState *ls = new_LexState();

  /* detect identical scanners */
  for (int i = 0; i < g.states.n; i++) {
    State *s = g.states[i];
    if (s.same_shifts)
      continue;
    for (int j = 0; j < i; j++) {
      if (g.states[j].same_shifts)
	continue;
      if (g.states[j].shift_actions.n != s.shift_actions.n)
	continue;
      if (g.states[j].scan_kind != s.scan_kind)
	continue;
      for (k = 0; k < g.states[j].shift_actions.n; k++)
	if (s.shift_actions[k].term != 
	    g.states[j].shift_actions[k].term)
	  break;
      if (k >= g.states[j].shift_actions.n) {
	s.same_shifts = g.states[j];
	break;
      }
    }
  }
  /* build scanners */
  foreach (s; g.states) {
    if (s.shift_actions.n) {
      if (s.same_shifts)
	s.scanner = s.same_shifts.scanner;
      else 
	build_state_scanner(g, ls, s);
    }
  }
  if (d_verbose_level)
    printf("%d scanners %d transitions\n", ls.scanners, ls.transitions);
}

