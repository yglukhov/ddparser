module ddparser.lr;

import ddparser.gram;
import ddparser.util;
import core.stdc.string;
import core.stdc.stdlib;

enum INITIAL_ALLITEMS =	3359;

private uint item_hash(Item* _i)
{
    return  ((cast(uint)_i.rule.index << 8) + 			
     (cast(uint)(_i.kind != ElemKind.ELEM_END ? _i.index : _i.rule.elems.n)));
}

private int
insert_item(State *s, Elem *e) {
  if (set_add(&s.items_hash, e)) {
    vec_add(&s.items, e);
    return 1;
  }
  return 0;
}

extern(C) private int 
itemcmp(const void *ai, const void *aj) {
  uint i = item_hash(*cast(Item**)ai);	
  uint j = item_hash(*cast(Item**)aj);
  return (i > j) ? 1 : ((i < j) ? -1 : 0);
}

private State *
new_state() {
  return new State();
}

private void
free_state(State *s) {
  vec_free(&s.items);
  vec_free(&s.items_hash);
  FREE(s);
}

private State *
maybe_add_state(Grammar *g, State *s) {
    foreach (i; g.states) {
        if (s.hash == i.hash && s.items.n == i.items.n) {
            bool cont = false;
            for (int j = 0; j < s.items.n; j++)
                if (s.items[j] != i.items[j])
                {
                    cont = true;
                    break;
                }
            if (!cont)
            {
                free_state(s);
                return i;
            }
        }
    }
    s.index = g.states.n;
    vec_add(&g.states, s);
    return s;
}

private Elem *
next_elem(Item *i) {
  if (i.index + 1 >= i.rule.elems.n)
    return i.rule.end;
  else
    return i.rule.elems[i.index + 1];
}

private State *
build_closure(Grammar *g, State *s) {
  for (int i = 0; i < s.items.length; i++) { // s.items may change during iteration
    if (s.items[i].kind == ElemKind.ELEM_NTERM) {
      foreach (r; s.items[i].e.nterm.rules)
	insert_item(s, r.elems.v ? 
		    r.elems[0] : r.end);
    }
  }
  qsort(s.items.v, s.items.n, (Item*).sizeof, &itemcmp);
  s.hash = 0;
  foreach (i; s.items)
    s.hash += item_hash(i);
  return maybe_add_state(g, s);
}

private Elem *
clone_elem(Elem *e) {
  Elem *ee = new Elem();
  *ee = *e;
  return ee;
}

private void
add_goto(State *s, State *ss, Elem *e) {
  Goto *g = new Goto();
  g.state = ss;
  g.elem = clone_elem(e);
  vec_add(&s.gotos, g);
}

private void
build_state_for(Grammar *g, State *s, Elem *e) {
  State *ss = null;

  foreach (i; s.items) {
    if (i.kind != ElemKind.ELEM_END && i.kind == e.kind &&
        i.e.term_or_nterm == e.e.term_or_nterm)
    {
      if (!ss) ss = new_state();
      insert_item(ss, next_elem(i));
    }
  }
  if (ss)
    add_goto(s, build_closure(g, ss), e);
}

private void
build_new_states(Grammar *g) {
  Elem e;
  for (int i = 0; i < g.states.length; i++) { // g.states may change during iteration
    foreach (t; g.terminals) {
      e.kind = ElemKind.ELEM_TERM;
      e.e.term = t;
      build_state_for(g, g.states[i], &e);
    }
    foreach (p; g.productions) {
      e.kind = ElemKind.ELEM_NTERM;
      e.e.nterm = p;
      build_state_for(g, g.states[i], &e);
    }
  }
}

private void
build_states_for_each_production(Grammar *g) {
  foreach (p; g.productions)
    if (!p.internal && p.elem) {
      State *s = new_state();
      insert_item(s, p.elem);
      p.state = build_closure(g, s);
    }
}

uint
elem_symbol(Grammar *g, Elem *e) {
  if (e.kind == ElemKind.ELEM_NTERM)
    return e.e.nterm.index;
  else
    return g.productions.n + e.e.term.index;
}

extern(C) private int 
gotocmp(const void *aa, const void *bb) {
  Goto *a = *cast(Goto **)aa, b = *cast(Goto **)bb;
  int i = a.state.index, j = b.state.index;
  return ((i > j) ? 1 : ((i < j) ? -1 : 0));
}

private void
sort_Gotos(Grammar *g) {
  foreach (s; g.states) {
    VecGoto *vg = &s.gotos;
    qsort(vg.v, vg.n, (Goto*).sizeof, &gotocmp);
  }
}

private void
build_LR_sets(Grammar *g) {
  State *s = new_state();
  insert_item(s, g.productions[0].rules[0].elems[0]);
  build_closure(g, s);
  build_states_for_each_production(g);
  build_new_states(g);
  sort_Gotos(g);
}

private Action *
new_Action(Grammar *g, ActionKind akind, Term *aterm, Rule *arule, State *astate) {
  Action *a = new Action();
  a.kind = akind;
  a.term = aterm;
  a.rule = arule;
  a.state = astate;
  a.index = g.action_count++;
  vec_add(&g.actions, a);
  return a;
}

void
free_Action(Action *a) {
  FREE(a);
}

private void
add_action(Grammar *g, State *s, ActionKind akind, Term *aterm, 
	   Rule *arule, State *astate) 
{
  Action *a;
  
  if (akind == ActionKind.ACTION_REDUCE) {
    /* eliminate duplicates */
    foreach (i; s.reduce_actions)
      if (i.rule == arule)
	return;
    a = new_Action(g, akind, aterm, arule, astate);
    vec_add(&s.reduce_actions, a);
  } else {
    /* eliminate duplicates */
    foreach (i; s.shift_actions)
      if (i.term == aterm &&
	  i.state == astate &&
	  i.kind == akind)
	return;
    a = new_Action(g, akind, aterm, arule, astate);
    vec_add(&s.shift_actions, a);
  }
}

private void 
init_LR(Grammar *g) {
  g.action_count = 0;
}

extern(C) private int 
actioncmp(const void *aa, const void *bb) {
  Action *a = *cast(Action **)aa, b = *cast(Action **)bb;
  int i, j;
  if (a.kind == ActionKind.ACTION_SHIFT_TRAILING)
    i = a.term.index + 11000000;
  else if (a.kind == ActionKind.ACTION_SHIFT)
    i = a.term.index + 1000000;
  else
    i = a.rule.index;
  if (b.kind == ActionKind.ACTION_SHIFT_TRAILING)
    j = b.term.index + 11000000;
  else if (b.kind == ActionKind.ACTION_SHIFT)
    j = b.term.index + 1000000;
  else
    j = b.rule.index;
  return ((i > j) ? 1 : ((i < j) ? -1 : 0));
}

void
sort_VecAction(VecAction *v) {
  qsort(v.v, v.n, (Action*).sizeof, &actioncmp);
}

private void
build_actions(Grammar *g) {
    foreach(s; g.states)
    {
        foreach(e; s.items)
        {
            if (e.kind != ElemKind.ELEM_END) {
                if (e.kind == ElemKind.ELEM_TERM) {
                    foreach (z; s.gotos) {
                        if (z.elem.e.term == e.e.term)
                            add_action(g, s, ActionKind.ACTION_SHIFT, 
                                    e.e.term, null, z.state);
                    }
                }
            } else if (e.rule.prod.index)
                add_action(g, s, ActionKind.ACTION_REDUCE, null, e.rule, null);
            else
                s.accept = 1;
        }
        sort_VecAction(&s.shift_actions);
        sort_VecAction(&s.reduce_actions);
    }
}

State *
goto_State(State *s, Elem *e) {
  foreach (i; s.gotos)
    if (i.elem.e.term_or_nterm == e.e.term_or_nterm)
      return i.state;
  return null;
}

private Hint *
new_Hint(uint d, State *s, Rule *r) {
  Hint *h = new Hint();
  h.depth = d;
  h.state = s;
  h.rule = r;
  return h;
}

extern(C) private int 
hintcmp(const void *ai, const void *aj) {
  Hint *i = *cast(Hint**)ai;	
  Hint *j = *cast(Hint**)aj;
  return 
    (i.depth > j.depth) ? 1 : (
      (i.depth < j.depth) ? -1 : (
	(i.rule.index > j.rule.index) ? 1 : (
	  (i.rule.index < j.rule.index) ? -1 : 0)));
}

private void
build_right_epsilon_hints(Grammar *g) {
  foreach (s; g.states) {
      foreach (e; s.items) {
          if (e.kind != ElemKind.ELEM_END) {
              Rule *r = e.rule;
              bool next = false;
              for (int z = e.index; z < r.elems.n; z++) {
                  if ((r.elems[z].kind != ElemKind.ELEM_NTERM ||
                              !r.elems[z].e.nterm.nullable))
                  {
                      next = true;
                      break;
                  }
              }
              if (!next)
              {
                  State *ss = s;
                  for (int z = e.index; z < r.elems.n; z++)
                      ss = goto_State(ss, r.elems[z]);
                  if (ss && r.elems.n)
                      vec_add(&s.right_epsilon_hints, 
                              new_Hint(r.elems.n - e.index - 1, ss, r));
                  else { /* ignore for states_for_each_productions */ }
              }
          }
      }
      if (s.right_epsilon_hints.n > 1)
          qsort(s.right_epsilon_hints.v, s.right_epsilon_hints.n, 
                  (Hint*).sizeof, &hintcmp);
  }
}

private void
build_error_recovery(Grammar *g) {
    foreach (s; g.states) {
        foreach (i; s.items) {
            Rule *r = i.rule;
            if (r.elems.n > 1 &&
                    r.elems[r.elems.n - 1].kind == ElemKind.ELEM_TERM &&
                    r.elems[r.elems.n - 1].e.term.kind == TermKind.TERM_STRING)
            {
                int depth = i.index;
                Elem *e = r.elems[r.elems.n - 1];
                bool done = false;
                foreach (erh; s.error_recovery_hints) {
                    Rule *rr = erh.rule;
                    Elem *ee = rr.elems[rr.elems.n - 1];
                    if (e.e.term.string_ == ee.e.term.string_) 
                    {
                        if (erh.depth > depth)
                            erh.depth = depth;
                        done = true;
                        break;
                    }
                }
                if (!done)
                {
                   vec_add(&s.error_recovery_hints, new_Hint(depth, null, r));
                }
            }
        }
        qsort(s.error_recovery_hints.v, s.error_recovery_hints.n, 
                (Hint*).sizeof, &hintcmp);
    }
}

void
build_LR_tables(Grammar *g) {
  init_LR(g);
  build_LR_sets(g);
  build_actions(g);
  build_right_epsilon_hints(g);
  build_error_recovery(g);
}

