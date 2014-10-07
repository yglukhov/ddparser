module ddparser.lr;

import ddparser.gram;
import ddparser.util;
import core.stdc.string;
import core.stdc.stdlib;

enum INITIAL_ALLITEMS =	3359;

private uint item_hash(Item* _i)
{
    return  ((cast(uint)(_i).rule.index << 8) + 			
     (cast(uint)((_i).kind != ElemKind.ELEM_END ? (_i).index : (_i).rule.elems.n)));
}

private int
insert_item(State *s, Elem *e) {
  Item *i = e;
  if (set_add(&s.items_hash, i)) {
    vec_add(&s.items, i);
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
  State *s = new State();
  memset(s, 0, (State).sizeof);
  return s;
}

private void
free_state(State *s) {
  vec_free(&s.items);
  vec_free(&s.items_hash);
  FREE(s);
}

private State *
maybe_add_state(Grammar *g, State *s) {
  int i, j;

  for (i = 0; i < g.states.n; i++) {
    if (s.hash == g.states.v[i].hash && 
	s.items.n == g.states.v[i].items.n) {
      for (j = 0; j < s.items.n; j++)
	if (s.items.v[j] != g.states.v[i].items.v[j])
	  goto Lcont;
      free_state(s);
      return g.states.v[i];
    Lcont:;
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
    return i.rule.elems.v[i.index + 1];
}

private State *
build_closure(Grammar *g, State *s) {
  int j, k;

  for (j = 0; j < s.items.n; j++) {
    Item *i = s.items.v[j];
    Elem *e = i;
    if (e.kind == ElemKind.ELEM_NTERM) {
      Production *pp = e.e.nterm;
      for (k = 0; k < e.e.nterm.rules.n; k++)
	insert_item(s, pp.rules.v[k].elems.v ? 
		    pp.rules.v[k].elems.v[0] : pp.rules.v[k].end);
    }
  }
  qsort(s.items.v, s.items.n, (Item*).sizeof, &itemcmp);
  s.hash = 0;
  for (j = 0; j < s.items.n; j++)
    s.hash += item_hash(s.items.v[j]);
  return maybe_add_state(g, s);
}

private Elem *
clone_elem(Elem *e) {
  Elem *ee = new Elem();
  memcpy(ee, e, (*ee).sizeof);
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
  int j;
  Item *i;
  State *ss = null;

  for (j = 0; j < s.items.n; j++) {
    i = s.items.v[j];
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
  int i, j;
  State *s;
  Elem e;

  for (i = 0; i < g.states.n; i++) {
    s = g.states.v[i];
    for (j = 0; j < g.terminals.n; j++) {
      e.kind = ElemKind.ELEM_TERM;
      e.e.term = g.terminals.v[j];
      build_state_for(g, s, &e);
    }
    for (j = 0; j < g.productions.n; j++) {
      e.kind = ElemKind.ELEM_NTERM;
      e.e.nterm = g.productions.v[j];
      build_state_for(g, s, &e);
    }
  }
}

private void
build_states_for_each_production(Grammar *g) {
  int i;
  for (i = 0; i < g.productions.n; i++)
    if (!g.productions.v[i].internal && g.productions.v[i].elem) {
      State *s = new_state();
      insert_item(s, g.productions.v[i].elem);
      g.productions.v[i].state = build_closure(g, s);
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
  int i;

  for (i = 0; i < g.states.n; i++) {
    VecGoto *vg = &g.states.v[i].gotos;
    qsort(vg.v, vg.n, (Goto*).sizeof, &gotocmp);
  }
}

private void
build_LR_sets(Grammar *g) {
  State *s = new_state();
  insert_item(s, g.productions.v[0].rules.v[0].elems.v[0]);
  build_closure(g, s);
  build_states_for_each_production(g);
  build_new_states(g);
  sort_Gotos(g);
}

private Action *
new_Action(Grammar *g, ActionKind akind, Term *aterm, Rule *arule, State *astate) {
  Action *a = new Action();
  memset(a, 0, (Action).sizeof);
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
  int i;
  Action *a;
  
  if (akind == ActionKind.ACTION_REDUCE) {
    /* eliminate duplicates */
    for (i = 0; i < s.reduce_actions.n; i++)
      if (s.reduce_actions.v[i].rule == arule)
	return;
    a = new_Action(g, akind, aterm, arule, astate);
    vec_add(&s.reduce_actions, a);
  } else {
    /* eliminate duplicates */
    for (i = 0; i < s.shift_actions.n; i++)
      if (s.shift_actions.v[i].term == aterm &&
	  s.shift_actions.v[i].state == astate &&
	  s.shift_actions.v[i].kind == akind)
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

extern(C) void
sort_VecAction(VecAction *v) {
  qsort(v.v, v.n, (Action*).sizeof, &actioncmp);
}

private void
build_actions(Grammar *g) {
    import std.stdio;
    foreach(s; g.states)
    {
        foreach(e; s.items)
        {
            if (e.kind != ElemKind.ELEM_END) {
                if (e.kind == ElemKind.ELEM_TERM) {
                    for (int z = 0; z < s.gotos.n; z++) {
                        if (s.gotos.v[z].elem.e.term == e.e.term)
                            add_action(g, s, ActionKind.ACTION_SHIFT, 
                                    e.e.term, null, s.gotos.v[z].state);
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
  int i;
  for (i = 0; i < s.gotos.n; i++)
    if (s.gotos.v[i].elem.e.term_or_nterm == e.e.term_or_nterm)
      return s.gotos.v[i].state;
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
  int x, y, z;
  State *s, ss;
  Elem *e;
  Rule *r;

  for (x = 0; x < g.states.n; x++) {
    s = g.states.v[x];
    for (y = 0; y < s.items.n; y++) {
      e = s.items.v[y];
      r = e.rule;
      if (e.kind != ElemKind.ELEM_END) {
	for (z = e.index; z < r.elems.n; z++) {
          if ((r.elems.v[z].kind != ElemKind.ELEM_NTERM ||
	       !r.elems.v[z].e.nterm.nullable))
	    goto Lnext;
	}
	ss = s;
	for (z = e.index; z < r.elems.n; z++)
	  ss = goto_State(ss, r.elems.v[z]);
	if (ss && r.elems.n)
	  vec_add(&s.right_epsilon_hints, 
		  new_Hint(r.elems.n - e.index - 1, ss, r));
	else { /* ignore for states_for_each_productions */ }
      }
      Lnext:;
    }
    if (s.right_epsilon_hints.n > 1)
      qsort(s.right_epsilon_hints.v, s.right_epsilon_hints.n, 
	    (Hint*).sizeof, &hintcmp);
  }
}

private void
build_error_recovery(Grammar *g) {
  int i, j, k, depth;
  State *s;
  Rule *r, rr;
  Elem *e, ee;

  for (i = 0; i < g.states.n; i++) {
    s = g.states.v[i];
    for (j = 0; j < s.items.n; j++) {
      r = s.items.v[j].rule;
      if (r.elems.n > 1 &&
	  r.elems.v[r.elems.n - 1].kind == ElemKind.ELEM_TERM &&
	  r.elems.v[r.elems.n - 1].e.term.kind == TermKind.TERM_STRING)
      {
	depth = s.items.v[j].index;
	e = r.elems.v[r.elems.n - 1];
	for (k = 0; k < s.error_recovery_hints.n; k++) {
	  rr = s.error_recovery_hints.v[k].rule;
	  ee = rr.elems.v[rr.elems.n - 1];
	  if (e.e.term.string_len == ee.e.term.string_len &&
	      !strcmp(e.e.term.string_, ee.e.term.string_)) 
	  {
	    if (s.error_recovery_hints.v[k].depth > depth)
	      s.error_recovery_hints.v[k].depth = depth;
	    goto Ldone;
	  }
	}
	vec_add(&s.error_recovery_hints, new_Hint(depth, null, r));
      Ldone:;
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

