module ddparser.symtab;

import std.bitmanip;
import ddparser.util;

import core.stdc.string;
import core.stdc.stdio;

enum INITIAL_SYMHASH_SIZE =	3137;

struct D_SymHash {
  int		index;
  int		grow;
  Vec!(D_Sym*)	syms;
}

alias D_UserSym = uint;

struct D_Sym {
  string name;
  uint	 hash;
  D_Scope *scope_;
  D_Sym	 *update_of;
  D_Sym	 *next;
  D_UserSym	 user;
}

enum D_SCOPE_INHERIT =			0;
enum D_SCOPE_RECURSIVE =		1;
enum D_SCOPE_PARALLEL =		2;
enum D_SCOPE_SEQUENTIAL =		3;



struct D_Scope {
    mixin(bitfields!(
                uint, "kind", 2,
                uint, "owned_by_user", 1,  /* don't automatically delete */
                uint, "", 29
                ));
  uint depth;
  D_Sym		 	*ll;
  D_SymHash	*hash;
  D_Sym		 	*updates;
  D_Scope *search;       /* scope to start search */
  D_Scope *dynamic;      /* dynamic scope (e.g. methods) */
  D_Scope *up;		/* enclosing scope */
  D_Scope *up_updates;	/* prior scope in speculative parse */
  D_Scope *down;		/* enclosed scopes (for FREE) */
  D_Scope *down_next;	/* next enclosed scope */
}

/*
  How this works.  In a normal symbol table there is simply
  a stack of scopes representing the scoping structure of
  the program.  Because of speculative parsing, this symbol table
  also has a tree of all 'updates' representing different
  views of the state of scoped variables by each speculative
  parse state.  Periodically, when the parse state collapses
  to a single state (we are nolonger speculating), these changes
  are can be 'committed' and the changes pushed into the top
  level hash table.
 
  All D_Scope's except the top level just have a 'll' of symbols, the
  top level has a 'hash'.
 
  'updates' is a list of changes to symbols in other scopes.  It is
  searched to find the current version of a symbol with respect to the
  speculative parse path represented by D_Scope.

  'up' points to the enclosing scope, it isn't used much.

  'up_updates' is the prior scope in speculative parsing, it is used find
  the next D_Scope to look in for 'updates' after the current one has been
  searched.

  'down' and 'down_next' are used to hold enclosing scopes, or in the
  case of the top level, sibling scopes (created by commmit).
*/
  

private void
symhash_add(D_SymHash *sh, D_Sym *s) {
  uint i, h = s.hash % sh.syms.n, n;
  D_Sym **v = sh.syms.v;
  D_Sym *x;
  Vec!(D_Sym*) vv, tv;

  sh.index++;
  s.next = v[h];
  v[h] = s;

  if (sh.index > sh.grow) {
    vv.v = sh.syms.v;
    vv.n = sh.syms.n;
    sh.syms.n = sh.grow;
    sh.grow = sh.grow * 2 + 1;
    sh.syms.v = cast(D_Sym**)MALLOC(sh.syms.n * (void *).sizeof);
    v = sh.syms.v;
    n = sh.syms.n;
    vec_clear(&tv);
    for (i = 0; i < vv.n; i++)
      /* use temporary to preserve order */
      while (vv.v[i]) {
	x = vv.v[i];
	vv.v[i] = x.next;
	vec_add(&tv, x);
      }
      while (tv.v[i]) {
	x = tv.v[i];
	tv.v[i] = x.next;
	h = x.hash % n;
	x.next = v[h];
	v[h] = x;
      }
    FREE(vv.v);
  }
}

private D_SymHash * 
new_D_SymHash() {
  D_SymHash *sh = new D_SymHash();
  sh.grow = INITIAL_SYMHASH_SIZE * 2 + 1;
  sh.syms.n = INITIAL_SYMHASH_SIZE;
  sh.syms.v = cast(D_Sym**)MALLOC(sh.syms.n * (void *).sizeof);
  memset(sh.syms.v, 0, sh.syms.n * (void *).sizeof);
  return sh;
}

private void
free_D_SymHash(D_SymHash *sh) {
  int i;
  D_Sym *sym;
  for (i = 0; i < sh.syms.n; i++)
    for (; sh.syms.v[i]; sh.syms.v[i] = sym) {
      sym = sh.syms.v[i].next; 
      free_D_Sym(sh.syms.v[i]);
    }
  FREE(sh.syms.v);
  FREE(sh);
}

D_Scope *
new_D_Scope(D_Scope *parent) {
  D_Scope *st = new D_Scope();
  if (parent) {
    st.depth = parent.depth + 1;
    st.kind = parent.kind;
    st.search = parent;
    st.up = parent;
    st.up_updates = parent;
    st.down_next = parent.down;
    parent.down = st;
  } else
    st.hash = new_D_SymHash();
  return st;
}

D_Scope *
equiv_D_Scope(D_Scope *current) {
  D_Scope *s = current, last = current;
  D_Sym *sy;
  if (!s)
    return s;
  while (s.depth >= current.depth) {
    if (s.depth == last.depth) {
      if (current.up == s.up)
	last = s;
      else
	break;
    }
    if (s.ll || s.hash)
      break;
    if (s.dynamic)
      break;
    sy = s.updates;
    while (sy) {
      if (sy.scope_.depth <= current.depth)
	break;
      sy = sy.next;
    }
    if (sy)
      break;
    if (!s.up_updates)
      break;
    s = s.up_updates;
  }
  return last;
}


D_Scope *
enter_D_Scope(D_Scope *current, D_Scope *scope_) {
  D_Scope *st = new D_Scope(), parent = scope_.up;
  st.depth = scope_.depth;
  st.up = parent;
  st.kind = scope_.kind;
  st.search = scope_;
  st.up_updates = current;
  st.down_next = current.down;
  current.down = st;
  return st;
}

D_Scope *
global_D_Scope(D_Scope *current) {
  D_Scope *g = current;
  while (g.up) g = g.search;
  return enter_D_Scope(current, g);
}

D_Scope *
scope_D_Scope(D_Scope *current, D_Scope *scope_) {
  D_Scope *st = new D_Scope(), parent = current.up;
  st.depth = current.depth;
  st.up = parent;
  st.kind = current.kind;
  st.search = current;
  st.dynamic = scope_;
  st.up_updates = current;
  st.down_next = current.down;
  current.down = st;
  return st;
}

void
free_D_Scope(D_Scope *st, int force) {
  D_Scope *s;
  D_Sym *sym;
  for (; st.down; st.down = s) {
    s = st.down.down_next;
    free_D_Scope(st.down, 0);
  }
  if (st.owned_by_user && !force)
    return;
  if (st.hash)
    free_D_SymHash(st.hash);
  else
    for (; st.ll; st.ll = sym) {
      sym = st.ll.next;
      free_D_Sym(st.ll);
    }
  for (; st.updates; st.updates = sym) {
    sym = st.updates.next;
    free_D_Sym(st.updates);
  }
  FREE(st);
}

private void 
commit_ll(D_Scope *st, D_SymHash *sh) {
  D_Sym *sym;
  if (st.search) {
    commit_ll(st.search, sh);
    for (; st.ll; st.ll = sym) {
      sym = st.ll.next;
      symhash_add(sh, st.ll);
    }
  }
}

/* make direct links to the latest update */
private void 
commit_update(D_Scope *st, D_SymHash *sh) {
  int i;
  D_Sym *s;

  for (i = 0; i < sh.syms.n; i++)
    for (s = sh.syms.v[i]; s; s = s.next) 
      s.update_of = current_D_Sym(st, s);
}

/* currently only commit the top level scope */
D_Scope *
commit_D_Scope(D_Scope *st) {
  D_Scope *x = st;
  if (st.up)
    return st;
  while (x.search) x = x.search;
  commit_ll(st, x.hash);
  commit_update(st, x.hash);
  return x;
}

D_Sym *
new_D_Sym(D_Scope *st, string name, int sizeof_D_Sym) {
  D_Sym *s = cast(D_Sym*)MALLOC(sizeof_D_Sym);
  memset(s, 0, sizeof_D_Sym);
  s.name = name;
  s.hash = strhashl(name);
  s.scope_ = st;
  if (st) {
    if (st.hash) {
      symhash_add(st.hash, s);
    } else {
      s.next = st.ll;
      st.ll = s;
    }
  }
  return s;
}

void
free_D_Sym(D_Sym *s) {
  FREE(s);
}

D_Sym *
current_D_Sym(D_Scope *st, D_Sym *sym) {
  D_Scope *sc;
  D_Sym *uu;

  if (sym.update_of) sym = sym.update_of;
  /* return the last update */
  for (sc = st; sc; sc = sc.up_updates) {
    for (uu = sc.updates; uu; uu = uu.next)
      if (uu.update_of == sym)
	return uu;
  }
  return sym;
}

private D_Sym *
find_D_Sym_in_Scope_internal(D_Scope *st, string name, uint h) {
    D_Sym *ll;
    for (;st ; st = st.search) {
        if (st.hash) 
            ll = st.hash.syms.v[h % st.hash.syms.n];
        else
            ll = st.ll;
        while (ll) {
            if (ll.hash == h && ll.name == name)
                return ll;
            ll = ll.next;
        }
        if (st.dynamic)
        {
            ll = find_D_Sym_in_Scope_internal(st.dynamic, name, h);
            if (ll)
                return ll;
        }
        if (!st.search || st.search.up != st.up)
            break;
    }
    return null;
}

private D_Sym *
find_D_Sym_internal(D_Scope *cur, string name, uint h) {
    D_Sym *ll;
    if (!cur)
        return null;
    if (cur.hash) 
        ll = cur.hash.syms.v[h % cur.hash.syms.n];
    else
        ll = cur.ll;
    while (ll) {
        if (ll.hash == h && ll.name == name)
            break;
        ll = ll.next;
    }
    if (!ll) { 
        if (cur.dynamic)
        {
            ll = find_D_Sym_in_Scope_internal(cur.dynamic, name, h);
            if (ll)
                return ll;
        }
        if (cur.search)
            return find_D_Sym_internal(cur.search, name, h);
        return ll;
    }
    return ll;
}

D_Sym *
find_D_Sym(D_Scope *st, string name) {
  uint h = strhashl(name);
  D_Sym *s = find_D_Sym_internal(st, name, h);
  if (s)
    return current_D_Sym(st, s);
  return null;
}

D_Sym *
find_global_D_Sym(D_Scope *st, string name) {
  uint h = strhashl(name);
  D_Scope *cur = st;
  while (cur.up) cur = cur.search;
  D_Sym *s = find_D_Sym_internal(cur, name, h);
  if (s)
    return current_D_Sym(st, s);
  return null;
}

D_Sym *
find_D_Sym_in_Scope(D_Scope *st, D_Scope *cur, string name) {
  uint h = strhashl(name);
  D_Sym *s = find_D_Sym_in_Scope_internal(cur, name, h);
  if (s)
    return current_D_Sym(st, s);
  return null;
}

D_Sym *
next_D_Sym_in_Scope(D_Scope **scope_, D_Sym **sym) {
  D_Sym *last_sym = *sym, ll = last_sym;
  D_Scope *st = *scope_;
  if (ll) {
    ll = ll.next;
    if (ll)
      goto Lreturn;
  }
  for (;st ; st = st.search) {
    if (st.hash) {
      int i = last_sym ? ((last_sym.hash + 1) % st.hash.syms.n) : 0;
      if (!last_sym || i)
	ll = st.hash.syms.v[i];
    } else {
      if (!last_sym)
	ll = st.ll;
    }
    last_sym = null;
    if (ll)
      goto Lreturn;
    if (!st.search || st.search.up != st.up)
      break;
  }
 Lreturn:
  if (ll)
    *sym = ll;
  *scope_ = st;
  return ll;
}

D_Sym *
update_additional_D_Sym(D_Scope *st, D_Sym *sym, int sizeof_D_Sym) {
  sym = current_D_Sym(st, sym);
  D_Sym *s = cast(D_Sym*)MALLOC(sizeof_D_Sym);
  memcpy(s, sym, (D_Sym).sizeof);
  if (sym.update_of) sym = sym.update_of;
  s.update_of = sym;
  s.next = st.updates;
  st.updates = s;
  return s;
}

D_Sym *
update_D_Sym(D_Sym *sym, D_Scope **pst, int sizeof_D_Sym) {
  *pst = enter_D_Scope(*pst, *pst);
  return update_additional_D_Sym(*pst, sym, sizeof_D_Sym);
}

void
print_sym(D_Sym *s) {
  logf("%s, ", s.name);
}

void
print_scope(D_Scope *st) {
  printf("SCOPE %p: ", st);
  printf("  owned: %d, kind: %d, ", st.owned_by_user, st.kind);
  if (st.ll) printf("  LL\n");
  if (st.hash) printf("  HASH\n");
  if (st.hash) {
    int i;
    for (i = 0; i < st.hash.syms.n; i++)
      if (st.hash.syms.v[i])
	print_sym(st.hash.syms.v[i]);
  } else {
    D_Sym *ll = st.ll;
    while (ll) {
      print_sym(ll);
      ll = ll.next;
    }
  }
  printf("\n\n");
  if (st.dynamic) print_scope(st.dynamic);
  if (st.search) print_scope(st.search);
}
