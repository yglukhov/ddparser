
import std.bitmanip;

extern(C):

struct D_SymHash;

alias D_UserSym = uint;

struct D_Sym {
  char		 *name;
  int		 len;
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

D_Scope *new_D_Scope(D_Scope *parent);
D_Scope *enter_D_Scope(D_Scope *current, D_Scope *scope_);
D_Scope *commit_D_Scope(D_Scope *scope_);
D_Scope *equiv_D_Scope(D_Scope *scope_);
D_Scope *global_D_Scope(D_Scope *scope_);
D_Scope *scope_D_Scope(D_Scope *current, D_Scope *scope_);
void free_D_Scope(D_Scope *st, int force);
D_Sym *new_D_Sym(D_Scope *st, char *name, char *end, int sizeof_D_Sym);
/* #define NEW_D_SYM(_st, _name, _end) new_D_Sym(_st, _name, _end, sizeof(D_Sym)) */
void free_D_Sym(D_Sym *sym);
D_Sym *find_D_Sym(D_Scope *st, char *name, char *end);
D_Sym *find_global_D_Sym(D_Scope *st, char *name, char *end);
/* use for first update in a production to update scope */
D_Sym *update_D_Sym(D_Sym *sym, D_Scope **st, int sizeof_D_Sym);
/* #define UPDATE_D_SYM(_sym, _st) update_D_Sym(_sym, _st, sizeof(D_Sym)) */
/* use for first subsequent updates in a production */
D_Sym *update_additional_D_Sym(D_Scope *st, D_Sym *sym, int sizeof_D_Sym);
/* #define UPDATE_ADDITIONAL_D_SYM(_st, _sym) update_additional_D_Sym(_st, _sym, sizeof(D_Sym)) */
D_Sym *current_D_Sym(D_Scope *st, D_Sym *sym);
D_Sym *find_D_Sym_in_Scope(D_Scope *st, D_Scope *cur, char *name, char *end);
D_Sym *next_D_Sym_in_Scope(D_Scope **st, D_Sym **sym);
void print_scope(D_Scope *st);

