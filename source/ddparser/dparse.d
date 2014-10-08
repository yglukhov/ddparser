module ddparser.dparse;

import ddparser.dparse_tables;
import ddparser.symtab;
import ddparser.parse;

alias d_voidp = void *;

alias D_ParseNode_User = d_voidp;
alias D_ParseNode_Globals = void;

alias D_SyntaxErrorFn = void function(D_Parser *);
alias D_AmbiguityFn = D_ParseNode * function(D_Parser *, 
					     int n, D_ParseNode **v);
alias D_FreeNodeFn = void function(D_ParseNode *d);

struct D_ParseNode {
  int			symbol;
  d_loc_t		start_loc;
  char			*end;
  char			*end_skip;
  D_Scope	*scope_;
  D_WhiteSpaceFn	white_space;
  D_ParseNode_Globals	*globals;
  D_ParseNode_User	user;
}

