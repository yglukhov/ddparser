/*
  Copyright 2002-2004 John Plevyak, All Rights Reserved
*/
module ddparser.dparse_tables;

import ddparser.dparse;
import ddparser.parse;
import std.conv;
import core.stdc.string;

enum SCANNER_BLOCKS_POW2=		2;
enum SCANNER_BLOCKS	=		(1 << SCANNER_BLOCKS_POW2);
enum SCANNER_BLOCK_SHIFT		=(8 - SCANNER_BLOCKS_POW2);
enum SCANNER_BLOCK_MASK		=((1 << SCANNER_BLOCK_SHIFT) - 1);
enum SCANNER_BLOCK_SIZE		=(256 / SCANNER_BLOCKS);

struct D_ShiftTable;

D_ParseNode* D_PN(T)(void* _x, T _o)
{
    return (cast(D_ParseNode*)(cast(char*)(_x) + _o));
}

struct d_loc_t {
  const(char) *s, pathname, ws;
  int col, line;
}

alias D_WhiteSpaceFn = void function(D_Parser *p, 
        d_loc_t *loc, void **p_globals);
alias D_ScanCode = int function(d_loc_t *loc, ushort *symbol, 
        int *term_priority, ubyte *op_assoc, int *op_priority);
alias D_ReductionCode = int function(
        void *new_ps, void **children, int n_children, int pn_offset,
        D_Parser *parser);

struct D_Reduction {
    ushort	nelements;
    ushort	symbol;
    D_ReductionCode	speculative_code;
    D_ReductionCode	final_code;
    ushort	op_assoc;
    ushort	rule_assoc;
    int 			op_priority;
    int 			rule_priority;
    int			action_index;
    D_ReductionCode[] pass_code;
}

struct D_RightEpsilonHint {
    ushort	depth;
    ushort	preceeding_state;
    D_Reduction		*reduction;
}

struct D_ErrorRecoveryHint {
    ushort	depth;
    ushort	symbol;
    string str;
}

struct D_Shift {
  ushort	symbol;
  ubyte		shift_kind;
  ubyte		op_assoc;
  int			op_priority;
  int			term_priority;
  int			action_index;
  D_ReductionCode	speculative_code;
}

struct SB_(T)
{
    D_Shift*[] shift;
    T* scanner_block[SCANNER_BLOCKS];
}

alias SB_uint8 = SB_!ubyte;
alias SB_uint16 = SB_!ushort;
alias SB_uint32 = SB_!uint;

static assert(SB_uint8.sizeof == SB_uint16.sizeof && SB_uint16.sizeof == SB_uint32.sizeof);

struct SB_trans(T)
{
    T* scanner_block[SCANNER_BLOCKS];
}

alias SB_trans_uint8 = SB_trans!ubyte;
alias SB_trans_uint16 = SB_trans!ushort;
alias SB_trans_uint32 = SB_trans!uint;

static assert(SB_trans_uint8.sizeof == SB_trans_uint16.sizeof && SB_trans_uint16.sizeof == SB_trans_uint32.sizeof);

enum D_SCAN_ALL=	0;
enum D_SCAN_LONGEST	=1;
enum D_SCAN_MIXED	=2;
enum D_SCAN_TRAILING	=3;
enum D_SCAN_RESERVED	=4;
enum D_SCAN_DEFAULT	=D_SCAN_ALL;

struct D_State {
    ubyte[]			goto_valid;
    D_Reduction*[]  reductions;
    D_RightEpsilonHint[] right_epsilon_hints;
    D_ErrorRecoveryHint[] error_recovery_hints;
    SB_uint32[]		scanner_table;
    SB_trans_uint32[]	transition_table;
    D_Shift*[][]     accepts_diff;
    D_ScanCode			scanner_code;
    int				reduces_to;
    int				goto_table_offset;
    bool			shifts;
    ubyte			scanner_size;
    bool			accept;
    ubyte			scan_kind;
}

enum D_SymbolKind
{
    D_SYMBOL_NTERM = 1,
    D_SYMBOL_INTERNAL,
    D_SYMBOL_EBNF,
    D_SYMBOL_STRING,
    D_SYMBOL_REGEX,
    D_SYMBOL_CODE,
    D_SYMBOL_TOKEN
}

struct D_Symbol {
  string name;
  int			start_symbol;
  D_SymbolKind		kind;
}

enum D_PASS_PRE_ORDER	=0x0001;
enum D_PASS_POST_ORDER	=0x0002;
enum D_PASS_MANUAL		=0x0004;
enum D_PASS_FOR_ALL		=0x0008;
enum D_PASS_FOR_UNDEFINED	=0x0010;
struct D_Pass {
    string name;
    uint    kind;
    uint	index;
}

struct D_ParserTables {
    D_State[]   states;
    ushort[]    goto_table;
    uint		whitespace_state;
    D_Symbol[]  symbols;
    D_WhiteSpaceFn	default_white_space;
    D_Pass[]    passes;
    uint		save_parse_tree;
}

