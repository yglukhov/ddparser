/*
  Copyright 2002-2004 John Plevyak, All Rights Reserved
*/
module ddparser.dparse_tables;

import ddparser.dparse;
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
  char *s, pathname, ws;
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
    int			npass_code;
    D_ReductionCode	*pass_code;
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
    D_Shift** shift;
    T* scanner_block[SCANNER_BLOCKS];
}

alias SB_uint8 = SB_!ubyte;
alias SB_uint16 = SB_!ushort;
alias SB_uint32 = SB_!uint;

struct SB_trans(T)
{
    T* scanner_block[SCANNER_BLOCKS];
}

alias SB_trans_uint8 = SB_trans!ubyte;
alias SB_trans_uint16 = SB_trans!ushort;
alias SB_trans_uint32 = SB_trans!uint;


enum D_SCAN_ALL=	0;
enum D_SCAN_LONGEST	=1;
enum D_SCAN_MIXED	=2;
enum D_SCAN_TRAILING	=3;
enum D_SCAN_RESERVED	=4;
enum D_SCAN_DEFAULT	=D_SCAN_ALL;

struct D_State {
    ubyte			*goto_valid;
    int				goto_table_offset;
    D_Reduction*[]  reductions;
    D_RightEpsilonHint[] right_epsilon_hints;
    D_ErrorRecoveryHint[] error_recovery_hints;
    int				shifts;
    D_ScanCode			scanner_code;
    void*				scanner_table;
    ubyte			scanner_size;
    ubyte			accept;
    ubyte			scan_kind;
    void*				transition_table;
    D_Shift			***accepts_diff;
    int				reduces_to;
}

enum D_SymbolKind : uint
{
D_SYMBOL_NTERM		=1,
D_SYMBOL_INTERNAL	=2,
D_SYMBOL_EBNF		=3,
D_SYMBOL_STRING		=4,
D_SYMBOL_REGEX		=5,
D_SYMBOL_CODE		=6,
D_SYMBOL_TOKEN		=7,
}

enum D_SHIFTS_CODE		=(cast(D_Shift**)-1);

enum D_SYMBOL_NTERM		=1;
enum D_SYMBOL_INTERNAL	=2;
enum D_SYMBOL_EBNF		=3;
enum D_SYMBOL_STRING		=4;
enum D_SYMBOL_REGEX		=5;
enum D_SYMBOL_CODE		=6;
enum D_SYMBOL_TOKEN		=7;
struct D_Symbol {
  uint		kind;
  string name;
  int			start_symbol;
}

enum D_PASS_PRE_ORDER	=0x0001;
enum D_PASS_POST_ORDER	=0x0002;
enum D_PASS_MANUAL		=0x0004;
enum D_PASS_FOR_ALL		=0x0008;
enum D_PASS_FOR_UNDEFINED	=0x0010;
struct D_Pass {
    char    *name;
    uint	name_len;
    uint    kind;
    uint	index;
}

struct D_ParserTables {
    D_State[]   states;
    ushort[]    goto_table;
    uint		whitespace_state;
    uint		nsymbols;
    D_Symbol		*symbols;
    D_WhiteSpaceFn	default_white_space;
    uint		npasses;
    D_Pass		*passes;
    uint		save_parse_tree;
}

