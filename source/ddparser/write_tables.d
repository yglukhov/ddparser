/*
  Copyright 2002-2006 John Plevyak, All Rights Reserved
*/
module ddparser.write_tables;

import ddparser.util;
import ddparser.lex;
import ddparser.gram;
import ddparser.dparse_tables;
import ddparser.lr;

import std.conv;
import std.stdio;
import std.bitmanip;
import std.system;
import std.array;
import std.string;
import std.algorithm;


private struct ScannerBlock { 
  int state_index; 
  uint scanner_index; 
  int block_index; 
  ScanState*[] chars; 
  ScanStateTransition*[] transitions; 
}

private alias VecScannerBlock = Vec!(ScannerBlock*);
private alias VecState = Vec!(State*);

private int
scanner_size(State *s) {
  if (s.scanner.states.n < 255 && s.scanner.transitions.n < 255)
    return 1;
  if (s.scanner.states.n < 32384 && s.scanner.transitions.n < 32384)
    return 2;
  return 4;
}

extern(C) private uint32
scanner_block_hash_fn(ScannerBlock *b, hash_fns_t *fns) {
  uint32 hash = 0;
  intptr_t i, block_size = cast(intptr_t)fns.data[0];
  ScanState*[] sb = b.chars;

  for (i = 0; i < block_size; i++) {
    hash *= 17;
    hash += sb[i] ? sb[i].index + 2 : 1;
  }
  return hash;
}

extern(C) private int
scanner_block_cmp_fn(ScannerBlock *a, ScannerBlock *b, hash_fns_t *fns) {
  intptr_t i, block_size = cast(intptr_t)fns.data[0];
  ScanState*[] sa = a.chars;
  ScanState*[] sb = b.chars;
    
  for (i = 0; i < block_size; i++) {
    if (sa[i] == sb[i])
      continue;
    if (!sa[i] || !sb[i])
      return 1;
    if (sa[i].index != sb[i].index)
      return 1;
  }
  return 0;
}

hash_fns_t scanner_block_fns;

static this()
{
scanner_block_fns = hash_fns_t(
  cast(hash_fn_t)&scanner_block_hash_fn,
  cast(cmp_fn_t)&scanner_block_cmp_fn,
  [null, null]
);
}

extern(C) private uint32
trans_scanner_block_hash_fn(ScannerBlock *b, hash_fns_t *fns) {
  uint32 hash = 0;
  intptr_t i, block_size = cast(intptr_t)fns.data[0];
  ScanStateTransition*[] sb = b.transitions;

  for (i = 0; i < block_size; i++) {
    hash *= 3;
    hash += sb[i] ? sb[i].index + 1 : 0;
  }
  return hash;
}

extern(C) private int
trans_scanner_block_cmp_fn(ScannerBlock *a, ScannerBlock *b, hash_fns_t *fns) {
  intptr_t i, block_size = cast(intptr_t)fns.data[0];
  ScanStateTransition*[] sa = a.transitions;
  ScanStateTransition*[] sb = b.transitions;
    
  for (i = 0; i < block_size; i++) {
    if (sa[i] == sb[i])
      continue;
    if (!sa[i] || !sb[i])
      return 1;
    if (sa[i].index != sb[i].index)
      return 1;
  }
  return 0;
}

hash_fns_t 
trans_scanner_block_fns;

static this()
{
trans_scanner_block_fns = hash_fns_t(
  cast(hash_fn_t)&trans_scanner_block_hash_fn,
  cast(cmp_fn_t)&trans_scanner_block_cmp_fn,
  [null, null]
);
}

extern(C) private uint32
shift_hash_fn(Action *sa, hash_fns_t *fns) {
  return sa.term.index + (sa.kind == ActionKind.ACTION_SHIFT_TRAILING ? 1000000 : 0);
}

extern(C) private int
shift_cmp_fn(Action *sa, Action *sb, hash_fns_t *fns) {
  return (sa.term.index != sb.term.index) || (sa.kind != sb.kind);
}

hash_fns_t 
shift_fns;

static this()
{
shift_fns = hash_fns_t(
  cast(hash_fn_t)&shift_hash_fn,
  cast(cmp_fn_t)&shift_cmp_fn,
  [null, null]
);
}


private void
buildScannerData(Grammar *g, ref BuildTables tables) {
    ScannerBlock *xv, yv;
    VecScannerBlock scanner_block_hash[4];
    VecScannerBlock *pscanner_block_hash;
    VecScannerBlock trans_scanner_block_hash[4];
    VecScannerBlock *ptrans_scanner_block_hash;
    VecAction shift_hash;
    int k, x, xx;
    VecScanState *ss;

    D_Shift*[] allShifts = new D_Shift*[g.terminals.length];
    D_Shift*[uint] allTShifts;

    /* shift_actions */
    for (int i = 0; i < g.terminals.length; i++) {
        int action_index = -1;
        Term *t = g.terminals[i];
        if (t.regex_production) {
            action_index = t.regex_production.rules.v[0].action_index;
        }
        D_Shift* shift = new D_Shift();
        shift.symbol = cast(ushort)(t.index + g.productions.length);
        shift.shift_kind = cast(ubyte)t.scan_kind;
        shift.op_assoc = cast(ubyte)t.op_assoc;
        shift.op_priority = t.op_priority;
        shift.term_priority = t.term_priority;
        shift.action_index = action_index;
        shift.speculative_code = tables.spec_code;
        allShifts[i] = shift;
        if (t.trailing_context) {
            shift = new D_Shift();
            shift.symbol = cast(ushort)(t.index + g.productions.length);
            shift.shift_kind = D_SCAN_TRAILING;
            shift.op_assoc = cast(ubyte)t.op_assoc;
            shift.op_priority = t.op_priority;
            shift.term_priority = t.term_priority;
            shift.action_index = action_index;
            shift.speculative_code = tables.spec_code;
            allTShifts[i] = shift;
        }
    }
    /* scanners */
    int nvsblocks = 0;
    foreach (i; g.states)
        nvsblocks += i.scanner.states.n * g.scanner_blocks;
    ScannerBlock[] vsblock = new ScannerBlock[nvsblocks ? nvsblocks : 1];
    for (int i = 0; i < 4; i++) {
        vec_clear(&scanner_block_hash[i]);
        vec_clear(&trans_scanner_block_hash[i]);
    }
    scanner_block_fns.data[0] = cast(void*)cast(uintptr_t)g.scanner_block_size;
    scanner_block_fns.data[1] = cast(void*)g;
    trans_scanner_block_fns.data[0] = cast(void*)cast(uintptr_t)g.scanner_block_size;
    trans_scanner_block_fns.data[1] = cast(void*)g;
    /* shift */
    vec_clear(&shift_hash);
    int ivsblock = 0;

    TableMap!(D_Shift*[], 2) tables_d_accepts_diff2;
    TableMap!(D_Shift *[], 2) tables_d_shift2;
    TableMap!(ubyte[], 3) tables_d_accepts_diff3;
    TableMap!(ubyte[], 3) tables_d_scanner3;

    for (int i = 0; i < g.states.length; i++) {
        State *s = g.states[i];
        if (s.same_shifts)
            continue;
        ss = &s.scanner.states;

        /* build accepts differences */
        for (int j = 0; j < s.scanner.transitions.n; j++) {
            D_Shift* d_accepts_diff2[];
            foreach (k; s.scanner.transitions.v[j].accepts_diff) {
                if (k.kind != ActionKind.ACTION_SHIFT_TRAILING)
                    d_accepts_diff2 ~= allShifts[k.term.index];
                else
                    d_accepts_diff2 ~= allTShifts[k.term.index];
            }
            //d_accepts_diff2 ~= null;
            tables_d_accepts_diff2[i, j] = d_accepts_diff2;
        }
        if (s.scanner.transitions.n) {
            D_Shift*[] d_accepts_diff1[];
            for (int j = 0; j < s.scanner.transitions.n; j++) {
                d_accepts_diff1 ~= tables_d_accepts_diff2[i,j];
            }
            tables.d_accepts_diff1[i] = d_accepts_diff1;
        }
        /* build scanner_block_hash */
        pscanner_block_hash = &scanner_block_hash[scanner_size(s)-1]; 
        ptrans_scanner_block_hash = &trans_scanner_block_hash[scanner_size(s)-1]; 
        for (int j = 0; j < ss.n; j++) {
            if (!s.same_shifts) {
                for (k = 0; k < g.scanner_blocks; k++) {
                    vsblock[ivsblock].state_index = s.index;
                    vsblock[ivsblock].scanner_index = j;
                    vsblock[ivsblock].block_index = k;
                    vsblock[ivsblock].chars = 
                        ss.v[j].chars[k * g.scanner_block_size .. $];
                    vsblock[ivsblock].transitions = 
                        ss.v[j].transition[k * g.scanner_block_size .. $];
                    xv = &vsblock[ivsblock];
                    ivsblock++;
                    assert(ivsblock <= nvsblocks);
                    /* output state scanner blocks */
                    yv = cast(ScannerBlock *)set_add_fn(pscanner_block_hash, xv, &scanner_block_fns);
                    if (xv == yv) {
                        int size = scanner_size(s);
                        auto d_scanner3 = appender!(ubyte[])();
                        for (x = 0; x < g.scanner_block_size; x++) {
                            xx = x + k * g.scanner_block_size;
                            uint val = ss.v[j].chars[xx] ? ss.v[j].chars[xx].index + 1 : 0;
                            if (size == 1)
                                d_scanner3.append!(ubyte, endian)(cast(ubyte)val);
                            else if (size == 2)
                                d_scanner3.append!(ushort, endian)(cast(ushort)val);
                            else
                                d_scanner3.append!(uint, endian)(val);
                        }
                        tables_d_scanner3[i,j,k] = d_scanner3.data;
                    }
                    if (s.scan_kind != D_SCAN_LONGEST || s.trailing_context) {
                        /* output accept_diff scanner blocks */
                        yv = cast(ScannerBlock*)set_add_fn(ptrans_scanner_block_hash, xv, 
                                &trans_scanner_block_fns);
                        if (xv == yv) {
                            int size = scanner_size(s);
                            auto d_accepts_diff3 = appender!(ubyte[])();
                            for (x = 0; x < g.scanner_block_size; x++) {
                                xx = x + k * g.scanner_block_size;
                                uint val = ss.v[j].transition[xx].index;
                                if (size == 1)
                                    d_accepts_diff3.append!(ubyte, endian)(cast(ubyte)val);
                                else if (size == 2)
                                    d_accepts_diff3.append!(ushort, endian)(cast(ushort)val);
                                else
                                    d_accepts_diff3.append!(uint, endian)(val);
                            }
                            tables_d_accepts_diff3[i,j,k] = d_accepts_diff3.data;
                        }
                    }
                }
                /* output shifts */
                if (ss.v[j].accepts.n) {
                    string tmp = i.to!string() ~ "." ~ j.to!string();
                    for (k = 0; k < ss.v[j].accepts.n; k++) {
                        Action *a = ss.v[j].accepts.v[k];
                        if (ss.v[j].accepts.n == 1) {
                            if (a.temp_string)
                            {
                                continue;
                            }
                            a.temp_string = tmp;
                            Action *aa = cast(Action*)set_add_fn(&shift_hash, a, &shift_fns);
                            if (aa != a)
                                continue;
                        }
                        /* output shifts */
                        if (!k) 
                        {
                            tables_d_shift2[i,j] = [];
                        }
                        if (a.kind != ActionKind.ACTION_SHIFT_TRAILING) {
                            tables_d_shift2[i,j] ~= allShifts[a.term.index];
                        } else {
                            tables_d_shift2[i,j] ~= allTShifts[a.term.index];
                        }
                    }
                }
            }
        }
    }
    for (int i = 0; i < g.states.length; i++) {
        State *s = g.states[i];
        ss = &s.scanner.states;
        ivsblock = 0;
        if (ss.n && !s.same_shifts) {
            /* output scanner state transition tables */
            /* assume SB_uint8, 16, and 32 have same member offsets */
            static assert((SB_uint8).sizeof == (SB_uint16).sizeof && (SB_uint16).sizeof == (SB_uint32).sizeof);
            SB_uint32[] d_scanner = new SB_uint32[ss.length];
            pscanner_block_hash = &scanner_block_hash[scanner_size(s)-1]; 
            foreach (j, ref sb; d_scanner) {
                if (ss.v[j].accepts.n) {
                    if (ss.v[j].accepts.n == 1) {
                        Action* a = ss.v[j].accepts.v[0];
                        a = cast(Action*)set_add_fn(&shift_hash, a, &shift_fns);
                        sb.shift = tables_d_shift2.storage[a.temp_string];
                    } else
                        sb.shift = tables_d_shift2[i, j];
                }
                for (k = 0; k < g.scanner_blocks; k++) {
                    ScannerBlock vs;
                    vs.state_index = s.index;
                    vs.scanner_index = cast(uint)j;
                    vs.block_index = k;
                    vs.chars = ss.v[j].chars[k * g.scanner_block_size .. $];
                    vs.transitions = 
                        ss.v[j].transition[k * g.scanner_block_size .. $];
                    xv = &vs;
                    yv = cast(ScannerBlock*)set_add_fn(pscanner_block_hash, xv, &scanner_block_fns);
                    assert(yv != xv);
                    sb.scanner_block[k] = cast(uint*)tables_d_scanner3[yv.state_index, yv.scanner_index, yv.block_index].ptr;
                }
            }

            tables.d_scanner1[i] = d_scanner;
            if (s.scan_kind != D_SCAN_LONGEST || s.trailing_context) {
                SB_trans_uint32 d_transition[] = new SB_trans_uint32[ss.length];
                /* output scanner accepts diffs tables */
                ptrans_scanner_block_hash = 
                    &trans_scanner_block_hash[scanner_size(s)-1]; 
                foreach (j, ref trans; d_transition) {
                    for (k = 0; k < g.scanner_blocks; k++) {
                        ScannerBlock vs;
                        vs.state_index = s.index;
                        vs.scanner_index = cast(uint)j;
                        vs.block_index = k;
                        vs.chars = ss.v[j].chars[k * g.scanner_block_size .. $];
                        vs.transitions = 
                            ss.v[j].transition[k * g.scanner_block_size .. $];
                        xv = &vs;
                        yv = cast(ScannerBlock*)set_add_fn(ptrans_scanner_block_hash, xv, 
                                &trans_scanner_block_fns);
                        assert(yv != xv);
                        trans.scanner_block[k] = cast(uint*)tables_d_accepts_diff3[ yv.state_index, yv.scanner_index,
                                    yv.block_index].ptr;
                    }
                }
                tables.d_transition1[i] = d_transition;
            }
        }
    }
}

private Rule* original_reduction(Rule* r)
{
    return r.same_reduction ? r.same_reduction : r;
}

private void
buildGotoData(Grammar *g, ref BuildTables tables) {
    size_t nvalid_bytes = ((g.productions.length + g.terminals.length) + 7) / 8;
    ushort[] vgoto;
    for (int i = 0; i < g.states.length; i++) {
        State *s = g.states[i];
        if (s.gotos.n) {
            /* check for goto on token */
            foreach (j; s.gotos)
                if (j.elem.kind == ElemKind.ELEM_TERM &&
                        j.elem.e.term.kind == TermKind.TERM_TOKEN)
                {
                    s.goto_on_token = 1;
                    break;
                }

            ubyte[] d_goto_valid = new ubyte[nvalid_bytes];
            /* find lowest goto, set valid bits */
            int lowest_sym = elem_symbol(g, s.gotos.v[0].elem);
            SET_BIT(d_goto_valid, lowest_sym);
            for (int j = 1; j < s.gotos.n; j++) {
                int sym = elem_symbol(g, s.gotos.v[j].elem);
                SET_BIT(d_goto_valid, sym);
                if (sym < lowest_sym)
                    lowest_sym = sym;
            }
            /* insert into vgoto */
            bool again = true;
            while (again) {
                again = false;
                for (int j = 0; j < s.gotos.n; j++) {
                    int x = elem_symbol(g, s.gotos.v[j].elem);
                    x -= lowest_sym;
                    if (vgoto.length <= x) 
                        vgoto.length = x + 1;

                    if (vgoto[x]) {
                        again = true;
                        /* undo the damage */
                        for (--j;j >= 0;j--) {
                            x = elem_symbol(g, s.gotos.v[j].elem);
                            x -= lowest_sym;
                            vgoto[x] = 0;
                        }
                        lowest_sym--;
                        break;
                    }
                    else
                    {
                        if (s.gotos.v[j].state.index + 1 > ushort.max)
                            d_fail("goto table overflow");
                        vgoto[x] = cast(ushort)(s.gotos.v[j].state.index + 1);
                    }
                }
            }
            s.goto_table_offset = lowest_sym;
            /* valid bits */
            tables.d_goto_valid[i] = d_goto_valid;
        } else
            s.goto_table_offset = -int.max;
        /* reduce_actions */
        if (s.reduce_actions.n) {
            tables.d_reductions1[i] = s.reduce_actions.array
                .map!(x => tables.reductions[original_reduction(x.rule).index])
                .array();
        }
        /* modified_reduce_actions */
        if (s.right_epsilon_hints.n) {
            tables.d_right_epsilon_hints1[i] =
                s.right_epsilon_hints.array
                .map!(x =>
                    D_RightEpsilonHint(cast(ushort)x.depth,
                                       cast(ushort)x.state.index,
                                       tables.reductions[original_reduction(x.rule).index]))()
                .array();
        }
    }

    /* gotos */
    tables.d_gotos = vgoto;
}

private void
buildReductions(Grammar *g, ref BuildTables tables) {
    foreach (p; g.productions) {
        for (int j = p.rules.n - 1; j >= 0; j--) {
            Rule *r = p.rules.v[j];
            for (int k = 0; k < j; k++)
                if (r.elems.n == p.rules.v[k].elems.n &&
                        r.speculative_code.code == p.rules.v[k].speculative_code.code &&
                        r.final_code.code == p.rules.v[k].final_code.code &&
                        r.op_priority == p.rules.v[k].op_priority &&
                        r.op_assoc == p.rules.v[k].op_assoc &&
                        r.rule_priority == p.rules.v[k].rule_priority &&
                        r.rule_assoc == p.rules.v[k].rule_assoc &&
                        r.action_index == p.rules.v[k].action_index) 
                {
                    if (r.pass_code.n != p.rules.v[k].pass_code.n)
                        continue;

                    bool cont = false;
                    for (int l = 0; l < r.pass_code.n; l++) {
                        if (!r.pass_code.v[l] && !p.rules.v[k].pass_code.v[l])
                            continue;
                        if ((!r.pass_code.v[l] || !p.rules.v[k].pass_code.v[l]) ||
                            (r.pass_code.v[l].code != p.rules.v[k].pass_code.v[l].code))
                        {
                            cont = true;
                            break;
                        }
                    }

                    if (!cont)
                    {
                        r.same_reduction = p.rules.v[k];
                        break;
                    }
                }
        }
        foreach (r; p.rules) {
            if (r.same_reduction)
                continue;
            D_Reduction* red = new D_Reduction();
            tables.reductions[r.index] = red;
            red.nelements = cast(ushort)r.elems.n;
            red.symbol = cast(ushort)r.prod.index;
            if (!r.prod.internal && r.final_code.line == -1)
            {
                red.final_code = r.final_code.f;
            }
            else if (!r.prod.internal && r.action_index >= 0) {
                red.speculative_code = tables.spec_code;
                red.final_code = tables.final_code;
            } else {
                red.speculative_code = null;
                red.final_code = null;
            }

            red.op_assoc = cast(ushort)r.op_assoc;
            red.rule_assoc = cast(ushort)r.rule_assoc;
            red.op_priority = r.op_priority;
            red.rule_priority = r.rule_priority;
            red.action_index = r.prod.internal ? -1 : r.action_index;
            red.pass_code = null;
        }
    }
}

extern(C) private uint32
er_hint_hash_fn(State *a, hash_fns_t *fns) {
  VecHint *sa = &a.error_recovery_hints;
  uint32 hash = 0, i;
  Term *ta;

  for (i = 0; i < sa.n; i++) {
    ta = sa.v[i].rule.elems.v[sa.v[i].rule.elems.n - 1].e.term;
    hash += (sa.v[i].depth + 1) * 13;
    hash += strhashl(ta.string_);
    if (sa.v[i].rule)
      hash += sa.v[i].rule.prod.index * 10007;
  }
  return hash;
}

extern(C) private int
er_hint_cmp_fn(State *a, State *b, hash_fns_t *fns) {
  int i;
  VecHint *sa = &a.error_recovery_hints, sb = &b.error_recovery_hints;
  if (sa.n != sb.n)
    return 1;
  for (i = 0; i < sa.n; i++) {
    Term *ta = sa.v[i].rule.elems.v[sa.v[i].rule.elems.n - 1].e.term;
    Term *tb = sb.v[i].rule.elems.v[sb.v[i].rule.elems.n - 1].e.term;
    if (sa.v[i].depth != sb.v[i].depth ||
	ta.string_ != tb.string_ ||
	sa.v[i].rule.prod.index != sb.v[i].rule.prod.index)
      return 1;
  }
  return 0;
}

hash_fns_t 
er_hint_hash_fns;

static this()
{
er_hint_hash_fns = hash_fns_t(
  cast(hash_fn_t)&er_hint_hash_fn,
  cast(cmp_fn_t)&er_hint_cmp_fn,
  [null, null]
);
}


private void
buildErrorData(Grammar *g, ref BuildTables tables, VecState *er_hash) {
    for (int i = 0; i < g.states.length; i++) {
        State *s = g.states[i];
        if (s.error_recovery_hints.n) {
            State *h = cast(State*)set_add_fn(er_hash, s, &er_hint_hash_fns);
            if (h == s) {
                D_ErrorRecoveryHint d_error_recovery_hints[];
                foreach (erh; s.error_recovery_hints) {
                    Term *t = erh.rule.elems.v[erh.rule.elems.n - 1].e.term;
                    D_ErrorRecoveryHint hint;
                    hint.depth = cast(ushort)erh.depth;
                    hint.symbol = cast(ushort)erh.rule.prod.index;
                    hint.str = escape_string(t.string_);
                    d_error_recovery_hints ~= hint;
                }
                tables.d_error_recovery_hints1[i] = d_error_recovery_hints;
            }
        }
    }
}

private void
buildStateData(Grammar *g, ref BuildTables tables, VecState *er_hash) {
    if (g.states.length) {
        tables.d_states = new D_State[g.states.length];
        foreach (i, ref state; tables.d_states) {
            State *s = g.states[i];
            State *shifts = s.same_shifts ? s.same_shifts : s;

            if (s.gotos.n)
                state.goto_valid = tables.d_goto_valid[i];

            state.goto_table_offset = s.goto_table_offset;

            if (s.reduce_actions.n) {
                state.reductions = tables.d_reductions1[i];
                assert(s.reduce_actions.n == state.reductions.length);
            }

            if (s.right_epsilon_hints.n) {
                state.right_epsilon_hints = tables.d_right_epsilon_hints1[i];
                assert(state.right_epsilon_hints.length == s.right_epsilon_hints.n);
            }

            if (s.error_recovery_hints.n) {
                State* h = cast(State*)set_add_fn(er_hash, s, &er_hint_hash_fns);
                state.error_recovery_hints = tables.d_error_recovery_hints1[h.index];
                assert(state.error_recovery_hints.length == s.error_recovery_hints.n);
            }

            state.shifts = s.shift_actions.n || s.scanner_code || (g.scanner.code && s.goto_on_token);

            if (s.scanner.states.n) {
                state.scanner_table = tables.d_scanner1[shifts.index];
            }

            state.scanner_size = cast(ubyte)scanner_size(s);
            state.accept = s.accept;
            state.scan_kind = cast(ubyte)s.scan_kind;

            if ((shifts.scan_kind != D_SCAN_LONGEST || shifts.trailing_context)
                    && shifts.scanner.states.n) {
                state.transition_table = tables.d_transition1[shifts.index];
            }

            if ((shifts.scan_kind != D_SCAN_LONGEST || shifts.trailing_context)
                    && shifts.scanner.states.n)
                state.accepts_diff = tables.d_accepts_diff1[shifts.index];

            if (s.reduces_to)
                state.reduces_to = s.reduces_to.index;
            else
                state.reduces_to = -1;
        }
    } else {
        d_fail("no states");
    }
}

bool is_EBNF(uint _x)
{
    return _x == InternalKind.INTERNAL_CONDITIONAL || _x == InternalKind.INTERNAL_STAR || _x == InternalKind.INTERNAL_PLUS;
}

private D_SymbolKind d_symbol_values[] = [ 
  D_SymbolKind.D_SYMBOL_STRING, D_SymbolKind.D_SYMBOL_REGEX, D_SymbolKind.D_SYMBOL_CODE, D_SymbolKind.D_SYMBOL_TOKEN ];

private void
buildSymbolData(Grammar *g, ref BuildTables tables) {
    int i = 0;
    D_Symbol[] d_symbols = new D_Symbol[g.productions.length + g.terminals.length];
    foreach (p; g.productions) {
        int state = -1;
        if (!p.internal && p.elem)
            state = p.state.index;
        d_symbols[i].kind = p.internal ? (is_EBNF(p.internal) ? D_SymbolKind.D_SYMBOL_INTERNAL : D_SymbolKind.D_SYMBOL_EBNF) : D_SymbolKind.D_SYMBOL_NTERM;
        d_symbols[i].name = p.name;
        d_symbols[i].start_symbol = state;
        ++i;
    }
    foreach (t; g.terminals) {
        string name = t.term_name;
        if (!name.length) name = escape_string(t.string_);
        d_symbols[i].kind = d_symbol_values[t.kind];
        d_symbols[i].name = name;
        ++i;
    }
    tables.d_symbols = d_symbols;
}

private void
buildPassesData(Grammar *g, ref BuildTables tables) {
  int i;
  if (g.passes.n) {
    D_Pass[] d_passes;
    for (i = 0; i < g.passes.n; i++) {
      D_Pass *p = g.passes.v[i];
      d_passes ~= *p;
    }
    tables.d_passes = d_passes;
  }
}

D_ParserTables* createTablesFromGrammar(Grammar* g, D_ReductionCode spec_code, D_ReductionCode final_code)
{
    VecState er_hash;
    vec_clear(&er_hash);

    g.scanner_block_size = 256/g.scanner_blocks;

    D_ParserTables* result = new D_ParserTables();
    BuildTables tables;

    tables.final_code = final_code;
    tables.spec_code = spec_code;

    buildReductions(g, tables);
    buildScannerData(g, tables);
    buildGotoData(g, tables);
    buildErrorData(g, tables, &er_hash);
    buildStateData(g, tables, &er_hash);
    buildSymbolData(g, tables);
    buildPassesData(g, tables);

    result.states = tables.d_states;
    assert(result.states.length == g.states.length);

    result.goto_table = tables.d_gotos;
    auto ws = lookup_production(g, "whitespace");
    if (ws) result.whitespace_state = ws.state.index;
    result.symbols = tables.d_symbols;
    assert(result.symbols.length == g.productions.length + g.terminals.length);
    result.passes = tables.d_passes;
    assert(result.passes.length == g.passes.length);
    result.save_parse_tree = g.save_parse_tree;
    return result;
}

struct TableMap(T, int dimention = 1)
{
    T[string] storage;

    private string keyWithArgs(size_t i, size_t j, size_t k, size_t l) pure
    {
        assert((dimention == 1 && j == 0 && k == 0 && l == 0)
                || (dimention == 2 && k == 0 && l == 0)
                || (dimention == 3 && l == 0)
                || (dimention == 4));
        switch(dimention)
        {
            case 1: return i.to!string();
            case 2: return i.to!string() ~ "." ~ j.to!string();
            case 3: return i.to!string() ~ "." ~ j.to!string() ~ "." ~ k.to!string();
            case 4: return i.to!string() ~ "." ~ j.to!string() ~ "." ~ k.to!string() ~ "." ~ l.to!string();
            default: assert(false);
        }
    }

    ref T opIndex(size_t i, size_t j = 0, size_t k = 0, size_t l = 0)
    {
        string key = keyWithArgs(i,j,k,l);
        assert(key in storage);
        return storage[key];
    }

    void opIndexAssign(T val, size_t i, size_t j = 0, size_t k = 0, size_t l = 0)
    {
        string key = keyWithArgs(i,j,k,l);
        assert(key !in storage);
        storage[key] = val;
    }
}

struct BuildTables
{
    D_Shift*[][][uint] d_accepts_diff1;
    D_Symbol d_symbols[];

    D_Reduction*[uint] reductions;
    SB_uint32[][uint] d_scanner1;
    SB_trans_uint32[][uint] d_transition1;
    ubyte[][size_t] d_goto_valid;
    D_Reduction*[][size_t] d_reductions1;
    D_RightEpsilonHint[][size_t] d_right_epsilon_hints1;
    D_ErrorRecoveryHint[][size_t] d_error_recovery_hints1;
    ushort d_gotos[];

    D_State[] d_states;
    D_Pass[] d_passes;

    D_ReductionCode spec_code;
    D_ReductionCode final_code;
}

