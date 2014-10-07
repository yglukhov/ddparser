/*
  Copyright 2002-2006 John Plevyak, All Rights Reserved
*/
module ddparser.write_tables;

import ddparser.util;
import ddparser.lex;
import ddparser.gram;
import ddparser.dparse_tables;
import ddparser.lr;
import core.stdc.string;
import std.conv;
import core.stdc.stdio;
import std.stdio;
import std.bitmanip;
import std.system;
import std.array;
import std.string;


private struct ScannerBlock { 
  int state_index; 
  int scanner_index; 
  int block_index; 
  ScanState **chars; 
  ScanStateTransition **transitions; 
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
  ScanState **sb = b.chars;

  for (i = 0; i < block_size; i++) {
    hash *= 17;
    hash += sb[i] ? sb[i].index + 2 : 1;
  }
  return hash;
}

extern(C) private int
scanner_block_cmp_fn(ScannerBlock *a, ScannerBlock *b, hash_fns_t *fns) {
  intptr_t i, block_size = cast(intptr_t)fns.data[0];
  ScanState **sa = a.chars;
  ScanState **sb = b.chars;
    
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
  ScanStateTransition **sb = b.transitions;

  for (i = 0; i < block_size; i++) {
    hash *= 3;
    hash += sb[i] ? sb[i].index + 1 : 0;
  }
  return hash;
}

extern(C) private int
trans_scanner_block_cmp_fn(ScannerBlock *a, ScannerBlock *b, hash_fns_t *fns) {
  intptr_t i, block_size = cast(intptr_t)fns.data[0];
  ScanStateTransition **sa = a.transitions;
  ScanStateTransition **sb = b.transitions;
    
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
    State *s;
    ScannerBlock *vsblock, xv, yv;
    VecScannerBlock scanner_block_hash[4];
    VecScannerBlock *pscanner_block_hash;
    VecScannerBlock trans_scanner_block_hash[4];
    VecScannerBlock *ptrans_scanner_block_hash;
    VecAction shift_hash;
    int nvsblocks, ivsblock, i, j, k, x, xx;
    VecScanState *ss;

    D_Shift*[uint] allShifts;
    D_Shift*[uint] allTShifts;

    /* shift_actions */
    for (i = 0; i < g.terminals.n; i++) {
        int action_index = -1;
        Term *t = g.terminals.v[i];
        if (t.regex_production) {
            action_index = t.regex_production.rules.v[0].action_index;
        }
        D_Shift* shift = new D_Shift();
        shift.symbol = cast(ushort)(g.terminals.v[i].index + g.productions.n);
        shift.shift_kind = cast(ubyte)g.terminals.v[i].scan_kind;
        shift.op_assoc = cast(ubyte)g.terminals.v[i].op_assoc;
        shift.op_priority = g.terminals.v[i].op_priority;
        shift.term_priority = g.terminals.v[i].term_priority;
        shift.action_index = action_index;
        shift.speculative_code = tables.spec_code;
        allShifts[i] = shift;
        if (g.terminals.v[i].trailing_context) {
            shift = new D_Shift();
            shift.symbol = cast(ushort)(g.terminals.v[i].index + g.productions.n);
            shift.shift_kind = D_SCAN_TRAILING;
            shift.op_assoc = cast(ubyte)g.terminals.v[i].op_assoc;
            shift.op_priority = g.terminals.v[i].op_priority;
            shift.term_priority = g.terminals.v[i].term_priority;
            shift.action_index = action_index;
            shift.speculative_code = tables.spec_code;
            allTShifts[i] = shift;
        }
    }
    /* scanners */
    nvsblocks = 0;
    for (i = 0; i < g.states.n; i++)
        nvsblocks += g.states.v[i].scanner.states.n * g.scanner_blocks;
    vsblock = cast(ScannerBlock *)MALLOC((nvsblocks ? nvsblocks : 1) * (ScannerBlock).sizeof);
    for (i = 0; i < 4; i++) {
        vec_clear(&scanner_block_hash[i]);
        vec_clear(&trans_scanner_block_hash[i]);
    }
    scanner_block_fns.data[0] = cast(void*)cast(uintptr_t)g.scanner_block_size;
    scanner_block_fns.data[1] = cast(void*)g;
    trans_scanner_block_fns.data[0] = cast(void*)cast(uintptr_t)g.scanner_block_size;
    trans_scanner_block_fns.data[1] = cast(void*)g;
    /* shift */
    vec_clear(&shift_hash);
    ivsblock = 0;

    TableMap!(D_Shift*[], 2) tables_d_accepts_diff2;
    TableMap!(D_Shift *[], 2) tables_d_shift2;


    for (i = 0; i < g.states.n; i++) {
        s = g.states.v[i];
        if (s.same_shifts)
            continue;
        ss = &s.scanner.states;

        /* build accepts differences */
        for (j = 0; j < s.scanner.transitions.n; j++) {
            VecAction *va = &s.scanner.transitions.v[j].accepts_diff;
            D_Shift* d_accepts_diff2[];
            for (k = 0; k < va.n; k++) {
                if (va.v[k].kind != ActionKind.ACTION_SHIFT_TRAILING)
                    d_accepts_diff2 ~= allShifts[va.v[k].term.index];
                else
                    d_accepts_diff2 ~= allTShifts[va.v[k].term.index];
            }
            d_accepts_diff2 ~= null;
            tables_d_accepts_diff2[i, j] = d_accepts_diff2;
        }
        if (s.scanner.transitions.n) {
            D_Shift** d_accepts_diff1[];
            for (j = 0; j < s.scanner.transitions.n; j++) {
                d_accepts_diff1 ~= tables_d_accepts_diff2[i,j].ptr;
            }
            tables.d_accepts_diff1[i] = d_accepts_diff1;
        }
        /* build scanner_block_hash */
        pscanner_block_hash = &scanner_block_hash[scanner_size(s)-1]; 
        ptrans_scanner_block_hash = &trans_scanner_block_hash[scanner_size(s)-1]; 
        for (j = 0; j < ss.n; j++) {
            if (!s.same_shifts) {
                for (k = 0; k < g.scanner_blocks; k++) {
                    vsblock[ivsblock].state_index = s.index;
                    vsblock[ivsblock].scanner_index = j;
                    vsblock[ivsblock].block_index = k;
                    vsblock[ivsblock].chars = 
                        &ss.v[j].chars[k * g.scanner_block_size];
                    vsblock[ivsblock].transitions = 
                        &ss.v[j].transition[k * g.scanner_block_size];
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
                        tables.d_scanner3[i,j,k] = d_scanner3.data;
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
                            tables.d_accepts_diff3[i,j,k] = d_accepts_diff3.data;
                        }
                    }
                }
                /* output shifts */
                if (ss.v[j].accepts.n) {
                    string tmp = i.to!string() ~ "." ~ j.to!string();
                    for (k = 0; k < ss.v[j].accepts.n; k++) {
                        Action *a = ss.v[j].accepts.v[k], aa;
                        if (ss.v[j].accepts.n == 1) {
                            if (a.temp_string)
                            {
                                continue;
                            }
                            a.temp_string = tmp;
                            aa = cast(Action*)set_add_fn(&shift_hash, a, &shift_fns);
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
                            if (k == ss.v[j].accepts.n - 1) {
                                tables_d_shift2[i,j] ~= null;
                            }
                        } else {
                            tables_d_shift2[i,j] ~= allTShifts[a.term.index];
                            if (k == ss.v[j].accepts.n - 1) {
                                tables_d_shift2[i,j] ~= null;
                            }
                        }
                    }
                }
            }
        }
    }
    for (i = 0; i < g.states.n; i++) {
        s = g.states.v[i];
        ss = &s.scanner.states;
        ivsblock = 0;
        if (ss.n && !s.same_shifts) {
            /* output scanner state transition tables */
            /* assume SB_uint8, 16, and 32 have same member offsets */
            assert((SB_uint8).sizeof == (SB_uint16).sizeof && (SB_uint16).sizeof == (SB_uint32).sizeof);
            SB_uint32[] d_scanner = [];
            pscanner_block_hash = &scanner_block_hash[scanner_size(s)-1]; 
            for (j = 0; j < ss.n; j++) {
                SB_uint32 sb;
                if (ss.v[j].accepts.n) {
                    Action* a = ss.v[j].accepts.v[0];
                    if (ss.v[j].accepts.n == 1) {
                        a = cast(Action*)set_add_fn(&shift_hash, a, &shift_fns);
                        sb.shift = tables_d_shift2.storage[a.temp_string].ptr;
                    } else
                        sb.shift = tables_d_shift2[i, j].ptr;
                }
                for (k = 0; k < g.scanner_blocks; k++) {
                    ScannerBlock vs;
                    vs.state_index = s.index;
                    vs.scanner_index = j;
                    vs.block_index = k;
                    vs.chars = &ss.v[j].chars[k * g.scanner_block_size];
                    vs.transitions = 
                        &ss.v[j].transition[k * g.scanner_block_size];
                    xv = &vs;
                    yv = cast(ScannerBlock*)set_add_fn(pscanner_block_hash, xv, &scanner_block_fns);
                    assert(yv != xv);
                    sb.scanner_block[k] = cast(uint*)tables.d_scanner3[yv.state_index, yv.scanner_index, yv.block_index].ptr;
                }

                d_scanner ~= sb;
            }
            tables.d_scanner1[i] = d_scanner;
            if (s.scan_kind != D_SCAN_LONGEST || s.trailing_context) {
                SB_trans_uint32 d_transition[];
                /* output scanner accepts diffs tables */
                ptrans_scanner_block_hash = 
                    &trans_scanner_block_hash[scanner_size(s)-1]; 
                for (j = 0; j < ss.n; j++) {
                    SB_trans_uint32 trans;
                    for (k = 0; k < g.scanner_blocks; k++) {
                        ScannerBlock vs;
                        vs.state_index = s.index;
                        vs.scanner_index = j;
                        vs.block_index = k;
                        vs.chars = &ss.v[j].chars[k * g.scanner_block_size];
                        vs.transitions = 
                            &ss.v[j].transition[k * g.scanner_block_size];
                        xv = &vs;
                        yv = cast(ScannerBlock*)set_add_fn(ptrans_scanner_block_hash, xv, 
                                &trans_scanner_block_fns);
                        assert(yv != xv);
                        trans.scanner_block[k] = cast(uint*)tables.d_accepts_diff3[ yv.state_index, yv.scanner_index,
                                    yv.block_index].ptr;
                    }
                    d_transition ~= trans;
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
    Vec!(intptr_t) vgoto;
    int i, j, x, again, nvalid_bytes, sym, lowest_sym;

    nvalid_bytes = ((g.productions.n + g.terminals.n) + 7) / 8;
    uint8 *goto_valid = cast(uint8 *)MALLOC(nvalid_bytes);
    vec_clear(&vgoto);
    for (i = 0; i < g.states.n; i++) {
        State *s = g.states.v[i];
        if (s.gotos.n) {
            /* check for goto on token */
            for (j = 0; j < s.gotos.n; j++)
                if (s.gotos.v[j].elem.kind == ELEM_TERM &&
                        s.gotos.v[j].elem.e.term.kind == TermKind.TERM_TOKEN)
                    s.goto_on_token = 1;
            /* find lowest goto, set valid bits */
            memset(goto_valid, 0, nvalid_bytes);
            lowest_sym = elem_symbol(g, s.gotos.v[0].elem);
            SET_BIT(goto_valid, lowest_sym);
            for (j = 1; j < s.gotos.n; j++) {
                sym = elem_symbol(g, s.gotos.v[j].elem);
                SET_BIT(goto_valid, sym);
                if (sym < lowest_sym)
                    lowest_sym = sym;
            }
            /* insert into vgoto */
            again = 1;
            while (again) {
                again = 0;
                for (j = 0; j < s.gotos.n; j++) {
                    x = elem_symbol(g, s.gotos.v[j].elem);
                    x -= lowest_sym;
                    while (vgoto.n <= x) {
                        int qq = 0;
                        vec_add(&vgoto, 0);
                        for (qq = 0; qq < vgoto.n; qq++)
                            if (vgoto.v[qq] == 239847234)
                                printf("wow...\n");
                    }
                    if (vgoto.v[x]) {
                        again = 1;
                        /* undo the damage */
                        for (--j;j >= 0;j--) {
                            x = elem_symbol(g, s.gotos.v[j].elem);
                            x -= lowest_sym;
                            vgoto.v[x] = 0;
                        }
                        lowest_sym--;
                        break;
                    } else
                        vgoto.v[x] = s.gotos.v[j].state.index + 1;
                }
            }
            s.goto_table_offset = lowest_sym;
            /* valid bits */
            ubyte[] d_goto_valid;
            for (j = 0; j < nvalid_bytes; j++)
                d_goto_valid ~= goto_valid[j];
            tables.d_goto_valid[i] = d_goto_valid;
        } else
            s.goto_table_offset = -int.max;
        /* reduce_actions */
        if (s.reduce_actions.n) {
            D_Reduction* d_reductions1[];
            for (j = 0; j < s.reduce_actions.n; j++)
                d_reductions1 ~= tables.reductions[original_reduction(s.reduce_actions.v[j].rule).index];
            tables.d_reductions1[i] = d_reductions1;
        }
        /* modified_reduce_actions */
        if (s.right_epsilon_hints.n) {
            D_RightEpsilonHint[] d_right_epsilon_hints;
            for (j = 0; j < s.right_epsilon_hints.n; j++) {
                D_RightEpsilonHint hint;
                hint.depth = cast(ushort)s.right_epsilon_hints.v[j].depth;
                hint.preceeding_state = cast(ushort)s.right_epsilon_hints.v[j].state.index;
                hint.reduction = tables.reductions[original_reduction(s.right_epsilon_hints.v[j].rule).index];
                d_right_epsilon_hints ~= hint;
            }
            tables.d_right_epsilon_hints1[i] = d_right_epsilon_hints;
        }
    }
    /* gotos */
    if (vgoto.n) {
        ushort d_gotos[];
        g.write_line += 1;
        for (j = 0; j < vgoto.n; j++) {
            if (vgoto.v[j] < 0 || vgoto.v[j] > 65535)
                d_fail("goto table overflow");
            d_gotos ~= cast(ushort)vgoto.v[j];
        }
        tables.d_gotos = d_gotos;
    } else {
        tables.d_gotos ~= 0;
    }
}

private int find_symbol(Grammar *g, const(char)[] s, int kind) {
    s = s.stripLeft();
        if (kind == D_SYMBOL_NTERM) {
            Production *p = lookup_production(g, s);
            if (p)
                return p.index;
        } else if (kind == D_SYMBOL_STRING) {
            int found = -1;
            for (int i = 0; i < g.terminals.n;i++)
                if (g.terminals.v[i].kind == TermKind.TERM_STRING &&
                        (g.terminals.v[i].term_name == s ||
                        (g.terminals.v[i].term_name.length == 0 &&
                          g.terminals.v[i].string_len == s.length &&
                          !strncmp(s.ptr, g.terminals.v[i].string_, s.length)))) {
                    if (found > 0) {
                        d_fail("attempt to find symbol for non-unique string '%s'\n",
                                g.terminals.v[i].string_);
                    } else
                        found = i;
                }
            if (found > 0)
                return found + g.productions.n;
        }
    return -1;
}

private int
find_symbol(Grammar *g, char *s, char *e, int kind) {
    if (e > s) {
        return find_symbol(g, s[0 .. e - s], kind);
    }
    return -1;
}

private void
buildReductions(Grammar *g, ref BuildTables tables) {
    int i, j, k, l, pmax;
    Production *p, pdefault;
    Rule *r, rdefault = null;

    pdefault = lookup_production(g, "_");
    if (pdefault) {
        rdefault = pdefault.rules.v[0];
    }
    for (i = 0; i < g.productions.n; i++) {
        p = g.productions.v[i];
        for (j = p.rules.n - 1; j >= 0; j--) {
            r = p.rules.v[j];
            for (k = 0; k < j; k++)
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
                    for (l = 0; l < r.pass_code.n; l++) {
                        if (!r.pass_code.v[l] && !p.rules.v[k].pass_code.v[l])
                            continue;
                        if (!r.pass_code.v[l] || !p.rules.v[k].pass_code.v[l])
                            goto Lcontinue;
                        if (r.pass_code.v[l].code != p.rules.v[k].pass_code.v[l].code)
                            goto Lcontinue;
                    }
                    r.same_reduction = p.rules.v[k];
                    break;
Lcontinue:;
                }
        }
        for (j = 0; j < p.rules.n; j++) {
            r = p.rules.v[j];
            if (r.same_reduction)
                continue;
            pmax = r.pass_code.n;
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
            red.npass_code = pmax;
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
    hash += strhashl(ta.string_, ta.string_len);
    if (sa.v[i].rule)
      hash += sa.v[i].rule.prod.index * 10007;
  }
  return hash;
}

extern(C) private int
er_hint_cmp_fn(State *a, State *b, hash_fns_t *fns) {
  int i;
  VecHint *sa = &a.error_recovery_hints, sb = &b.error_recovery_hints;
  Term *ta, tb;
  if (sa.n != sb.n)
    return 1;
  for (i = 0; i < sa.n; i++) {
    ta = sa.v[i].rule.elems.v[sa.v[i].rule.elems.n - 1].e.term;
    tb = sb.v[i].rule.elems.v[sb.v[i].rule.elems.n - 1].e.term;
    if (sa.v[i].depth != sb.v[i].depth ||
	strcmp(ta.string_, tb.string_) ||
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
    int i, j;
    State *s;
    Term *t;
    State *h;
    char *ss;

    if (g.states.n) {
        for (i = 0; i < g.states.n; i++) {
            s = g.states.v[i];
            if (s.error_recovery_hints.n) {
                h = cast(State*)set_add_fn(er_hash, s, &er_hint_hash_fns);
                if (h == s) {
                    D_ErrorRecoveryHint d_error_recovery_hints[];
                    for (j = 0; j < s.error_recovery_hints.n; j++) {
                        t = s.error_recovery_hints.v[j].rule.elems.v[
                            s.error_recovery_hints.v[j].rule.elems.n - 1].e.term;
                        ss = escape_string(t.string_);
                        D_ErrorRecoveryHint hint;
                        hint.depth = cast(ushort)s.error_recovery_hints.v[j].depth;
                        hint.symbol = cast(ushort)s.error_recovery_hints.v[j].rule.prod.index;
                        hint.str = ss[0 .. strlen(ss)].idup;
                        d_error_recovery_hints ~= hint;
                        if (j != s.error_recovery_hints.n - 1)
                            g.write_line += 1;
                    }
                    tables.d_error_recovery_hints1[i] = d_error_recovery_hints;
                }
            }
        }
    }
}

private void
buildStateData(Grammar *g, ref BuildTables tables, VecState *er_hash) {
    int i;
    State *s, h, shifts;

    D_State[] d_states;
    if (g.states.n) {
        for (i = 0; i < g.states.n; i++) {
            s = g.states.v[i];
            shifts = s.same_shifts ? s.same_shifts : s;
            D_State state;
            if (s.gotos.n)
                state.goto_valid = tables.d_goto_valid[i].ptr;
            else
                state.goto_valid = null;
            state.goto_table_offset = s.goto_table_offset;
            if (s.reduce_actions.n) {
                state.reductions.n = s.reduce_actions.n;
                state.reductions.v = tables.d_reductions1[i].ptr;
            } else {
                state.reductions.n = 0;
                state.reductions.v = null;
            }
            if (s.right_epsilon_hints.n) {
                state.right_epsilon_hints.n = s.right_epsilon_hints.n;
                state.right_epsilon_hints.v = tables.d_right_epsilon_hints1[i].ptr;
            } else {
                state.right_epsilon_hints.n = 0;
                state.right_epsilon_hints.v = null;
            }
            if (s.error_recovery_hints.n) {
                h = cast(State*)set_add_fn(er_hash, s, &er_hint_hash_fns);
                state.error_recovery_hints.n = s.error_recovery_hints.n;
                state.error_recovery_hints.v = tables.d_error_recovery_hints1[h.index].ptr;
            } else {
                state.error_recovery_hints.n = 0;
                state.error_recovery_hints.v = null;
            }
            if (s.shift_actions.n || s.scanner_code || (g.scanner.code && s.goto_on_token))
                state.shifts = 1;
            else
                state.shifts = 0;
            if (s.scanner.states.n) {
                state.scanner_table = tables.d_scanner1[shifts.index].ptr;
            } else {
                state.scanner_table = null;
            }
            state.scanner_size = cast(ubyte)scanner_size(s);
            state.accept = s.accept ? 1 : 0;
            state.scan_kind = cast(ubyte)s.scan_kind;
            if ((shifts.scan_kind != D_SCAN_LONGEST || shifts.trailing_context)
                    && shifts.scanner.states.n) {
                state.transition_table = tables.d_transition1[shifts.index].ptr;
            } else {
                state.transition_table = null;
            }
            if ((shifts.scan_kind != D_SCAN_LONGEST || shifts.trailing_context)
                    && shifts.scanner.states.n)
                state.accepts_diff = tables.d_accepts_diff1[shifts.index].ptr;
            else
                state.accepts_diff = null;
            if (s.reduces_to)
                state.reduces_to = s.reduces_to.index;
            else
                state.reduces_to = -1;
            d_states ~= state;
        }
        tables.d_states = d_states;
    } else {
            d_fail("no states\n");
    }
}

bool is_EBNF(uint _x)
{
    return _x == InternalKind.INTERNAL_CONDITIONAL || _x == InternalKind.INTERNAL_STAR || _x == InternalKind.INTERNAL_PLUS;
}

private int d_internal_values[] = [D_SYMBOL_NTERM, D_SYMBOL_EBNF, D_SYMBOL_INTERNAL];
private int d_symbol_values[] = [ 
  D_SYMBOL_STRING, D_SYMBOL_REGEX, D_SYMBOL_CODE, D_SYMBOL_TOKEN ];

private void
buildSymbolData(Grammar *g, ref BuildTables tables) {
    int i;
    D_Symbol d_symbols[];
    for (i = 0; i < g.productions.n; i++) {
        int state = -1, internal_index;
        if (!g.productions.v[i].internal && g.productions.v[i].elem)
            state = g.productions.v[i].state.index;
        internal_index = g.productions.v[i].internal ? (is_EBNF(g.productions.v[i].internal) ? 2 : 1) : 0;
        D_Symbol sym;
        sym.kind = d_internal_values[internal_index];
        sym.name = g.productions.v[i].name;
        sym.start_symbol = state;
        d_symbols ~= sym;
    }
    for (i = 0; i < g.terminals.n; i++) {
        char *s = escape_string(g.terminals.v[i].string_); /* so it is a string */
        const char *name = g.terminals.v[i].term_name.length ? g.terminals.v[i].term_name.toStringz() : s;
        int symbol_index = g.terminals.v[i].kind;
        D_Symbol sym;
        sym.kind = d_symbol_values[symbol_index];
        sym.name = name[0 .. strlen(name)].idup;
        d_symbols ~= sym;
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

    result.nstates = g.states.n;
    result.state = tables.d_states.ptr;
    result.goto_table = tables.d_gotos.ptr;
    auto ws = lookup_production(g, "whitespace");
    if (ws) result.whitespace_state = ws.state.index;
    result.nsymbols = g.productions.n + g.terminals.n;
    result.symbols = tables.d_symbols.ptr;
    result.npasses = g.passes.n;
    result.passes = tables.d_passes.ptr;
    result.save_parse_tree = g.save_parse_tree;
    return result;
}

struct TableMap(T, int dimention = 1)
{
    T[string] storage;

    private string keyWithArgs(int i, int j, int k, int l) pure
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

    ref T opIndex(int i, int j = 0, int k = 0, int l = 0)
    {
        string key = keyWithArgs(i,j,k,l);
        assert(key in storage);
        return storage[key];
    }

    void opIndexAssign(T val, int i, int j = 0, int k = 0, int l = 0)
    {
        string key = keyWithArgs(i,j,k,l);
        assert(key !in storage);
        storage[key] = val;
    }
}

struct BuildTables
{
    TableMap!(D_Shift**[], 1) d_accepts_diff1;
    TableMap!(ubyte[], 3) d_scanner3;
    TableMap!(ubyte[], 3) d_accepts_diff3;
    D_Symbol d_symbols[];

    D_Reduction*[uint] reductions;
    TableMap!(SB_uint32[], 1) d_scanner1;
    TableMap!(SB_trans_uint32[], 1) d_transition1;
    TableMap!(ubyte[], 1) d_goto_valid;
    TableMap!(D_Reduction*[], 1) d_reductions1;
    TableMap!(D_RightEpsilonHint[], 1) d_right_epsilon_hints1;
    TableMap!(D_ErrorRecoveryHint[], 1) d_error_recovery_hints1;
    ushort d_gotos[];

    D_State[] d_states;
    D_Pass[] d_passes;

    D_ReductionCode spec_code;
    D_ReductionCode final_code;
}

