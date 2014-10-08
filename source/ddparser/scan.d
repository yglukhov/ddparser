module ddparser.scan;


import ddparser.dparse;
import ddparser.parse;
import ddparser.dparse_tables;
import ddparser.util;
import core.stdc.string;
import std.stdio;

struct ShiftResult
{
    SNode* snode;
    const(D_Shift)* shift;
    d_loc_t	loc;
}

private void do_smth(State)(ref d_loc_t loc, ref d_loc_t last_loc,
                ref int nresults, 
                ref D_Shift** shift, const ref D_State parse_state, ShiftResult[] results)
{
    /* all matches */
    auto st = cast(SB_!(State)*)parse_state.scanner_table;
    auto tst = cast(SB_trans!(State)*)parse_state.transition_table;
    State state, last, prev;

    char *s = loc.s;
    int col = loc.col, line = loc.line;

    while (true)
    {
        uint8 c = cast(uint8)*s++;
        uint32 sb = c >> SCANNER_BLOCK_SHIFT;
        uint32 so = c & SCANNER_BLOCK_MASK;
        state = st[state].scanner_block[sb][so];
        if (!state) break;

        --state;

        if (prev && parse_state.accepts_diff) {
            const D_Shift*[] shift_diff = parse_state.accepts_diff[tst[prev].scanner_block[sb][so]];
            foreach(sd; shift_diff) {
                results[nresults].loc = loc;
                results[nresults++].shift = sd;
            }
        }
        prev = state;
        if (c == '\n') { line++; col = 0; } else col++;
        loc.s = s; loc.line = line; loc.col = col;
        if (st[state].shift) {
            last = state;
            last_loc = loc;
        }
    }
    shift = st[last].shift;
}

int
scan_buffer(d_loc_t loc, const ref D_State parse_state, ShiftResult[] results) {
    d_loc_t last_loc = loc;
    int nresults = 0;
    D_Shift **shift = null;

    switch (parse_state.scanner_size) {
        case 1:
            do_smth!ubyte(loc, last_loc, nresults, shift, parse_state, results);
            break;
        case 2:
            do_smth!ushort(loc, last_loc, nresults, shift, parse_state, results);
            break;
        case 4:
            do_smth!uint(loc, last_loc, nresults, shift, parse_state, results);
            break;
        default:
    }
    if (shift) {
        for (; *shift; shift++) {
            results[nresults].loc = last_loc;
            results[nresults++].shift = *shift;
        }
    }
    if (nresults) {
        bool longest = false;
        char *end = results[nresults-1].loc.s;
        if (parse_state.scan_kind == D_SCAN_LONGEST)
            longest = true;

        int i = 0;
        if (parse_state.scan_kind == D_SCAN_MIXED) {
            for (i = nresults - 1; i >= 0; i--) {
                if (results[i].loc.s < end)
                    break;
                if (results[i].shift.shift_kind == D_SCAN_LONGEST)
                    longest = true;
            }
        }

        if (longest) {
            /* keep only 'longest' */
            i = 0;
            for (int j = 0; j < nresults; j++) {
                if (results[j].loc.s == end || results[j].shift.shift_kind == D_SCAN_TRAILING) {
                    if (i != j)
                        results[i] = results[j];
                    i++;
                }
            }
            nresults = i;
        } else if (parse_state.scan_kind == D_SCAN_MIXED) {
            /* only keep non-longest */
            for (int j = i; j >= 0; j--)
                if (results[j].shift.shift_kind != D_SCAN_LONGEST) {
                    if (i != j)
                        results[i] = results[j];
                    i--;
                }
            nresults = nresults - i - 1;
            if (i != -1)
                for (int j = 0; j < nresults; ++j)
                    results[j] = results[i + j + 1];
        }
    }
    return nresults;
}

