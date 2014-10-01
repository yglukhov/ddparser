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

/+

void myfprintf(FILE *f, const char *format, ...) {
  va_list ap;
  va_start(ap, format);

  if(!f)
    d_warn("trying to write code to binary file");
  else
    vfprintf(f, format, ap);
  va_end(ap);
}

alias fprintf = myfprintf;

struct OffsetEntry {
  char *name;
  int len;
  int offset;
}

alias OffsetEntrySet = Vec!(OffsetEntry*);
alias CharPtrVec = Vec!(char*);

struct Buf {
  char *start;
  char *cur;
  int len;
}

struct File {
  int binary;
  FILE *fp;
  ubyte *cur_str;
  ubyte **str;
  uint *str_len;
  Buf tables;
  Buf strings;
  OffsetEntrySet entries;
  CharPtrVec relocations;
  CharPtrVec str_relocations;
  int first_member;
  int array_length;
  int n_elems;
  int elem_size;
  int d_parser_tables_loc;
}

OffsetEntry null_entry = {"null", ("null").sizeof-1, -1};
OffsetEntry spec_code_entry = {"#spec_code", ("#spec_code").sizeof-1, -2};
OffsetEntry final_code_entry = {"#final_code", ("#final_code").sizeof-1, -3};

uint32 
offset_hash_fn(OffsetEntry *entry, hash_fns_t* fn) {
  fn = fn;
  return strhashl(entry.name, entry.len);
}

int 
offset_cmp_fn(OffsetEntry *a, OffsetEntry *b, hash_fns_t* fn) {
  fn = fn;
  return strcmp(a.name, b.name);
}

hash_fns_t 
offset_fns = {
  cast(hash_fn_t)offset_hash_fn,
  cast(cmp_fn_t)offset_cmp_fn,
  {0, 0}
};

static void 
write_chk(const void* ptr, size_t size, size_t nmemb, File *file) {
  if (file.fp) {
    if (fwrite(ptr, size, nmemb, file.fp) != nmemb) 
      d_fail("error writing binary file\n");
  } else {
    memcpy(file.cur_str, ptr, size * nmemb);
    file.cur_str += size * nmemb;
  }
}

static void
save_binary_tables(File *file) {
  int i;
  BinaryTablesHead tables;
  uint len;
  
  tables.n_relocs = file.relocations.n;
  tables.n_strings = file.str_relocations.n;
  tables.d_parser_tables_loc = file.d_parser_tables_loc;
  tables.tables_size = file.tables.cur - file.tables.start;
  tables.strings_size = file.strings.cur - file.strings.start;

  len = (BinaryTablesHead).sizeof +
    tables.tables_size + tables.strings_size +
    (file.relocations.n * (void*).sizeof) +
    (file.str_relocations.n * (void*).sizeof);

  if (file.str) {
    file.cur_str = *file.str = cast(ubyte*)MALLOC(len);
    *file.str_len = len;
  }

  write_chk(&tables, (BinaryTablesHead).sizeof, 1, file);
  write_chk(file.tables.start, (char).sizeof, tables.tables_size, file);
  write_chk(file.strings.start, (char).sizeof, tables.strings_size, file);
  for (i=0; i < file.relocations.n; i++)
    write_chk(&file.relocations.v[i], (void*).sizeof, 1, file);
  for (i=0; i < file.str_relocations.n; i++)
    write_chk(&file.str_relocations.v[i], (void*).sizeof, 1, file);    
}

static void
free_tables(File *f) {
  int i;
  if (f.tables.start)
    FREE(f.tables.start);
  if (f.strings.start)
    FREE(f.strings.start);
  vec_free(&f.str_relocations);
  vec_free(&f.relocations);
  for (i=0; i<f.entries.n; i++) {
    if (f.entries.v[i]) {
      FREE(f.entries.v[i].name);
      FREE(f.entries.v[i]);
      f.entries.v[i] = 0;
    }
  }
  vec_free(&f.entries);
}

static void
init_buf(Buf *buf, int initial_size) {
  buf.len = initial_size;
  buf.start = MALLOC(buf.len); 
  memset(buf.start, 0, buf.len);
  buf.cur = buf.start;
}

static void
file_init(File *file, int binary, FILE* fp, ubyte **str, uint *str_len) {
  memset(file, 0, (File).sizeof);
  file.binary = binary;
  file.fp = fp;
  file.str = str;
  file.str_len = str_len;
  if (binary) {
    init_buf(&file.tables, 1024);
    init_buf(&file.strings, 1024);
  }
  vec_clear(&file.relocations);
  vec_clear(&file.entries);
  vec_clear(&file.str_relocations);
}

static void
make_room_in_buf(Buf *buf, size_t size) {
  while (buf.cur + size > buf.start + buf.len) {
    int cur = buf.cur - buf.start;
    buf.len = buf.len*2 + 1;
    buf.start = REALLOC(buf.start, buf.len);
    buf.cur = buf.start + cur;
    memset(buf.cur, 0, buf.len - (buf.cur - buf.start));
  }
}

static void
new_offset(File *fp, char *name) {
  OffsetEntry *entry = MALLOC((OffsetEntry).sizeof);
  memset(entry, 0, (OffsetEntry).sizeof);
  entry.name = name;
  entry.offset = fp.tables.cur - fp.tables.start;
  entry.len = strlen(name);
  set_add_fn(&fp.entries, entry, &offset_fns);
}

static uintptr_t
make_string(File *fp, const char *name) {
  intptr_t size = strlen(name)+1;
  Buf *buf = &fp.strings;
  char *dest;
  make_room_in_buf(buf, size);
  dest = buf.cur;
  strcpy(dest, name);
  buf.cur += size;
  return dest - buf.start;
}

static OffsetEntry *
search_for_offset(File *fp, char *name) {
  uint32 tt = strhashl(name, strlen(name));
  OffsetEntrySet *v = &fp.entries;
  int j, n = v.n;
  uint i;
  if (n) {
    uint h = tt % n;
    for (i = h, j = 0; 
	 i < v.n && j < SET_MAX_SEQUENTIAL; 
	 i = ((i + 1) % n), j++) 
    {
      if (!v.v[i]) {
	assert(0);
	return 0;
      } else {
	if (!strcmp(v.v[i].name, name))
	  return v.v[i];
      }
    }
  }
  assert(0);
  return 0;
}

static OffsetEntry *
get_offset(File *fp, char* name, ...) {
  int n;
  char buf[256];
  va_list ap;
  va_start(ap, name);
  n = vsnprintf(buf, (buf).sizeof, name, ap);
  va_end(ap);
  assert(n < 256 && n >= 0);
  return search_for_offset(fp, buf);
}

static char*
make_name(char* fmt, ...) {
  int n;
  char *h_buf;
  char buf[256];
  va_list ap;
  va_start(ap, fmt);
  n = vsnprintf(buf, (buf).sizeof, fmt, ap);
  va_end(ap);
  assert(n < 256 && n >= 0);
  h_buf = MALLOC(n+1);
  strcpy(h_buf, buf);
  return h_buf;
}

static void
print_no_comma(File *fp, char *str) {
  if (!fp.binary) {
    fprintf(fp.fp, "%s", str);
    fp.first_member = 1;
  }
}

static void 
print(File *fp, char *str) {
  if (!fp.binary) {
    fprintf(fp.fp, "%s", str);
  }
}

/* 
struct:
  start_struct
  repeat: add_struct_X_member
  end_struct

array of primitives:
  start_array
  repeat: add_array_member or add_array_ptr_member
  end_array

array of structs:
  start_array
  repeat:
    start_struct_in_array
    repeat: add_struct_X_member
    end_struct_in_array
  end_array
*/
+/
/* void start_struct(T)(File* file, char* name, char* whitespace) */
/* { */
/*     start_struct_fn(file, T.sizeof, T.stringof.ptr, name, whitespace); */
/* } */
/*  */
/* void start_array(T)(File* file, char* name, char* length_str, int length_data, char* whitespace) */
/* { */
/*     start_array_fn(file, T.sizeof, "".ptr, T.stringof.ptr, name, length_str, length_data, whitespace); */
/* } */
/*  */
/* void add_struct_member(struct_type, D, string member_name)(File* file, string format, D data) */
/* { */
/*     if ((file).binary) {  */
/*         mixin("((struct_type*)((file).tables.cur))." ~ member_name ~ " = data;"); */
/*     } else {  */
/*         handle_comma(file);  */
/*         fprintf((file).fp, #format, data);  */
/*     } */
/* } */

/* #define add_struct_member(file, struct_type, format, data, member_name) do { \ */
/*   if ((file).binary) { \ */
/*     ((struct_type*)((file).tables.cur)).member_name = data; \ */
/*   } else { \ */
/*      handle_comma(file); \ */
/*      fprintf((file).fp, #format, data); \ */
/*   }} while(0) */
/+#define add_struct_const_member(file, struct_type, string, value, member_name) do { \
  if ((file).binary) { \
     ((struct_type*)((file).tables.cur)).member_name = value; \
  } else { \
     handle_comma(file); \
     fprintf((file).fp, string); \
  }} while(0)
#define add_struct_str_member(file, struct_type, string, member_name) \
  add_struct_str_member_fn(file, (char**)&((struct_type*)(file).tables.cur).member_name, string)
#define add_struct_ptr_member(file, struct_type, ampersand, entry, member_name) \
  add_struct_ptr_member_fn(file, (void**)&((struct_type*)(file).tables.cur).member_name, entry, ampersand "%s")
#define add_array_member(file, type, format, data, last) do {\
if ((file).binary) {\
    add_array_member_internal(file);\
    *((type*)((file).tables.cur)) = data;\
    (file).tables.cur += (file).elem_size;\
  } else {\
    fprintf((file).fp, #format "%s", data, last ? "" : ",");\
  }} while(0)
#define add_array_ptr_member(file, type, ampersand, entry, last) \
  add_array_ptr_member_fn(file, entry, ampersand "%s%s", last)
#define end_struct(file, type, whitespace) end_struct_fn(file, (type).sizeof, whitespace)

static void 
add_array_member_internal(File *fp) {
  fp.n_elems++;
  make_room_in_buf(&fp.tables, fp.elem_size);
}

static void 
handle_comma(File *file) {
  if (!file.first_member)
    fprintf(file.fp, ", ");
  file.first_member = 0;
}

static void
start_array_internal(File *fp, int length, int size) {
  fp.array_length = length;
  fp.n_elems = 0;
  fp.elem_size = size;
}

static void 
start_struct_fn(File *fp, int size, char *type_str, char *name, char *whitespace) {
  new_offset(fp, name);
  fp.first_member = 1;
  if (fp.binary) {
    make_room_in_buf(&fp.tables, size);
  } else {
    fprintf(fp.fp, "%s %s = {%s", type_str, name, whitespace);
  }
}

static void 
start_struct_in_array(File *fp) {
  fp.first_member = 1;
  if (fp.binary) {
    add_array_member_internal(fp);
  } else {
    fprintf(fp.fp, "{");
  }
}

static void 
start_array_fn(File *fp, int type_size, char *type_prefix, char *type_str, 
		    char *name, char *length_str, int length_data, char *whitespace) {
  new_offset(fp, name);
  if (fp.binary) {
    start_array_internal(fp, length_data, type_size);
  } else {
    if (length_data == 0)
      fprintf(fp.fp, "%s%s %s[] = {%s", type_prefix, type_str, name, whitespace);
    else if(strlen(length_str) == 0)
      fprintf(fp.fp, "%s%s %s[%d] = {%s", type_prefix, type_str, name, length_data, whitespace);
    else 
      fprintf(fp.fp, "%s%s %s[%s] = {%s", type_prefix, type_str, name, length_str, whitespace);
  }
}

static void 
add_struct_str_member_fn(File *fp, char **dest, const char *str) {
  if (fp.binary) {
    *dest = (char*)make_string(fp, str);
    vec_add(&fp.str_relocations, (void*)((char*)dest - fp.tables.start));
  } else {
    if (!fp.first_member)
      fprintf(fp.fp, ", ");
    fprintf(fp.fp, "\"%s\"", str);
  }
  fp.first_member = 0;
}

static void
add_struct_ptr_member_fn(File *fp, void **dest, OffsetEntry *oe, char *format) {
  if (fp.binary) {
    *dest = (void*)(uintptr_t)oe.offset;
    vec_add(&fp.relocations, (void*)((char*)dest - fp.tables.start));
  } else {
    if (*format == '&' && strcmp(oe.name, "null") == 0)
      format++;
    if (!fp.first_member)
      fprintf(fp.fp, ", ");
    fprintf(fp.fp, format, oe.name);
  }
  fp.first_member = 0;
}

static void
add_array_ptr_member_fn(File *fp, OffsetEntry *oe, char *format, int last) {
  if (fp.binary) {
    add_array_member_internal(fp);
    *(void**)fp.tables.cur = (void*)(intptr_t)oe.offset;
    vec_add(&fp.relocations, (void*)(fp.tables.cur - fp.tables.start));
    fp.tables.cur += fp.elem_size;
  } else {
    if (*format == '&' && strcmp(oe.name, "null") == 0)
      format++;
    fprintf(fp.fp, format, oe.name, last ? "" : ",");
  }
}

typedef void (*CopyFuncType)(void*,int);
static void
add_array_member_fn(File *file, CopyFuncType copy, char *format, uint data, int last) {
  if (file.binary) {
    add_array_member_internal(file);
    copy((void*)(file.tables.cur), data);
    file.tables.cur += file.elem_size;
  } else {
    fprintf((file).fp, format, data);
    if(!last)
      fprintf(file.fp, ", ");
  }
}

static void 
end_struct_fn(File *fp, int size, char *whitespace) {
  if (fp.binary) {
    fp.tables.cur += size;
  } else {
    fprintf(fp.fp, "};%s", whitespace);
  }
}

static void
end_struct_in_array(File *fp, char *last) {
  if (fp.binary) {
    fp.tables.cur += fp.elem_size;
  } else {
    fprintf(fp.fp,"}%s", last);
  }
}

static void
end_array(File *fp, char *whitespace) {
  if (fp.binary) {
    if (fp.array_length != 0) {
      int remaining = (fp.array_length - fp.n_elems)*fp.elem_size;
      if (remaining) {
	make_room_in_buf(&fp.tables, remaining);
	memset(fp.tables.cur, 0, remaining);
	fp.tables.cur += remaining;
      }
    }
  } else {
    fprintf(fp.fp, "};%s", whitespace);
  }
}
+/
struct ScannerBlock { 
  int state_index; 
  int scanner_index; 
  int block_index; 
  ScanState **chars; 
  ScanStateTransition **transitions; 
}

alias VecScannerBlock = Vec!(ScannerBlock*);
alias VecState = Vec!(State*);

static int
scanner_size(State *s) {
  if (s.scanner.states.n < 255 && s.scanner.transitions.n < 255)
    return 1;
  if (s.scanner.states.n < 32384 && s.scanner.transitions.n < 32384)
    return 2;
  return 4;
}
/+
#define copy_func(name, type) static void name(void *dest, int data) { (*(type*)(dest)) = (data); }
copy_func(unsigned_char_copy, unsigned char);
copy_func(unsigned_short_copy, unsigned short);
copy_func(unsigned_int_copy, unsigned int);

static CopyFuncType 
get_copy_func(int i) {
  switch (i) {
  case 1: return unsigned_char_copy;
  case 2: return unsigned_short_copy;
  case 4: return unsigned_int_copy;
  default: d_fail("bad case"); return 0;
  }
}
static char *
make_type(int i) {
  switch (i) {
    case 1: return "unsigned char";
    case 2: return "unsigned short";
    case 4: return "unsigned int";
    default: d_fail("bad case"); return "";
  }
}

static char *
scanner_type(State *s) {
  return make_type(scanner_size(s));
}

static char *
make_u_type(int i) {
  switch (i) {
    case 1: return "uint8";
    case 2: return "uint16";
    case 4: return "uint32";
    default: d_fail("bad case"); return "";
  }
}

static char *
scanner_u_type(State *s) {
  return make_u_type(scanner_size(s));
}
+/

extern(C) static uint32
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

extern(C) static int
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

extern(C) static uint32
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

extern(C) static int
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

extern(C) static uint32
shift_hash_fn(Action *sa, hash_fns_t *fns) {
  return sa.term.index + (sa.kind == ActionKind.ACTION_SHIFT_TRAILING ? 1000000 : 0);
}

extern(C) static int
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


static void
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
    //print(fp, "\n");
    //g.write_line++;
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
            //start_array(fp, D_Shift *, make_name("d_accepts_diff_%d_%d_%s", i, j, tag), "", 0, "");
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
            //start_array(fp, D_Shift **, make_name("d_accepts_diff_%d_%s", i, tag), "", 0, "\n");
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
                //start_struct_in_array(fp);
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
                /* start_array_fn(fp, (SB_trans_uint8).sizeof, "SB_trans_", scanner_u_type(s),  */
                /*         make_name("d_transition_%d_%s", i, tag), "", ss.n, "\n"); */
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
                        /* add_struct_ptr_member(fp, SB_trans_uint8, "",  */
                        /*         get_offset(fp, "d_accepts_diff_%d_%d_%d_%s",  */
                        /*             yv.state_index, yv.scanner_index, */
                        /*             yv.block_index, tag), scanner_block[k]);	     */
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

static void
buildGotoData(Grammar *g, ref BuildTables tables) {
    Vec!(intptr_t) vgoto;
    uint8 *goto_valid = null;
    int i, j, x, again, nvalid_bytes, sym, lowest_sym;

    nvalid_bytes = ((g.productions.n + g.terminals.n) + 7) / 8;
    goto_valid = cast(uint8 *)MALLOC(nvalid_bytes);
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

/+
static void
write_scanner_code(File *file, Grammar *g, char *tag) {
  int i, j, l;
  Action *a;
  State *s;
  FILE *fp = file.fp;

  for (i = 0; i < g.states.n; i++) {
    s = g.states.v[i];
    for (j = 0; j < s.shift_actions.n; j++) {
      a = s.shift_actions.v[j];
      if (a.kind == ACTION_SHIFT && a.term.kind == TERM_CODE) {
	if (!s.scanner_code) {
	  s.scanner_code = 1;
	  new_offset(file, make_name("d_scan_code_%d_%s", i, tag));
	  fprintf(fp, "int d_scan_code_%d_%s(d_loc_t *loc,"
		  "unsigned short *symbol, int *term_priority,"
		  "unsigned char *op_assoc, int *op_priority) {\n"
		  "  int res;\n",
		  i, tag);
	  g.write_line += 1;
	}
	fprintf(fp, "  if ((res = ");
	l = strlen(a.term.string);
	if (a.term.string[l - 1] == ')') {
	  fwrite(a.term.string, l - 1, 1, fp);
	  fprintf(fp, ", ");
	} else
	  fprintf(fp, "%s(", a.term.string);
	  fprintf(fp, "loc, op_assoc, op_priority))) {\n"
		"    *symbol = %d;\n"
		"    *term_priority = %d;\n"
		"    return res;\n"
		"  }\n",
		a.term.index + g.productions.n, 
		a.term.term_priority);
	g.write_line += 1;
      }
    }
    if (s.scanner_code) {
      fprintf(fp, "  return 0;\n}\n\n");
      g.write_line += 3;
    }
  }
}
+/

static int
find_symbol(Grammar *g, char *s, char *e, int kind) {
    while (*s && isspace_(*s)) s++;
    if (e > s) {
        if (kind == D_SYMBOL_NTERM) {
            Production *p = lookup_production(g, s, cast(int)(e-s));
            if (p)
                return p.index;
        } else if (kind == D_SYMBOL_STRING) {
            int found = -1;
            for (int i = 0; i < g.terminals.n;i++)
                if (g.terminals.v[i].kind == TermKind.TERM_STRING &&
                        ((g.terminals.v[i].term_name &&
                          strlen(g.terminals.v[i].term_name) == e-s &&
                          !strncmp(s, g.terminals.v[i].term_name, e-s)) ||
                         (!g.terminals.v[i].term_name &&
                          g.terminals.v[i].string_len == (e-s) &&
                          !strncmp(s, g.terminals.v[i].string, e-s)))) {
                    if (found > 0) {
                        d_fail("attempt to find symbol for non-unique string '%s'\n",
                                g.terminals.v[i].string);
                    } else
                        found = i;
                }
            if (found > 0)
                return found + g.productions.n;
        }
    }
    return -1;
}
/+
static void
write_code(FILE *fp, Grammar *g, Rule *r, char *code,
		char *fname, int line, char *pathname) 
{
  char *c;

  if (!fp) {
    d_warn("trying to write code to binary file");
    return;
  }
  if (g.write_line_directives) {
    fprintf(fp, "#line %d \"%s\"\n", line, pathname);
    g.write_line++;
  }
  fprintf(fp, "%s{ ", fname);
  c = code;
  int in_string = 0;
  while (*c) {
    if (*c != '\\') {
      if (c[1] == '\'' || c[1] == '"') {
        if (in_string == c[1]) in_string = 0; else if (!in_string) in_string = c[1];
      }
    }
    if (!in_string && *c == '/') {
      // pass through c++ style comments
      if (c[1] == '/') {
        while (*c && *c != '\n') {
          fputc(*c, fp); 
          c++;
        }
      } else if (c[1] == '*') {
        while (*c && *c != '*' && c[1] != '\\') {
          fputc(*c, fp); 
          c++;
        }
        if (*c) {
          fputc(*c, fp); 
          c++;
          fputc(*c, fp); 
          c++;
        }
      }
    }
    if (*c == '\n')
      g.write_line++;
    if (!in_string && *c == '$') {
      c++;
      if (*c == '#') {
	c++;
	if (isdigit_(*c)) {
	  int n = atoi(c);
	  fprintf(fp, "(d_get_number_of_children((D_PN(_children[%d], _offset))))", n);
	  if (n > r.elems.n-1)
	    d_fail("$nXXXX greater than number of children at line %d", line);
	  while (isdigit_(*c)) c++;
	} else
	  fprintf(fp, "(_n_children)");
      } else if (*c == 'g') {
	fprintf(fp, "(D_PN(_ps, _offset).globals)");
	c++;
      } else if (*c == 'n') {
	++c;
	if (isdigit_(*c)) {
	  int n = atoi(c);
	  fprintf(fp, "(*(D_PN(_children[%d], _offset)))", n);
	  if (n > r.elems.n-1)
	    d_fail("$nXXXX greater than number of children at line %d", line);
	  while (isdigit_(*c)) c++;
	} else 
	  fprintf(fp, "(*(D_PN(_ps, _offset)))");
      } else if (*c == '$') {
	fprintf(fp, "(D_PN(_ps, _offset).user)");
	c++;
      } else if (isdigit_(*c)) {
	int n = atoi(c);
	fprintf(fp, "(D_PN(_children[%d], _offset).user)", n);
	while (isdigit_(*c)) c++;
      } else if (*c == '{') {
	char *e = ++c, *a;
	while (*e && *e != '}' && !isspace_(*e)) e++;
	a = e;
	if (isspace_(*a)) a++;
	while (*a && *a != '}') a++;
	if (!*a)
	  d_fail("unterminated ${...} at line %d", line);
	if (STREQ(c, e-c, "child")) {
	  char xx[2][4096], *x, *y;
	  int i = 0;
	  *xx[0] = 0; *xx[1] = 0;
	  while (*e != '}') {
	    char *ss = e, *n;
	    x = xx[i];
	    y = xx[!i];
	    while (*e && *e != '}' && *e != ',') e++;
	    if (!*e || ss == e)
	      d_fail("bad ${...} at line %d", line);
	    n = dup_str(ss, e);
	    if (!*y)
	      sprintf(x, "(D_PN(_children[%s], _offset))", n);
	    else
	      sprintf(x, "d_get_child(%s, %s)", y, n);
	    if (*e == ',') e++;
	    if (isspace_(*e)) e++;
	    i = !i;
	  }
	  if (!xx[!i])
	    d_fail("empty ${child } at line %d", line);
	  fprintf(fp, "%s", xx[!i]);
	} else if (STREQ(c, e-c, "reject")) {
	  fprintf(fp, " return -1 ");
	} else if (STREQ(c, e-c, "free_below")) {
	  fprintf(fp, " free_D_ParseTreeBelow(_parser, (D_PN(_ps, _offset)))");
	} else if (STREQ(c, e-c, "scope")) {
	  fprintf(fp, "(D_PN(_ps, _offset).scope)");
	} else if (STREQ(c, e-c, "parser")) {
	  fprintf(fp, "_parser");
	} else if (STREQ(c, e-c, "nterm")) {
	  fprintf(fp, "%d", find_symbol(g, e, a, D_SYMBOL_NTERM));
	} else if (STREQ(c, e-c, "string")) {
	  fprintf(fp, "%d", find_symbol(g, e, a, D_SYMBOL_STRING));
	} else if (STREQ(c, e-c, "pass")) {
	  D_Pass *p = find_pass(g, e, a);
	  if (!p)
	    d_fail("unknown pass '%s' line %d", dup_str(e, a), line);
	  fprintf(fp, "%d", p.index);
	} else
	  d_fail("bad $ escape in code line %u\n", line);
	c = a + 1;
      } else
	d_fail("bad $ escape in code line %u\n", line);
    } else { 
      fputc(*c, fp); 
      c++;
    }
  }
  fprintf(fp, "  return 0;");
  fprintf(fp, "}\n\n");
  g.write_line += 2;
  if (g.write_line_directives) {
    fprintf(fp, "#line %d \"%s\"\n", g.write_line, g.write_pathname);
    g.write_line++;
  }
}

static void
write_global_code(FILE *fp, Grammar *g, char *tag) {
  int i;
  char *c;
  
  for (i = 0; i < g.ncode; i++) {
    if (g.write_line_directives) {
      fprintf(fp, "#line %d \"%s\"\n", g.code[i].line, g.pathname);
      g.write_line++;
    }
    c = g.code[i].code;
    while (*c) {
      if (*c == '\n')
	g.write_line++;
      if (*c == '$') {
	c++;
	if (*c == '{') {
	  char *e = ++c, *a;
	  while (*e && *e != '}' && !isspace_(*e)) ++e;
	  a = e;
	  if (isspace_(*a)) ++a;
	  while (*a && *a != '}') a++;
	  if (STREQ(c, e-c, "nterm")) {
	    fprintf(fp, "%d", find_symbol(g, e, a, D_SYMBOL_NTERM));
	  } else if (STREQ(c, e-c, "string")) {
	    fprintf(fp, "%d", find_symbol(g, e, a, D_SYMBOL_STRING));
	  } else if (STREQ(c, e-c, "pass")) {
	    D_Pass *p = find_pass(g, e, a);
	    if (!p)
	      d_fail("unknown pass '%s' line %d", dup_str(e, a), g.code[i].line + i);
	    fprintf(fp, "%d", p.index);
	  } else
	    d_fail("bad $ escape in code line %u\n", g.code[i].line + i);
	  c = a + 1;
	}
	else
	  d_fail("bad $ escape in code line %u\n", g.code[i].line + i);
      } else {
	fputc(*c, fp);
	c++;
      }
    }
    fprintf(fp, "\n");
    g.write_line += 1;
  }
  if (g.write_line_directives) {
    fprintf(fp, "#line %d \"%s\"\n", g.write_line, g.write_pathname);
    g.write_line++;
  }
}
static char * reduction_args = "(void *_ps, void **_children, int _n_children, int _offset, D_Parser *_parser)";
+/


static void
buildReductions(Grammar *g, ref BuildTables tables) {
    int i, j, k, l, pmax;
    Production *p, pdefault;
    Rule *r, rdefault = null;

    pdefault = lookup_production(g, "_", 1);
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

extern(C) static uint32
er_hint_hash_fn(State *a, hash_fns_t *fns) {
  VecHint *sa = &a.error_recovery_hints;
  uint32 hash = 0, i;
  Term *ta;

  for (i = 0; i < sa.n; i++) {
    ta = sa.v[i].rule.elems.v[sa.v[i].rule.elems.n - 1].e.term;
    hash += (sa.v[i].depth + 1) * 13;
    hash += strhashl(ta.string, ta.string_len);
    if (sa.v[i].rule)
      hash += sa.v[i].rule.prod.index * 10007;
  }
  return hash;
}

extern(C) static int
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
	strcmp(ta.string, tb.string) ||
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


static void
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
                        ss = escape_string(t.string);
                        D_ErrorRecoveryHint hint;
                        hint.depth = cast(ushort)s.error_recovery_hints.v[j].depth;
                        hint.symbol = cast(ushort)s.error_recovery_hints.v[j].rule.prod.index;
                        hint.str = ss;
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
//static char *scan_kind_strings[] = {"D_SCAN_ALL", "D_SCAN_LONGEST", "D_SCAN_MIXED",  null};

static void
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
/+
static int
write_header(Grammar *g, char *base_pathname, char *tag) {
  char pathname[FILENAME_MAX];
  char ver[128];
  int i, tokens = 0, states = 0, col;
  FILE *hfp;

  for (i = 0; i < g.terminals.n; i++)
    if (g.terminals.v[i].kind == TermKind.TERM_TOKEN)
      tokens = 1;
  if (g.states_for_all_nterms)
    states = 1;
  else
    for (i = 0; i < g.productions.n; i++)
      if (state_for_declaration(g, i))
	states = 1;
  if (g.write_header > 0 || (g.write_header < 0 && (tokens || states))) {
    strcpy(pathname, base_pathname);
    strcat(pathname, ".d_parser.h");
    hfp = fopen(pathname, "w");
    if (!hfp)
      d_fail("unable to open `%s` for write\n", pathname);
    d_version(ver);
    fprintf(hfp, "/*\n  Generated by Make DParser Version %s\n", ver);  
    fprintf(hfp, "  Available at http://dparser.sf.net\n*/\n\n");  
    fprintf(hfp, "#ifndef _%s_h\n", tag);
    fprintf(hfp, "#define _%s_h\n", tag);
    if (tokens) {
      if (!g.token_type) {
	for (i = 0; i < g.terminals.n; i++)
	  if (g.terminals.v[i].kind == TermKind.TERM_TOKEN)
	    fprintf(hfp, "#define %s \t%d\n",
		    g.terminals.v[i].string,
		    g.terminals.v[i].index + g.productions.n);
      } else {
	fprintf(hfp, "enum D_Tokens_%s {\n", tag);
	col = 0;
	for (i = 0; i < g.terminals.n; i++) {
	  if (g.terminals.v[i].kind == TermKind.TERM_TOKEN) {
	    col += g.terminals.v[i].string_len + 7;
	    if (col > 70) { printf("\n"); col = 0; }
	    fprintf(hfp, "%s = %d%s",
		    g.terminals.v[i].string,
		    g.terminals.v[i].index + g.productions.n,
		    i == g.terminals.n-1 ? "" : ", ");
	  }
	}
	fprintf(hfp, "\n};\n");
      }
    }
    if (states) {
      for (i = 0; i < g.productions.n; i++)
	if (!g.productions.v[i].internal && g.productions.v[i].elem)
	  fprintf(hfp, "#define D_START_STATE_%s \t%d\n",
		  g.productions.v[i].name, g.productions.v[i].state.index);
    }
    fprintf(hfp, "#endif\n");
    fclose(hfp);
    return 1;
  }
  return 0;
}
+/
bool is_EBNF(uint _x)
{
    return _x == InternalKind.INTERNAL_CONDITIONAL || _x == InternalKind.INTERNAL_STAR || _x == InternalKind.INTERNAL_PLUS;
}

//static char *d_internal[] = {"D_SYMBOL_NTERM", "D_SYMBOL_EBNF", "D_SYMBOL_INTERNAL"};
static int d_internal_values[] = [D_SYMBOL_NTERM, D_SYMBOL_EBNF, D_SYMBOL_INTERNAL];
/* static char *d_symbol[] = {  */
/*   "D_SYMBOL_STRING", "D_SYMBOL_REGEX", "D_SYMBOL_CODE", "D_SYMBOL_TOKEN" }; */
static int d_symbol_values[] = [ 
  D_SYMBOL_STRING, D_SYMBOL_REGEX, D_SYMBOL_CODE, D_SYMBOL_TOKEN ];

static void
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
        sym.name_len = g.productions.v[i].name_len;
        sym.start_symbol = state;
        d_symbols ~= sym;
    }
    for (i = 0; i < g.terminals.n; i++) {
        char *s = escape_string(g.terminals.v[i].string); /* so it is a string */
        char *name = g.terminals.v[i].term_name ? g.terminals.v[i].term_name : s;
        int symbol_index = g.terminals.v[i].kind;
        D_Symbol sym;
        sym.kind = d_symbol_values[symbol_index];
        sym.name = name;
        sym.name_len = cast(int)strlen(name);
        d_symbols ~= sym;
    }
    tables.d_symbols = d_symbols;
}

static void
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
/+
void
write_parser_tables(Grammar *g, char *tag, File *file) {
  int whitespace_production = 0;
  VecState er_hash;
  Production *p;
  vec_clear(&er_hash);

  g.scanner_block_size = 256/g.scanner_blocks;

  write_reductions(file, g, tag);
  write_scanner_data(file, g, tag);
  if (!file.binary)
    write_scanner_code(file, g, tag);
  write_goto_data(file, g, tag);
  write_error_data(file, g, &er_hash, tag);
  write_state_data(file, g, &er_hash, tag);
  write_symbol_data(file, g, tag);
  write_passes(file, g, tag);
  vec_free(&er_hash);

  if ((p = lookup_production(g, "whitespace", ("whitespace").sizeof-1)))
    whitespace_production = p.state.index;

  if (file.binary) {
    file.d_parser_tables_loc = file.tables.cur - file.tables.start;
  }

  start_struct(file, D_ParserTables, make_name("parser_tables_%s", tag), "\n");
  add_struct_member(file, D_ParserTables, %d, g.states.n, nstates);
  add_struct_ptr_member(file, D_ParserTables, "", get_offset(file, "d_states_%s", tag), state);
  add_struct_ptr_member(file, D_ParserTables, "", get_offset(file, "d_gotos_%s", tag), goto_table);
  add_struct_member(file, D_ParserTables, %d, whitespace_production, whitespace_state);
  add_struct_member(file, D_ParserTables, %d, g.productions.n + g.terminals.n, nsymbols);
  add_struct_ptr_member(file, D_ParserTables, "", get_offset(file, "d_symbols_%s", tag), symbols);
  if (g.default_white_space) {
    assert(!file.binary);
    fprintf(file.fp, ", %s", g.default_white_space);
  } else 
    add_struct_ptr_member(file, D_ParserTables, "", &null_entry, default_white_space);
  add_struct_member(file, D_ParserTables, %d, g.passes.n, npasses);
  if (g.passes.n)
    add_struct_ptr_member(file, D_ParserTables, "", get_offset(file, "d_passes_%s", tag), passes);
  else
    add_struct_ptr_member(file, D_ParserTables, "", &null_entry, passes);
  if (g.save_parse_tree)
    add_struct_member(file, D_ParserTables, %d, 1, save_parse_tree);
  else
    add_struct_member(file, D_ParserTables, %d, 0, save_parse_tree);
  end_struct(file, D_ParserTables, "\n");

  if (file.binary) {
    if (!file.str) {
      file.fp = fopen(g.write_pathname, "wb");
      if (!file.fp)
        d_fail("unable to open `%s` for write\n", g.pathname);
    }
    save_binary_tables(file);
  }
  free_tables(file);
  if (file.fp)
    fclose(file.fp);
}
+/
struct TableMap(T, int dimention = 1)
{
    T[string] storage;

    static string keyWithArgs(int i, int j, int k, int l) pure
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

/+
void
write_parser_tables_internal(Grammar *g, char *base_pathname, char *tag, int binary, 
			     FILE *fp, ubyte **str, uint *str_len)
{
  File file;
  file_init(&file, binary, fp, str, str_len);
  write_parser_tables(g, tag, &file);
}

int
write_c_tables(Grammar *g) {
  write_parser_tables_internal(g, g.pathname, 
			       *g.grammar_ident ? g.grammar_ident : null, 
			       0, 0, 0, 0);
  return 0;
}

int
write_binary_tables(Grammar *g) {
  write_parser_tables_internal(g, g.pathname, 
			       *g.grammar_ident ? g.grammar_ident : null, 
			       1, 0, 0, 0);
  return 0;
}

int
write_binary_tables_to_file(Grammar *g, FILE *fp) {
  write_parser_tables_internal(g, g.pathname, 
			       *g.grammar_ident ? g.grammar_ident : null, 
			       1, fp, 0, 0);
  return 0;
}

int
write_binary_tables_to_string(Grammar *g, ubyte **str, uint *str_len) {
  write_parser_tables_internal(g, g.pathname, 
			       *g.grammar_ident ? g.grammar_ident : null, 
			       1, 0, str, str_len);
  return 0;
}
+/

