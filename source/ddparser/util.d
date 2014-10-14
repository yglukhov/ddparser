
module ddparser.util;

import core.stdc.string;
public import core.stdc.stdint;

import std.ascii;
import std.string;
import std.stdio;
import std.json;
import std.traits;
import std.conv;
import ddparser.serialize;

enum INITIAL_SET_SIZE_INDEX =       2;

enum INITIAL_VEC_SHIFT =    3;
enum INITIAL_VEC_SIZE = (1 << INITIAL_VEC_SHIFT);
enum INTEGRAL_VEC_SIZE =    3;
enum INTEGRAL_STACK_SIZE =  8;

enum SET_MAX_SEQUENTIAL =   5;

bool IS_BIT_SET(T, V)(T _v, V _s) { return cast(bool)(_v[_s / 8] & 1 << (_s % 8)); }
void SET_BIT(T, V)(ref T _v, V _s) { _v[_s / 8] |= (1 << (_s % 8)); }

int d_verbose_level = 0;
int d_debug_level = 0;
int test_level = 0;
int d_rdebug_grammar_level = 0;

void trace(string file = __FILE__, int line = __LINE__, Args...)(Args args)
{
    /+
    stderr.write(file, ":", line);
    if (args.length)
    {
        stderr.write(":");
        stderr.writeln(args);
    }
    else stderr.writeln();
    +/
}

struct Vec(T)
{
    uint n;
    uint i;
    T *v;
    T e[INTEGRAL_VEC_SIZE];
    uint isIterating;

    void clear()
    {
        assert(isIterating == 0);
        n = 0;
        v = null;
    }

    @property size_t length() const
    {
        return n;
    }

    void add(T _i) @trusted
    {
        assert(isIterating == 0);
        if (!v) {
            v = e.ptr;
            e[n] = _i;
            n++;
            return;
        } else if (v == e.ptr) {
            if ((n < INTEGRAL_VEC_SIZE)) {
                v[n++] = (_i);
                return;
            }
        } else if (n & (INITIAL_VEC_SIZE - 1)) {
            v[n++] = (_i);
            return;
        }
        vec_add_internal(_i);
    }

    @property inout(T)[] array() inout
    {
        return v[0 .. n];
    }

    ref inout(T) opIndex(size_t index) inout @trusted
    {
        assert(index < n, "Index out of bounds: " ~ index.to!string());
        return v[index];
    }

    void opOpAssign(string s: "~")(T v)
    {
        add(v);
    }

    int opApply(int delegate(ref T) operations)
    {
        isIterating++;
        int res = 0;
        for(int i = 0; i < n; ++i)
        {
            res = operations(v[i]);
            if (res) break;
        }
        isIterating--;
        return res;
    }

    int opApplyReverse(scope int delegate(ref T) operations)
    {
        isIterating++;
        int res = 0;
        for(int i = n - 1; i >= 0; --i)
        {
            res = operations(v[i]);
            if (res) break;
        }
        isIterating--;
        return res;
    }


    void vec_add_internal(T elem) {
        assert(isIterating == 0);
        if (!n) {
            v = e.ptr;
        } else if (v == e.ptr) {
            v = cast(T*)MALLOC(INITIAL_VEC_SIZE * T.sizeof);
            memcpy(v, e.ptr, n * T.sizeof);
        } else {
            if ((n & (INITIAL_VEC_SIZE - 1)) == 0) {
                int l = n, nl = (1 + INITIAL_VEC_SHIFT);
                l = l >> INITIAL_VEC_SHIFT;
                while (!(l&1)) { l = l >> 1; nl++; }
                l = l >> 1;
                if (!n || !l) {
                    nl = 1 << nl;
                    v = cast(T*)REALLOC(v, nl * T.sizeof);
                }
            }
        }
        v[n] = elem;
        n++;
    }

    JSONValue customSerialize(Serializer s)
    {
        auto arr = v[0 .. n];
        return s.serializeValue(arr);
    }

    void customDeserialize(Serializer s, in JSONValue j)
    {
        auto arr = j.array;
        foreach(i; arr)
        {
            T t;
            s.deserializeValue(t, i);
            add(t);
        }
    }
}

unittest
{
    Vec!int v;
    v ~= 5;
    assert(v.length == 1);
    v ~= 6;
    assert(v.length == 2);
    assert(v[0] == 5);
    assert(v[1] == 6);
}

void vec_add(T, U)(T _v, U _i) @safe if (isPointer!T)
{
    _v.add(_i);
}

struct Stack(_x)
{
    _x *start;
    _x *end;
    _x *cur;
    _x initial[INTEGRAL_STACK_SIZE];

    void push(_x v)
    {
        if (cur == end )
            stack_push_internal(v);
        else
        {
            *cur = v;
            cur++;
        }
    }

    @property bool isEmpty()
    {
        return cur == start;
    }

    void stack_push_internal(_x elem)
    {
        auto n = cur - start;
        if (start == initial.ptr) {
            cur = cast(_x*)MALLOC(n * 2 * _x.sizeof);
            memcpy(cur, start, n * _x.sizeof);
        } else
            cur = cast(_x*)REALLOC(start, n * 2 * _x.sizeof);
        end = start = cur;
        cur += n;
        end += n * 2;
        *cur++ = elem;
    }
}

alias uint8 = ubyte;
alias uint16 = ushort;
alias uint32 = uint;
alias uint64 = ulong;


void vec_clear(T)(T vec) if (isPointer!T && !isArray!(typeof(*vec)))
{
    vec.clear();
}

alias vec_free = vec_clear;
bool is_stack_empty(T)(T _s)
{
    return _s.isEmpty;
}

void stack_push(T, U)(T _s, U _x)
{
    _s.push(_x);
}

auto stack_head(T)(T _s)
{
    return _s.cur[-1];
}

auto stack_pop(T)(T _s)
{
    return (*--((_s).cur));
}

int stack_depth(T)(T _s) @safe
{
    return cast(int)(_s.cur - _s.start);
}

void stack_clear(T)(T _s)
{
    _s.start = _s.cur = _s.end = _s.initial.ptr;
    _s.end += INTEGRAL_STACK_SIZE;
}

alias stack_free = stack_clear;

uint d_prime2[] = [
  1, 3, 7, 13, 31, 61, 127, 251, 509, 1021, 2039, 4093, 8191,
  16381, 32749, 65521, 131071, 262139, 524287, 1048573, 2097143,
  4194301, 8388593, 16777213, 33554393, 67108859, 134217689,
  268435399, 536870909
];

void* MALLOC(size_t s)
{
    import core.memory;
    return GC.calloc(s);
}

void* REALLOC(void* p, size_t s)
{
    import core.memory;
    return GC.realloc(p, s);
}

void FREE(void* p) @safe
{

}

string dup_code(const char *str, const char *end)
{
    if (0)
    {
    writeln("/+ g.r.final_code.code = dup_code(");
    writeln("q{");
    auto s = str[0 .. end - str].strip();
    //assert(s.startsWith("TRACE("));
    //assert(s.endsWith(")"));
    if (s.startsWith("TRACE("))
        s = s["TRACE(".length .. $ - 1];
    writeln(s);
    writeln("});");
    writeln("+/");
    }
    return dup_str(str, end);
}

import core.memory;

extern(C)
{

alias hash_fn_t = uint function (void *, hash_fns_t*);
alias cmp_fn_t = int function (void *, void *, hash_fns_t*);
}

struct hash_fns_t {
  hash_fn_t hash_fn;
  cmp_fn_t  cmp_fn;
  void      *data[2];
}

string
d_dup_pathname_str(const(char)*s) {
    const(char)*e = s;
    if (!s)
        return dup_str("", null);
    if (*e == '"') {
        e++; while (*e && *e != '"') e++;
        return dup_str(s + 1, e);
    } else
        return dup_str(s, s+strlen(s));
}

string
dup_str(const char *s, const char *e) {
  int l = cast(int)(e ? e-s : strlen(s));
  return s[0 .. l].idup;
}

uint strhashl(const(char)[] s)
{
    uint h = 0, g;
    foreach(char c; s)
    {
        h = (h << 4) + cast(ubyte)c;
        if ((g = h & 0xf0000000) != 0)
            h = (h ^ (g >> 24)) ^ g;
    }
    return h;
}

string readContentsOfFile(string path)
{
    byte[] outbuf;
    foreach(chunk; File(path).byChunk(4096)) outbuf ~= chunk;
    return cast(string)outbuf;
}

void d_fail(Args...)(Args args) @trusted
{
    write("error: ");
    writefln(args);
    assert(false);
}

void d_warn(Args...)(Args args) @trusted
{
    write("warning: ");
    writefln(args);
}

string escape_string(const(char)[] s, bool singleQuote = false) @safe
{
    auto result = appender!string();
    result.reserve(s.length * 4);
    foreach(c; s)
    {
        switch(c)
        {
            case '\b': result ~= "\\b"; break;
            case '\f': result ~= "\\f"; break;
            case '\n': result ~= "\\n"; break;
            case '\r': result ~= "\\r"; break;
            case '\t': result ~= "\\t"; break;
            case '\v': result ~= "\\v"; break;
            case '\a': result ~= "\\a"; break;
            case '\\': result ~= "\\\\"; break;
            case '\"':
                if (!singleQuote) result ~= '\\';
                result ~= c;
                break;
            case '\'':
                if (singleQuote) result ~= '\\';
                result ~= c;
                break;
            default:
                if (isPrintable(c))
                    result ~= c;
                else
                    formattedWrite(result, "\\x%X", cast(uint)c);
        }
    }

    return result.data;
}

string escape_string_single_quote(const(char)[] s) @safe
{
    return escape_string(s, true);
}


bool
set_add(T, V : Vec!T)(V* v, T t) @trusted {
    V vv;
    int j, n = v.n;
    uint i;
    if (n) {
        uint h = cast(uint)(cast(uintptr_t)t);
        h = h % n;
        for (i = h, j = 0;
                i < v.n && j < SET_MAX_SEQUENTIAL;
                i = ((i + 1) % n), j++)
        {
            if (!v.v[i]) {
                v.v[i] = t;
                return true;
            } else if (v.v[i] == t)
                return false;
        }
    }
    if (!n) {
        vv.v = null;
        v.i = INITIAL_SET_SIZE_INDEX;
    } else {
        vv.v = v.v;
        vv.n = v.n;
        v.i = v.i + 1;
    }
    v.n = d_prime2[v.i];
    v.v = cast(T*)MALLOC(v.n * T.sizeof);
    if (vv.v) {
        set_union(v, &vv);
    }
    return set_add(v, t);
}

T
set_add_fn(T, V : Vec!T)(V *v, T t, hash_fns_t *fns) {
  V vv;
  uint32 tt = fns.hash_fn(t, fns);
  int j, n = v.n;
  uint i;
  if (n) {
    uint h = tt % n;
    for (i = h, j = 0;
     i < v.n && j < SET_MAX_SEQUENTIAL;
     i = ((i + 1) % n), j++)
    {
      if (!v.v[i]) {
    v.v[i] = t;
    return t;
      } else {
    if (!fns.cmp_fn(v.v[i], t, fns))
      return v.v[i];
      }
    }
  }
  if (!n) {
    vv.v = null;
    v.i = INITIAL_SET_SIZE_INDEX;
  } else {
    vv.v = v.v;
    vv.n = v.n;
    v.i = v.i + 1;
  }
  v.n = d_prime2[v.i];
  v.v = cast(T*)MALLOC(v.n * T.sizeof);
  if (vv.v) {
    set_union_fn(v, &vv, fns);
  }
  return set_add_fn(v, t, fns);
}

bool
set_union(V)(V *v, V *vv) @trusted {
  bool changed = false;
  foreach (i; *vv)
    if (i)
      changed = set_add(v, i) || changed;
  return changed;
}

void
set_union_fn(V)(V *av, V *vv, hash_fns_t *fns) {
  uint i;

  for (i = 0; i < vv.n; i++)
    if (vv.v[i])
      set_add_fn(av, vv.v[i], fns);
}

void
set_to_vec(V)(V *v) {
  V vv;
  uint i;

  vv.n = v.n;
  vv.v = v.v;
  if (v.v == v.e.ptr) {
    memcpy(vv.e.ptr, v.e.ptr, (v.e).sizeof);
    vv.v = vv.e.ptr;
  }
  v.n = 0;
  v.v = null;
  for (i = 0; i < vv.n; i++)
    if (vv.v[i])
      v.vec_add_internal(vv[i]);
}

bool
set_find(T, V : Vec!T)(V *v, T t) {
  int j, n = v.n;
  uint i;
  if (n) {
    uint h = cast(uint)(cast(uintptr_t)t);
    h = h % n;
    for (i = h, j = 0;
     i < v.n && j < SET_MAX_SEQUENTIAL;
     i = ((i + 1) % n), j++)
    {
      if (!v.v[i]) {
    return false;
      } else if (v.v[i] == t)
    return true;
    }
  }
  return false;
}

void d_free(void *x) { }

import std.format;
import std.algorithm;
import std.array;

void delegate(string s) logFunc;

void log(Args...)(Args args)
{
    writeln(args);
}

void logf(Args...)(Args args)
{
    auto writer = appender!string();
    formattedWrite(writer, args);
    if (logFunc) logFunc(writer.data);
    else write(writer.data);
}

