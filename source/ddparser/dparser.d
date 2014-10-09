module ddparser.dparser;

import std.c.string;
import std.string;
import std.stdio;
import core.memory;
import std.conv;
import std.typecons;
import std.variant;
import std.exception;
import ddparser.gram;
import ddparser.dparse_tables;
import ddparser.dparse;
import ddparser.gramgram;
import ddparser.parse;
import ddparser.write_tables;

alias D_Grammar = ddparser.gram.Grammar;
alias _Parser = ddparser.parse.Parser;

void hexDump(const ubyte* ptr, uint len)
{
    for(uint i = 0; i < len; ++i)
    {
        writef("%X", ptr[i]);
    }
}

class Grammar
{
    alias Action = void delegate(Parser.Context c);

    shared static this()
    {
        propagate = (c)
        {
            enforce(c.length == 1);
            c.result = c[0];
        };
    }

    static Action propagate;
    static Action reduceByAppending;
    enum Spec { spec }
    alias spec = Spec.spec;

    static struct Production
    {
        string g;
        bool spec = false;
        Action a;
    }

    Grammar opBinary(string s : "<<")(string g)
    {
        productions ~= Production(g);
        return this;
    }

    Grammar opBinary(string s : "<<")(Action a)
    {
        productions[$-1].a = a;
        return this;
    }

    Grammar opBinary(string s : "<<")(Spec)
    {
        productions[$-1].spec = true;
        return this;
    }

    Action actionAtIndex(uint i)
    {
        return productions[i].a;
    }

    final @property void rootSymbol(string symbol)
    {
        uint start = firstIndexOfProductionForSymbol(symbol);
        if (start == 0) return; // The needed production is already first

        uint end = endOfProductionStartingAtIndex(start);

        // Move productions from start to end to the beginning of array
        productions = productions[start .. end] ~ productions[0 .. start] ~ productions[end .. $];
    }

    final @property string rootSymbol() const
    {
        assert(productions.length, "Grammar is empty");
        auto s = productions[0].g;
        auto indexOfColon = s.indexOf(":");
        assert(indexOfColon != -1, "Malformed production");
        return s[0 .. indexOfColon].strip();
    }

    override string toString() const
    {
        string result;
        foreach(i, p; productions)
        {
            if (i)
            {
                if (!p.g.strip().startsWith("|")) result ~= ";";
                result ~= "\n";
            }
            result ~= p.g;
            result ~= " ${action}";
        }
        return result ~ ";";
    }

    private final uint endOfProductionStartingAtIndex(uint index) const
    {
        foreach(i, p; productions[index + 1 .. $])
        {
            if (!p.g.strip.startsWith("|")) return cast(uint)i + index;
        }
        return cast(uint)productions.length;
    }

    private final uint firstIndexOfProductionForSymbol(string sym) const
    {
        string searchString = sym ~ ":";
        foreach(i, p; productions)
        {
            if (p.g.strip().startsWith(searchString)) return cast(uint)i;
        }
        assert(false, "Production " ~ sym ~ " not found");
    }

    Production[] productions;
}

struct Location
{
    ulong offset;
    uint line;
    uint column;
    uint length;
}

abstract class ParseNode
{
    string identifier;
    Variant value;
    Location location;
    abstract @property string stringValue() const;
    abstract override string toString() const;
    @property bool isTerminal() const { return true; }
    @property bool isEmpty() const { return false; }
}

class TerminalNode : ParseNode
{
    override @property string stringValue() const
    {
        return value.get!(const string)();
    }

    override string toString() const
    {
        return "'" ~ stringValue ~ "'";
    }
}

class NonTerminalNode : ParseNode
{
    ParseNode[] children;
    override @property string stringValue() const
    {
        return toString();
    }

    override string toString() const
    {
        return identifier ~ children.to!string();
    }

    override @property bool isTerminal() const { return false; }
    override @property bool isEmpty() const { return children.length == 0; }
}

extern(C) void BREAK()
{

}

class Parser
{
    alias AmbiguityHandler = ParseNode delegate(ParseNode[]);

    this()
    {
        context = new Context();
        context.parser = this;
    }

    ~this()
    {
        //if (binaryTables) free_BinaryTables(binaryTables);
        //if (parser) free_D_Parser(parser);
    }

    bool setGrammar(Grammar g)
    {
        if (!setGrammar(g.toString())) return false;
        grammar = g;
        return true;
    }

    bool setGrammarNewWay(Grammar g)
    {
        D_Grammar *_g = grammarWithString(g.toString());

        binaryTables = tablesWithGrammar(_g);

        createParser();
        grammar = g;
        return true;
    }

    ParseNode parse(string s)
    {
        D_ParseNode * node = dparse(parser, cast(char*)s.ptr, cast(int)s.length);
        ParseNode result;
        bool ok = false;
        if (node && !parser.syntax_errors)
        {
            result = cast(ParseNode)node.user;
            ok = true;
        }

        if (node)
        {
            free_D_ParseNode(parser, node);
        }

        if (!ok && !errorRecoveryEnabled)
        {
            syntaxError();
        }

        return result;
    }

    static class Context
    {
        Parser parser;
        Object userInfo;
        bool isFinal;

        @property ulong length()
        {
            return children.length;
        }

        auto opDollar()
        {
            return length;
        }

        auto opIndex(uint index)
        {
            index < children.length || assert(false, "Out of bounds: " ~ this.toString());
            return children[index];
        }

        @property Location location() const
        {
            return result.location;
        }

        ParseNode[] children;
        ParseNode result;

        override string toString() const
        {
            return result.toString();
        }
    }

    final @property Location location() const
    {
        return Location(parser.loc.s - inputPtr, parser.loc.line, parser.loc.col);
    }

    @property bool useGreedinessForDisambiguation() const
    {
        if (parser) return !parser.dont_use_greediness_for_disambiguation;
        return _useGreedinessForDisambiguation;
    }

    @property void useGreedinessForDisambiguation(bool flag)
    {
        _useGreedinessForDisambiguation = flag;
        if (parser) parser.dont_use_greediness_for_disambiguation = !flag;
    }

    @property bool useHeightForDisambiguation() const
    {
        if (parser) return !parser.dont_use_height_for_disambiguation;
        return _useHeightForDisambiguation;
    }

    @property void useHeightForDisambiguation(bool flag)
    {
        _useHeightForDisambiguation = flag;
        if (parser) parser.dont_use_height_for_disambiguation = !flag;
    }

    @property bool errorRecoveryEnabled() const
    {
        if (parser) return !!parser.error_recovery;
        return _errorRecoveryEnabled;
    }

    @property void errorRecoveryEnabled(bool flag)
    {
        _errorRecoveryEnabled = flag;
        if (parser) parser.error_recovery = !!flag;
    }

    AmbiguityHandler ambiguityHandler;
    void delegate(Parser) syntaxErrorHandler;

    static D_ParserTables* tablesWithGrammar(Grammar g, bool oldWay)
    {
        auto dg = grammarWithString(g.toString());
        return tablesWithGrammar(dg);
    }

private:

    static D_Grammar* grammarWithString(string grammarString)
    {
        D_Grammar *g = new_D_Grammar();

        g.set_op_priority_from_rule = 0;
        g.right_recursive_BNF = 0;
        g.states_for_whitespace = 1;
        g.states_for_all_nterms = 1;
        g.tokenizer = 0;
        g.longest_match = 1;
        strcpy(g.grammar_ident.ptr, "grammar".ptr);
        g.scanner_blocks = 4;
        g.scanner_block_size = 0;
        g.write_line_directives = 1;
        g.write_header = -1;
        g.token_type = 0;

        // TODO: Can't handle syntax error here =(
        if (parse_grammar(g, null, cast(char*)grammarString.toStringz()) < 0) return null;

        if (g.productions.n < 2) throw new Exception("Too few productions");

        if (build_grammar(g) < 0) return null;
        return g;
    }

    static D_ParserTables* tablesWithGrammar(D_Grammar* g)
    {
        return createTablesFromGrammar(g, &spec_code, &final_code);
    }

    bool setGrammar(string grammarString)
    {
        D_Grammar *g = grammarWithString(grammarString);
        scope(exit) free_D_Grammar(g);

        binaryTables = tablesWithGrammar(g);

        createParser();
        return true;
    }

    void createParser()
    {
        parser = new_D_Parser(binaryTables, D_ParseNode_User.sizeof);
        parser.initial_globals = cast(void*)this;
        parser.save_parse_tree = 1;
        parser.free_node_fn = &free_node;
//        parser.dont_fixup_internal_productions = 0;
        parser.commit_actions_interval = 0;
        parser.ambiguity_fn = &ambigfn;
        parser.syntax_error_fn = &syntaxErrorFn;
        parser.dont_use_greediness_for_disambiguation = !_useGreedinessForDisambiguation;
        parser.dont_use_height_for_disambiguation = !_useHeightForDisambiguation;
        parser.error_recovery = _errorRecoveryEnabled;
    }

    static D_ParseNode * ambigfn(D_Parser* p, int n, D_ParseNode **v)
    {
        if (p.initial_globals)
        {
            return (cast(Parser)p.initial_globals).ambiguity(p, v[0 .. n]);
        }
        return null;
    }

    final int tryResolvingAmbiguityByPreferringStringOverRegex(D_ParseNode*[] nodes)
    {
        int resolvingIndex = -1;

        foreach(i, pn; nodes)
        {
            bool metString = false;
            bool metRegex = false;
            auto tempNode = pn;
            do
            {
                auto sym = symbolForNode(tempNode);
                metString = sym.kind == D_SymbolKind.D_SYMBOL_STRING;
                metRegex = sym.kind == D_SymbolKind.D_SYMBOL_REGEX;
                if (metString || metRegex) break;
                if (d_get_number_of_children(tempNode) != 1)
                    return -1;
                tempNode = d_get_child(tempNode, 0);
            } while(true);

            if (metString)
            {
                if (resolvingIndex == -1)
                    resolvingIndex = cast(int)i;
                else
                    return -1;
            }
            else if (!metRegex)
                return -1;
        }
        return resolvingIndex;
    }

    final D_ParseNode* ambiguity(D_Parser* p, D_ParseNode*[] v)
    {   
        writeln("AMBIGUITY: ");

        foreach (i, n; v)
        {
            writeln("NODE ", i);
            printNode(n);
        }

        int res = tryResolvingAmbiguityByPreferringStringOverRegex(v);
        if (res != -1) return v[res];


//        throw new Exception("AMBIG!!");

        return null;
    }

    static void free_node(D_ParseNode *d)
    {
        if (d.user)
        {
            GC.removeRoot(cast(void*)d.user);
        }
    }

    static void syntaxErrorFn(D_Parser* p)
    {
        assert(p.initial_globals);
        (cast(Parser)p.initial_globals).syntaxError();
    }

    void syntaxError()
    {
        if (syntaxErrorHandler) syntaxErrorHandler(this);
        else
        {
            throw new Exception("Syntax error: " ~ location.line.to!string() ~ ":" ~ location.column.to!string());
        }
    }

    static int spec_code(void *new_ps, void **children, int n_children, int pn_offset, D_Parser *parser)
    {
        if (parser.initial_globals)
        {
            return (cast(Parser)parser.initial_globals).action(new_ps, children, n_children, pn_offset, true);
        }
        return 0;
    }

    static int final_code(void *new_ps, void **children, int n_children, int pn_offset, D_Parser *parser)
    {
        if (parser.initial_globals)
        {
            return (cast(Parser)parser.initial_globals).action(new_ps, children, n_children, pn_offset, false);
        }
        return 0;
    }

    final void appendNodeResults(ref ParseNode[] output, D_ParseNode* node)
    {
        ParseNode u = cast(ParseNode)node.user;

        if (u is null)
        {
            auto childSym = symbolForNode(node);

            if (childSym.kind == D_SymbolKind.D_SYMBOL_REGEX || childSym.kind == D_SymbolKind.D_SYMBOL_STRING)
            {
                string symbolName = symbolNameForSymbol(childSym);
                if (logLevel) writeln("CH ", symbolName);
                string value = stringValueForNode(node);
                auto term = new TerminalNode();
                copyLocationFromDParseNodeToParseNode(node, term);
                term.location.length = cast(uint)value.length;
                term.identifier = symbolName;
                term.value = value;
                node.user = cast(void*)term;
                GC.addRoot(cast(void*)term);
                output ~= term;
            }
            else if (childSym.kind == D_SymbolKind.D_SYMBOL_INTERNAL || childSym.kind == D_SymbolKind.D_SYMBOL_EBNF)
            {
                int count = d_get_number_of_children(node);
                if (logLevel) writeln("CH INT {");
                for (int i = 0; i < count; ++i)
                    appendNodeResults(output, d_get_child(node, i));
                if (logLevel) writeln("}");
            }
            else if (logLevel)
            {
                writeln("CH UNKNOWN ", childSym.kind);
            }
        }
        else
        {
            output ~= u;
        }
    }

    final int action(void *new_ps, void **children, int n_children, int pn_offset, bool speculative)
    {
        uint actionIndex = (cast(PNode*)new_ps).reduction.action_index;

        if (!speculative || grammar.productions[actionIndex].spec)
        {
            context.isFinal = !speculative;
            auto sym = symbolForNode(D_PN(new_ps, pn_offset));
            ParseNode[] args;
        //    writeln(speculative ? "SPEC" : "FINAL", " DEB ", symbolNameForSymbol(sym), " ", sym.kind);
        //    if(symbolNameForSymbol(sym) == "FUNC_ARG"){ logLevel = 1; printNode(D_PN(new_ps, pn_offset)); }
            foreach (child; children[0 .. n_children])
            {
                appendNodeResults(args, D_PN(child, pn_offset));
            }
            logLevel = 0;
            doReduceAction(D_PN(new_ps, pn_offset), (cast(PNode*)new_ps).reduction.action_index, args);
        }

        return 0;
    }

    final void doReduceAction(D_ParseNode* newNode, uint actionIndex, ParseNode[] args)
    {
        free_node(newNode);
        auto action = grammar.actionAtIndex(actionIndex);
        context.result = _defaultAction(args, newNode);
        context.children = args;
        try
        {
            if (action) action(context);
        }
        catch(Exception e)
        {
            writeln("Exception while reducing: ", context);
            throw e;
        }
        newNode.user = cast(void*)context.result;
        if (context.result) GC.addRoot(cast(void*)context.result);
    }

    static string symbolNameForSymbol(const D_Symbol* s) @trusted nothrow pure
    {
        return s.name;
    }

    static string stringValueForNode(D_ParseNode* cn)
    {
        return cn.start_loc.s[0 .. cn.end - cn.start_loc.s].idup;
    }

    final const(D_Symbol)* symbolForNode(D_ParseNode* n) const @trusted nothrow pure
    {
        return &binaryTables.symbols[n.symbol];
    }

    final ParseNode _defaultAction(ParseNode[] args, D_ParseNode* newNode) @safe nothrow pure
    {
//        if (args.length == 1) return args[0];
        auto result = new NonTerminalNode();
        result.children = args;
        result.identifier = symbolNameForSymbol(symbolForNode(newNode));
        copyLocationFromDParseNodeToParseNode(newNode, result);
        if (args.length)
        {
            result.location.length = cast(uint)(args[$-1].location.offset - result.location.offset + args[$-1].location.length);
        }
        return result;
    }

    final printNode(D_ParseNode* node) @safe nothrow
    {
        //print_parsetree(*binaryTables, node);
    }

    final @property char* inputPtr() const pure nothrow @trusted
    {
        assert(parser);
        return (cast(_Parser*)parser).start;
    }

    final void copyLocationFromDParseNodeToParseNode(D_ParseNode* from, ParseNode to) pure nothrow @safe const
    {
        to.location.offset = from.start_loc.s - inputPtr;
        to.location.line = from.start_loc.line;
        to.location.column = from.start_loc.col;
    }

    ParseNode[] allNodes; // This array is needed not to let GC collect the objects which are referenced from
    // somewhere in C code

    Context context;
    D_ParserTables * binaryTables;
    D_Parser* parser;
    Grammar grammar;
    uint logLevel;
    bool _useGreedinessForDisambiguation = true;
    bool _useHeightForDisambiguation = true;
    bool _errorRecoveryEnabled = true;
}
