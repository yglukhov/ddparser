
import std.c.string;
import std.string;
import std.stdio;
import core.memory;
import std.conv;
import std.typecons;
import std.variant;

class Grammar
{
	alias Action = void delegate(Parser.Context c);

	static this()
	{
		propagate = (c)
		{
			assert(c.length == 1);
			c.result = c[0];
		};

		reduceByAppending = (c)
		{
			c.reduceByAppending();
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

	void doCast(ref string to, TerminalNode from)
	{
		to = from.stringValue;
	}

	bool tryCast(To)(ref To to, Object from)
	{
		alias FromTypes = TypeTuple!(TerminalNode);
		foreach(T; FromTypes)
		{
			T t = cast(T)from;
			if (t !is null)
			{
				static if (is(T == To))
				{
					to = t;
				}
				else
				{
					doCast(to, t);
				}
				return true;
			}
		}

		return false;
	}

	Action actionAtIndex(uint i)
	{
		return productions[i].a;
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

	private static Object toRet(string a)
	{
		auto res = new TerminalNode();
		res.value = a;
		return res;
	}

	Production[] productions;
}


private
{
	enum INTEGRAL_VEC_SIZE = 3;

extern(C):

	// util.h
	struct Vec(T)
	{
		uint n;
		uint i;
		T *v;
		T e[INTEGRAL_VEC_SIZE];
	}

	alias AbstractVec = Vec!(void*);

	void d_fail(const char *str, ...);
	void d_free(void *);


	// dparse.h
	alias D_ParseNode_Globals = Object;
	alias D_ParseNode_User = ParseNode;

	struct D_Parser
	{
		D_ParseNode_Globals	initial_globals;		/* global values */
		D_WhiteSpaceFn 	initial_white_space_fn;
		D_Scope 	*initial_scope;
		D_SyntaxErrorFn 	syntax_error_fn;
		D_AmbiguityFn 	ambiguity_fn;
		D_FreeNodeFn          free_node_fn;
		d_loc_t 		loc; 		/* initial location, set on error */
		int			start_state; // do not move or change without fixing copy_user_configurables()
		/* user configurables */
		int 			sizeof_user_parse_node;
		int 			save_parse_tree;
		int			dont_compare_stacks;
		int 			dont_fixup_internal_productions;
		int 			fixup_EBNF_productions;
		int			dont_merge_epsilon_trees;
		int			dont_use_height_for_disambiguation;
		int			dont_use_greediness_for_disambiguation;
		int 			commit_actions_interval; /* 0 is immediate */
		int 			error_recovery;
		int			partial_parses;
		/* parse results */
		int 			syntax_errors; // do not move or change without fixing copy_user_configurables()
	}

	alias D_SyntaxErrorFn = void function(D_Parser *);
	alias D_AmbiguityFn = D_ParseNode* function(D_Parser*, int n, D_ParseNode **v);
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

	D_Parser *new_D_Parser(D_ParserTables *t, int sizeof_ParseNode_User);
	void free_D_Parser(D_Parser *p);
	D_ParseNode *dparse(D_Parser *p, char *buf, int buf_len);

	int d_get_number_of_children(D_ParseNode *pn);
	D_ParseNode *d_get_child(D_ParseNode *pn, int child);
	D_ParseNode *d_find_in_tree(D_ParseNode *pn, int symbol);
	void free_D_ParseNode(D_Parser *p, D_ParseNode *pn);

	// gram.h
	struct Production;
	struct Term;
	struct State;
	struct Action;
	struct Rule;
	struct Elem;
	struct Declaration;
	struct D_Pass;

	struct Code {
	char 	*code;
	int	line;
	}

	struct D_Grammar {
	char			*pathname;
	Vec!(Production *)	productions;
	Vec!(Term *)		terminals;
	Vec!(State *)		states;
	Vec!(Action *)		actions;
	Code			scanner;
	Code			*code;
	int			ncode;
	Vec!(Declaration *)	declarations;
	Vec!(D_Pass *)		passes;
	Vec!(char *)		all_pathnames;
	char			*default_white_space;
	/* grammar construction options */
	int			set_op_priority_from_rule;
	int			right_recursive_BNF;
	int			states_for_whitespace;
	int			states_for_all_nterms;
	int			tokenizer;
	int			longest_match;
	int			save_parse_tree;
	/* grammar writing options */
	char			grammar_ident[256];
	int			scanner_blocks;
	int			scanner_block_size;
	int			write_line_directives;
	int			write_header;
	int			token_type;
	int			write_cpp;
	char			write_extension[256];
	/* temporary variables for grammar construction */
	Production *	p;
	Rule *		r;
	Elem *		e;
	int			action_index;
	int			action_count;
	int			pass_index;
	int			rule_index;
	int			write_line;
	char			*write_pathname;
	}

	D_Grammar *new_D_Grammar(char *pathname);
	void free_D_Grammar(D_Grammar *g);
	int parse_grammar(D_Grammar *g, char *pathname, char *str);
	int build_grammar(D_Grammar *g);


	enum ASSOC_LEFT =   	0x0001;
	enum ASSOC_RIGHT =   	0x0002;
	enum ASSOC_NARY =   	0x0004;
	enum ASSOC_UNARY =  	0x0008;
	enum ASSOC_BINARY = 	0x0010;

	enum AssocKind {
		ASSOC_NONE		= 0,
		ASSOC_NARY_LEFT 	= (ASSOC_NARY|ASSOC_LEFT),
		ASSOC_NARY_RIGHT 	= (ASSOC_NARY|ASSOC_RIGHT),
		ASSOC_UNARY_LEFT 	= (ASSOC_UNARY|ASSOC_LEFT),
		ASSOC_UNARY_RIGHT 	= (ASSOC_UNARY|ASSOC_RIGHT),
		ASSOC_BINARY_LEFT	= (ASSOC_BINARY|ASSOC_LEFT),
		ASSOC_BINARY_RIGHT	= (ASSOC_BINARY|ASSOC_RIGHT),
		ASSOC_NO		= 0x0020
	}

	// mkdparse.h
	void mkdparse_from_string(D_Grammar *g, char *str);

	// write_tables.h
	int write_binary_tables(D_Grammar *g);
	int write_binary_tables_to_string(D_Grammar *g,
					ubyte **str, uint *str_len);

	// read_binary.h
	struct BinaryTables
	{
		D_ParserTables *parser_tables_gram;
		char *tables;
	}

	BinaryTables* read_binary_tables(char *file_name, D_ReductionCode spec_code, D_ReductionCode final_code);
	BinaryTables* read_binary_tables_from_string(ubyte *buf, D_ReductionCode spec_code, D_ReductionCode final_code);
	void free_BinaryTables(BinaryTables * binary_tables);

	// dparse_tables.h
	D_ParseNode* D_PN(void* _x, int _o) { return cast(D_ParseNode*)(cast(char*)_x + _o); }

	struct d_loc_t
	{
		char *s;
		char* pathname;
		char* ws;
		int col, line;
	}

	struct D_ParserTables {
	uint		nstates;
	D_State		*state;
	ushort	*goto_table;
	uint		whitespace_state;
	uint		nsymbols;
	D_Symbol		*symbols;
	D_WhiteSpaceFn	default_white_space;
	uint		npasses;
	D_Pass		*passes;
	uint		save_parse_tree;
	}

	alias D_ReductionCode = int function(void *new_ps, void **children, int n_children, int pn_offset, D_Parser *parser);
	alias D_WhiteSpaceFn = void function(D_Parser *p, d_loc_t *loc, void **p_globals);

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

	struct D_Shift;
	struct D_State;

	enum D_SymbolKind : uint
	{
		D_SYMBOL_NTERM =		1,
		D_SYMBOL_INTERNAL =	2,
		D_SYMBOL_EBNF =		3,
		D_SYMBOL_STRING =		4,
		D_SYMBOL_REGEX =		5,
		D_SYMBOL_CODE =		6,
		D_SYMBOL_TOKEN =		7
	}


	struct D_Symbol {
	D_SymbolKind		kind;
	const char	        *name;
	int			name_len;
	int			start_symbol;
	}

	// dsymtab.h
	struct D_Scope;

	// parse.h

	alias VecPNode = Vec!(PNode*);

	struct PNode {
	uint			hash;
	AssocKind		assoc;
	int			priority;
	AssocKind		op_assoc;
	int			op_priority;
	D_Reduction		*reduction;
	D_Shift		*shift;
	static if (0) // USE_GC
	{
		uint32		refcount;
	}
	VecPNode		children;
	// uint			height;		/* max tree height */
	// uint8			evaluated;
	// uint8			error_recovery;
	// struct PNode		*all_next;
	// struct PNode		*bucket_next;
	// struct PNode		*ambiguities;
	// struct PNode		*latest;	/* latest version of this PNode */
	// char			*ws_before;
	// char			*ws_after;
	// D_Scope               *initial_scope;
	// void                  *initial_globals;
	// D_ParseNode		parse_node;	/* public fields */
	// #ifdef TRACK_PNODES
	// struct PNode		*xnext;
	// struct PNode		*xprev;
	// #endif
	}

	// dparse_tree.h
	void  print_parsetree(D_ParserTables pt, D_ParseNode *pn, void* fn = null, void *client_data = null) @trusted nothrow;

	// VERSION
	void d_version(char *v)
	{
		v[0] = 0;
	}
}

abstract class ParseNode
{
	string identifier;
	Variant value;
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
		if (binaryTables) free_BinaryTables(binaryTables);
		if (parser) free_D_Parser(parser);
	}

	bool setGrammar(Grammar g)
	{
		if (!setGrammar(g.toString())) return false;
		grammar = g;
		return true;
	}

	ParseNode parse(string s)
	{
		D_ParseNode * node = dparse(parser, cast(char*)s.ptr, cast(int)s.length);
		ParseNode result;
		if (node && !parser.syntax_errors)
		{
			result = node.user;
		}
		else
		{
			writeln("fail");
		}

		if (node)
		{
			free_D_ParseNode(parser, node);
		}

		return result;
	}

	static class Context
	{
		Parser parser;
		Object userInfo;
		bool isFinal;

		void reduceByAppending()
		{
			if (length == 0) return;
			assert(length == 2);
			NonTerminalNode res = cast(NonTerminalNode)result;
			assert(res);
			if (children[0].isEmpty && !children[1].isEmpty)
			{
				res.children = [children[1]];
				res.value = children[1].value;
			//	children = [children[1]];
			}
			else if (children[1].isEmpty && !children[0].isEmpty)
			{
				res.children = [children[0]];
				res.value = children[0].value;
			//	children = [children[0]];
			}
			else if (children[1].isEmpty && children[0].isEmpty)
			{
				res.children = [];
			}
			else
			{
				if (children[1].value.hasValue)
				{
					res.value = children[0].value ~ children[1].value;
				}
				else
				{
					res.value = children[0].value;
				}
				res.children = (cast(NonTerminalNode)children[0]).children ~ children[1];
			}
		}

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
			return children[index];
		}

		ParseNode[] children;
		ParseNode result;

		override string toString() const
		{
			return result.toString();
		}
	}

	AmbiguityHandler ambiguityHandler;

private:
	bool setGrammar(string grammarString)
	{
		D_Grammar *g = new_D_Grammar(cast(char*)"grammar".ptr);
		scope(exit) free_D_Grammar(g);

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
		if (parse_grammar(g, cast(char*)"-".ptr, cast(char*)grammarString.toStringz()) < 0) return false;

		if (g.productions.n < 2) throw new Exception("Too few productions");

		if (build_grammar(g) < 0) return false;

		ubyte* gram;
		uint len;

		if (write_binary_tables_to_string(g, &gram, &len) < 0) return false;

		binaryTables = read_binary_tables_from_string(gram, &spec_code, &final_code);
		d_free(gram);

		createParser();
		return true;
	}

	void createParser()
	{
		parser = new_D_Parser(binaryTables.parser_tables_gram, D_ParseNode_User.sizeof);
		parser.initial_globals = this;
		parser.save_parse_tree = 1;
		parser.free_node_fn = &free_node;
//		parser.dont_fixup_internal_productions = 0;
		parser.commit_actions_interval = 0;
		parser.ambiguity_fn = &ambigfn;
	}

	static extern(C) D_ParseNode * ambigfn(D_Parser* p, int n, D_ParseNode **v)
	{

		writeln("AMBIGUITY: ");
		for (int i = 0; i < n; ++i)
		{
			writeln("NODE ", i);
			(cast(Parser)p.initial_globals).printNode(v[i]);
		}


//		throw new Exception("AMBIG!!");

		return null;
	}

	static extern(C) void free_node(D_ParseNode *d)
	{
		if (d.user)
		{
			GC.removeRoot(cast(void*)d.user);
		}
	}

	static extern(C) int spec_code(void *new_ps, void **children, int n_children, int pn_offset, D_Parser *parser)
	{
		if (parser.initial_globals)
		{
			return (cast(Parser)parser.initial_globals).action(new_ps, children, n_children, pn_offset, true);
		}
		return 0;
	}

	static extern(C) int final_code(void *new_ps, void **children, int n_children, int pn_offset, D_Parser *parser)
	{
		if (parser.initial_globals)
		{
			return (cast(Parser)parser.initial_globals).action(new_ps, children, n_children, pn_offset, false);
		}
		return 0;
	}

	final void appendNodeResults(ref D_ParseNode_User[] output, D_ParseNode* node)
	{
		D_ParseNode_User u = node.user;

		if (u is null)
		{
			auto childSym = symbolForNode(node);

			if (childSym.kind == D_SymbolKind.D_SYMBOL_REGEX || childSym.kind == D_SymbolKind.D_SYMBOL_STRING)
			{
				string symbolName = symbolNameForSymbol(childSym);
				if (logLevel) writeln("CH ", symbolName);
				string value = stringValueForNode(node);
				auto term = new TerminalNode();
				term.identifier = symbolName;
				term.value = value;
				node.user = term;
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
			D_ParseNode_User[] args;
		//	writeln(speculative ? "SPEC" : "FINAL", " DEB ", symbolNameForSymbol(sym), " ", sym.kind);
		//	if(symbolNameForSymbol(sym) == "FUNC_ARG"){ logLevel = 1; printNode(D_PN(new_ps, pn_offset)); }
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
		if (action) action(context);
		newNode.user = context.result;
		if (context.result) GC.addRoot(cast(void*)context.result);
	}

	static string symbolNameForSymbol(const D_Symbol* s) @trusted nothrow pure
	{
        try { return s.name[0 .. s.name_len].idup; } catch(Exception e) { return null; }
	}

	static string stringValueForNode(D_ParseNode* cn)
	{
		return cn.start_loc.s[0 .. cn.end - cn.start_loc.s].idup;
	}

	final const(D_Symbol)* symbolForNode(D_ParseNode* n) const @trusted nothrow pure
	{
		return &binaryTables.parser_tables_gram.symbols[n.symbol];
	}

	final ParseNode _defaultAction(ParseNode[] args, D_ParseNode* newNode) @safe nothrow pure
	{
//		if (args.length == 1) return args[0];
		auto result = new NonTerminalNode();
		result.children = args;
		result.identifier = symbolNameForSymbol(symbolForNode(newNode));
		return result;
	}

	final printNode(D_ParseNode* node) @safe nothrow
	{
		print_parsetree(*binaryTables.parser_tables_gram, node);
	}

	Context context;
	BinaryTables* binaryTables;
	D_Parser* parser;
	Grammar grammar;
	uint logLevel;
}
