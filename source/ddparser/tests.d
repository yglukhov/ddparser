module ddparser.tests;

import ddparser;
import std.stdio;

version (unittest) Grammar createGrammar()
{
    Grammar g = new Grammar();
    g
    << `EXPRESSION: EXPRESSION '+' EXPRESSION` << (c)
    {
        //writeln("EXPRESSION: ", c[0], " + ", c[2]);
    }
    << ` | INT_LITERAL` << g.propagate
    << `INT_LITERAL: "[0-9]+"` << (c)
    {
        //writeln("Parsed literal: ", c[0].stringValue);
    }
    ;
    return g;
}

version(unittest) bool testParser()
{
    Grammar g = createGrammar();

    Parser p = new Parser();
    p.setGrammar(g) || assert(false, "Invalid grammar");

    auto result = p.parse("123 + 456");
    assert(result.toString() == "EXPRESSION[INT_LITERAL['123'], '+', INT_LITERAL['456']]", "Unexpected parse result");
    return true;
}

unittest
{
    //enum t = testParser();
    assert(testParser());
}

unittest
{
    Grammar g = createGrammar();
    assert(g.rootSymbol == "EXPRESSION");
    g.rootSymbol = "INT_LITERAL";
    assert(g.rootSymbol == "INT_LITERAL");
    g.rootSymbol = "EXPRESSION";
    assert(g.rootSymbol == "EXPRESSION");
}


