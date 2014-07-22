
import dparser;
import std.stdio;

unittest
{
    Grammar g = new Grammar();
    g
    << `EXPRESSION: EXPRESSION '+' EXPRESSION` << (c)
    {
        writeln("EXPRESSION: ", c[0], " + ", c[2]);
    }
    << ` | INT_LITERAL` << g.propagate
    << `INT_LITERAL: "[0-9]+"` << (c)
    {
        writeln("Parsed literal: ", c[0].stringValue);
    }
    ;

    Parser p = new Parser();
    if (p.setGrammar(g))
    {
        writeln("PARSE RESULT: ", p.parse("123 + 456"));
    }
}

