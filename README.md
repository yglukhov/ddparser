ddparser
========

[Dparser](http://dparser.sourceforge.net) bindings for [D language](http://dlang.org)

Dparser is a GLR parser, with lots of features:
- Runtime generation of parsing tables. Compile-time generation is also supported.
- Handling of ambigous grammars. Calls back to user code, when ambiguity can not be resolved automatically.
- Terminals are defined within the same grammar used for language syntax, by means of strings and regular expressions.
- Many more. Please, read the [official page](http://dparser.sourceforge.net).

Usage example:
```d
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
```
