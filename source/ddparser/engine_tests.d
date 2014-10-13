module ddparser.engine_tests;

import ddparser.write_tables;
import ddparser.parse;
import ddparser.dparse_tables;
import ddparser.gramgram;
import ddparser.gram;
import ddparser.dparse;
import ddparser.util;
import std.stdio;


unittest
{
    string[] tests = [
        "g2.test",
        "g3.test",
//      "g4.test",
        "g5.test",
        "g6.test",
//      "g7.test",
        "g8.test",
        "g9.test",
//      "g10.test",
        "g11.test",
        "g12.test",
        "g13.test",
        "g14.test",
        /* "g15.test", */
        /* "g16.test", */
        /* "g17.test", */
        /* "g18.test", */
        /* "g19.test", */
        /* "g20.test", */
        "g21.test",
        "g22.test",
        "g23.test",
        "g24.test",
        /* "g25.test", */
        /* "g26.test", */

        "g37.test",
        /* "g38.test", */
        "g39.test",
        /* "g40.test", */
        "g41.test",
        /* "g42.test", */
        "g43.test",
        "g44.test",
        /* "g45.test", */
        "g46.test",
        /* "g47.test", // WHITESPACE */
        /* "g48.test", */
        /* "g49.test", */
        /* "g50.test", */
        "g51.test",
    ];

    foreach(i; tests)
    {
        writeln(i);
        D_Grammar* g = createEmptyGrammar();
        string testFolder = "/Volumes/Work/Projects/ddparser/d/tests";
        string gram = readContentsOfFile(testFolder ~ "/" ~ i ~ ".g");
        parseGrammar(g, gram) || assert(false);
        build_grammar(g) >= 0 || assert(false);

        auto binaryTables = createTablesFromGrammar(g, null, null);

        auto parser = new_D_Parser(binaryTables, D_ParseNode_User.sizeof);
        parser.save_parse_tree = 1;

        string input = readContentsOfFile(testFolder ~ "/" ~ i ~ ".g.1");
        auto oldVL = d_verbose_level;

        string output;
        logFunc = (s) { output ~= s; };
        d_verbose_level = 1;
        D_ParseNode * node = dparse(parser, input);
        d_verbose_level = oldVL;

        string expectedOutput = readContentsOfFile(testFolder ~ "/" ~ i ~ ".g.1.check");
        assert(output == expectedOutput);
    }
}

