package com.galois.aslparser;

import com.galois.aslparser.gen.ASLParser;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CodePointCharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;

import java.io.IOException;

public class Main {
    public static void main(String[] args) {
        if(args.length != 2) {
            usageAndExit("Expecting 2 arguments, but got " + args.length);
        }

        boolean instMode = false;
        if(args[0].equalsIgnoreCase("inst"))
            instMode = true;

        else if(!args[0].equalsIgnoreCase("defs")) {
            usageAndExit("Mode argument must be either 'inst' or 'defs'");
        }

        String target = args[1];

        try {
            ParseTree defs;

            if(instMode)
                defs = fromFilename(target).instructions();
            else
                defs = fromFilename(target).definitions();

            SExp sexp = new ParseTreeToSExp().visit(defs);
            sexp.pretty(System.out::print);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void usageAndExit(String msg) {
        if(msg != null) {
            System.err.println("Error: " + msg);
            System.err.println();
        }
        System.err.println("Usage: <MODE> <FILENAME>");
        System.err.println();
        System.err.println("Where <MODE> is one of: inst defs");
        System.exit(1);
    }

    private static ASLParser fromFilename(String s) throws IOException {
        CharStream cs = CharStreams.fromFileName(s);
        ASLIndentLexer lexer = new ASLIndentLexer(cs);
        return new ASLParser(new CommonTokenStream(lexer));

    }

    private ASLParser fromString(String s) {
        CodePointCharStream cs = CharStreams.fromString(s);
        ASLIndentLexer lexer = new ASLIndentLexer(cs);
        return new ASLParser(new CommonTokenStream(lexer));
    }
}
