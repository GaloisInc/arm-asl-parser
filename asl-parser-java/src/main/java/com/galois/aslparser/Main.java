package com.galois.aslparser;

import com.galois.aslparser.gen.ASLParser;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CodePointCharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;

import java.io.IOException;

public class Main {

    private enum ParseMode {
        INST, DEFS, REGS
    }

    public static void main(String[] args) {
        if(args.length != 2) {
            usageAndExit("Expecting 2 arguments, but got " + args.length);
        }

        ParseMode instMode = ParseMode.INST;
        String modeArg = args[0].toLowerCase();

        switch (modeArg){
            case "inst" : instMode = ParseMode.INST; break;
            case "defs" : instMode = ParseMode.DEFS; break;
            case "regs" : instMode = ParseMode.REGS; break;
            default: usageAndExit("Mode argument must be either 'inst', 'defs' or 'regs'");
        }

        String target = args[1];

        try {
            ParseTree defs = null;

            switch (instMode){
                case INST: defs = fromFilename(target).instructions(); break;
                case DEFS: defs = fromFilename(target).definitions(); break;
                case REGS: defs = fromFilename(target).registers(); break;
            }

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
        System.err.println("Where <MODE> is one of: inst defs regs");
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
