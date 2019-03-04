package com.galois.aslparser;

import com.galois.aslparser.gen.ASLParser;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CodePointCharStream;
import org.antlr.v4.runtime.CommonTokenStream;

import java.io.IOException;

public class Main {
    public static void main(String[] args) {
        String target = "/home/jlamar/prj/src/brittle/asl-conversion/gen/out.asl";
        // String target = "/home/jlamar/prj/src/brittle/asl-conversion/gen/out_instrs.asl";

        try {
            ASLParser.DefinitionsContext defs = fromFilename(target).definitions();
            SExp sexp = new ParseTreeToSExp().visit(defs);

            sexp.pretty(System.out::print);
        } catch (IOException e) {
            e.printStackTrace();
        }
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
