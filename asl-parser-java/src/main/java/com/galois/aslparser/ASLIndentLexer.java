package com.galois.aslparser;

import com.galois.aslparser.gen.ASLLexer;
import com.galois.aslparser.gen.ASLParser;
import org.antlr.v4.runtime.*;

import java.util.*;

public class ASLIndentLexer extends ASLLexer {
    private Stack<Integer> indents = initStack();
    private int nestDepth = 0;
    private int previousTokenLine = 0;
    private Set<String> openBrackSet = new HashSet<>(java.util.Arrays.asList("'{'", "'['", "'('", "'if'", "'elsif'", "'while'"));
    private Set<String> closeBrackSet = new HashSet<>(java.util.Arrays.asList("'}'", "']'", "')'", "'then'", "'do'"));
    private Token nextTok = null;

    public ASLIndentLexer(CharStream input) {
        super(input);
    }

    private static Stack<Integer> initStack() {
        Stack<Integer> st = new java.util.Stack<>();
        st.push(0);
        return st;
    }

    public String getLitName(Token t) {
        return ASLParser.VOCABULARY.getLiteralName(t.getType());
    }

    @Override
    public Token nextToken() {
        if(nextTok == null)
            nextTok = super.nextToken();

        // unindent at EOF
        if(nextTok.getType() == ASLParser.EOF) {
            if(!Objects.equals(this.indents.peek(), 0)) {
                this.indents.pop();
                return dedent();
            } else {
                return emitNext();
            }
        }

        // emit token if it's on the appropriate line or there is nesting
        if(previousTokenLine == nextTok.getLine() || nestDepth != 0) {
            return emitNext();
        }

        // If there's no indent change, emit this token
        if(nextTok.getCharPositionInLine() == indents.peek()) {
            return emitNext();
        }

        if(nextTok.getCharPositionInLine() > indents.peek()) {
            indents.push(nextTok.getCharPositionInLine());
            return indent();
        }

        // nextTok.getCharPositionInLine() < indents.peek())
        if(!indents.contains(nextTok.getCharPositionInLine())) {
            int row = nextTok.getLine();
            int col = nextTok.getCharPositionInLine();

            throw new RuntimeException("BUG: " + row + ":" + col + " unindent to unintroduced level");
        } else {
            indents.pop();
            return dedent();
        }
    }

    private Token emitNext() {
        if(nextTok == null)
            throw new RuntimeException("Tried to emit nextTok and it was null");

        Token ret = nextTok;
        nextTok = null;

        if(openBrackSet.contains(getLitName(ret)))
            nestDepth++;

        if(closeBrackSet.contains(getLitName(ret)))
            nestDepth--;

        previousTokenLine = ret.getLine();

        return ret;
    }

    private Token indent() {
        CommonToken indent = commonToken(ASLParser.INDENT);
        indent.setLine(nextTok.getLine());
        indent.setCharPositionInLine(nextTok.getCharPositionInLine());
        return indent;
    }

    private Token dedent() {
        CommonToken dedent = commonToken(ASLParser.DEDENT);
        dedent.setLine(nextTok.getLine());
        dedent.setCharPositionInLine(nextTok.getCharPositionInLine());
        return dedent;
    }


    private CommonToken commonToken(int type) {
        int stop = nextTok.getCharPositionInLine() - 1;
        int start = 0;
        return new CommonToken(this._tokenFactorySourcePair, type, DEFAULT_TOKEN_CHANNEL, start, stop);
    }
}
