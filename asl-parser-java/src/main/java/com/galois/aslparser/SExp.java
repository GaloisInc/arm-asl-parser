package com.galois.aslparser;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;

public class SExp {
    private String label;
    private List<SExp> exps;
    private boolean atom;

    private SExp(String name, List<SExp> asList) {
        if(asList.contains(null))
            throw new IllegalArgumentException("asList cannot be null");

        label = name;
        exps = asList;
        atom = false;
    }

    private SExp(String name, boolean atom) {
        label = name;
        exps = Collections.emptyList();
        this.atom = atom;
    }

    public String getLabel() {
        return label;
    }

    public List<SExp> getExps() {
        return exps;
    }

    static SExp sexp(String s, SExp ... nds) {
        return new SExp(s, Arrays.asList(nds));
    }

    static SExp sexp(String s, List<SExp> n) {
        return new SExp(s, n);
    }

    static SExp atom(String s) {
        return new SExp(s, true);
    }

    static SExp atomq(String s) {
        return atom("\"" + s + "\"");
    }

    public void pretty(Consumer<String> output) {
        pretty(output, "");
    }

    private boolean isFlat() {
        return this.exps.stream().allMatch(f -> f.getExps().isEmpty());
    }

    private void pretty(Consumer<String> output, String indent) {
        if(atom) {
            output.accept(getLabel());
            return;
        }

        if(isFlat()) {
            output.accept("(");
            output.accept(getLabel());
            if(exps.size() > 0)
                output.accept(" ");

            for(int i = 0; i < exps.size(); i++) {
                exps.get(i).pretty(output, indent);
                if(i != exps.size() - 1) {
                    output.accept(" ");
                }
            }
            output.accept(")" );
            return;
        }

        output.accept("(");
        output.accept(getLabel());
        output.accept("\n");

        indent += "  ";
        for(int i = 0; i < getExps().size(); i++) {
            output.accept(indent);
            getExps().get(i).pretty(output, indent);
            if(i != getExps().size() - 1)
                output.accept("\n");
        }
        output.accept(")");
    }
}
