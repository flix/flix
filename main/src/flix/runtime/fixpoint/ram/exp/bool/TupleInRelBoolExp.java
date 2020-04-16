package flix.runtime.fixpoint.ram.exp.bool;

import flix.runtime.fixpoint.ram.exp.relation.RelationExp;
import flix.runtime.fixpoint.ram.term.RamTerm;

import java.io.PrintStream;

public final class TupleInRelBoolExp implements BoolExp {
    private final RamTerm[] terms;
    private final RelationExp exp;

    public TupleInRelBoolExp(RamTerm[] terms, RelationExp exp) {
        if (terms == null)
            throw new IllegalArgumentException("'terms' must be non-null");
        if (exp == null) throw new IllegalArgumentException("'table' must be non-null");
        this.terms = terms;
        this.exp = exp;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print("(");
        for (int i = 0; i < terms.length; i++) {
            terms[i].prettyPrint(stream);
            if (i < terms.length - 1) {
                stream.print(", ");
            }
        }
        stream.print(") in ");
        exp.prettyPrint(stream, indentLevel);
    }

    public RamTerm[] getTerms() {
        return terms;
    }

    public RelationExp getExp() {
        return exp;
    }
}
