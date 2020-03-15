package flix.runtime.fixpoint.ram.exp.bool;

import flix.runtime.fixpoint.ram.RamTerm;

import java.io.PrintStream;

public final class EqualsBoolExp implements BoolExp {
    private final RamTerm term1;
    private final RamTerm term2;

    public EqualsBoolExp(RamTerm term1, RamTerm term2) {
        this.term1 = term1;
        this.term2 = term2;
    }

    @Override
    public void prettyPrint(PrintStream stream) {
        term1.prettyPrint(stream);
        stream.print(" = ");
        term2.prettyPrint(stream);
    }
}
