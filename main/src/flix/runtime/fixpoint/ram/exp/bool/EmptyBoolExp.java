package flix.runtime.fixpoint.ram.exp.bool;

import flix.runtime.fixpoint.ram.exp.relation.RelationExp;

import java.io.PrintStream;

public final class EmptyBoolExp implements BoolExp {
    private final RelationExp relExp;

    public EmptyBoolExp(RelationExp relExp) {
        if (relExp == null) throw new IllegalArgumentException("'relExp' must be non-null");
        this.relExp = relExp;
    }

    @Override
    public void prettyPrint(PrintStream stream) {
        stream.print("Empty ");
        relExp.prettyPrint(stream);
    }
}
