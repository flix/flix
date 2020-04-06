package flix.runtime.fixpoint.ram.exp.bool;

import flix.runtime.fixpoint.ram.exp.relation.RelationExp;

import java.io.PrintStream;

public final class EmptyBoolExp implements BoolExp {
    private final RelationExp relExp;

    public EmptyBoolExp(RelationExp relExp) {
        if (relExp == null) throw new IllegalArgumentException("'relExp' must be non-null");
        this.relExp = relExp;
    }

    public RelationExp getRelExp() {
        return relExp;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print("Empty ");
        relExp.prettyPrint(stream, indentLevel);
    }
}
