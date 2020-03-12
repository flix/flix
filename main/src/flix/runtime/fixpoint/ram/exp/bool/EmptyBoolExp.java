package flix.runtime.fixpoint.ram.exp.bool;

import flix.runtime.fixpoint.ram.exp.relation.RelationExp;

import java.io.PrintStream;

public class EmptyBoolExp implements BoolExp {
    RelationExp relExp;

    public EmptyBoolExp(RelationExp relExp) {
        this.relExp = relExp;
    }

    @Override
    public void prettyPrint(PrintStream stream) {
        stream.print("Empty ");
        relExp.prettyPrint(stream);
    }
}
