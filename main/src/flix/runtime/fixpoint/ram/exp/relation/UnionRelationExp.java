package flix.runtime.fixpoint.ram.exp.relation;

import java.io.PrintStream;

public class UnionRelationExp implements RelationExp {
    private final RelationExp exp1;
    private final RelationExp exp2;

    public UnionRelationExp(RelationExp exp1, RelationExp exp2) {
        this.exp1 = exp1;
        this.exp2 = exp2;
    }

    public RelationExp getExp1() {
        return exp1;
    }

    public RelationExp getExp2() {
        return exp2;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        exp1.prettyPrint(stream, indentLevel);
        stream.print(" U ");
        exp2.prettyPrint(stream, indentLevel);
    }
}
