package flix.runtime.fixpoint.ram.exp.relation;

import java.io.PrintStream;

public final class BinaryRelationExp implements RelationExp {
    private final BinaryRelationOperator operator;
    private final RelationExp exp1;
    private final RelationExp exp2;

    public BinaryRelationExp(BinaryRelationOperator operator, RelationExp exp1, RelationExp exp2) {
        if (operator == null) throw new IllegalArgumentException("'operator' must be non-null");
        if (exp1 == null) throw new IllegalArgumentException("'exp1' must be non-null");
        if (exp2 == null) throw new IllegalArgumentException("'exp2' must be non-null");
        this.operator = operator;
        this.exp1 = exp1;
        this.exp2 = exp2;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        exp1.prettyPrint(stream, indentLevel);
        if (operator == BinaryRelationOperator.UNION) {
            stream.print(" U ");
        } else if (operator == BinaryRelationOperator.SUBTRACT) {
            stream.print(" \\ ");
        }
        exp2.prettyPrint(stream, indentLevel);
    }
}
