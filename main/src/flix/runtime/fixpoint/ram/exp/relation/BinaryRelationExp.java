package flix.runtime.fixpoint.ram.exp.relation;

import java.io.PrintStream;

public final class BinaryRelationExp implements RelationExp{
    private final BinaryRelationOperator operator;
    private final RelationExp exp1;
    private final RelationExp exp2;

    public BinaryRelationExp(BinaryRelationOperator operator, RelationExp exp1, RelationExp exp2) {
        this.operator = operator;
        this.exp1 = exp1;
        this.exp2 = exp2;
    }

    @Override
    public void prettyPrint(PrintStream stream) {
        exp1.prettyPrint(stream);
        if (operator == BinaryRelationOperator.UNION){
            stream.print(" U ");
        } else if (operator == BinaryRelationOperator.SUBTRACT){
            stream.print(" \\ ");
        }
        exp2.prettyPrint(stream);
    }
}
