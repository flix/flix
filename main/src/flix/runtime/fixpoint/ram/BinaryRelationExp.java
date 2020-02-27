package flix.runtime.fixpoint.ram;

import java.io.PrintStream;

public class BinaryRelationExp implements RelationExp{
    BinaryRelationOperator operator;
    RelationExp exp1;
    RelationExp exp2;

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
