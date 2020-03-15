package flix.runtime.fixpoint.ram.exp.bool;

import java.io.PrintStream;

public final class BinaryBoolExp implements BoolExp {
    private final BinaryBoolOperator operator;
    private final BoolExp exp1;
    private final BoolExp exp2;

    public BinaryBoolExp(BinaryBoolOperator operator, BoolExp exp1, BoolExp exp2) {
        this.operator = operator;
        this.exp1 = exp1;
        this.exp2 = exp2;
    }

    @Override
    public void prettyPrint(PrintStream stream) {
        exp1.prettyPrint(stream);
        if (operator == BinaryBoolOperator.AND){
            stream.print(" and ");
        } else if (operator == BinaryBoolOperator.OR){
            stream.print(" or ");
        }
        exp2.prettyPrint(stream);

    }
}
