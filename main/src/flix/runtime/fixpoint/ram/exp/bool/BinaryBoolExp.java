package flix.runtime.fixpoint.ram.exp.bool;

import java.io.PrintStream;

public final class BinaryBoolExp implements BoolExp {
    private final BinaryBoolOperator operator;
    private final BoolExp exp1;
    private final BoolExp exp2;

    public BinaryBoolExp(BinaryBoolOperator operator, BoolExp exp1, BoolExp exp2) {
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
        if (operator == BinaryBoolOperator.AND) {
            stream.print(" and ");
        } else if (operator == BinaryBoolOperator.OR) {
            stream.print(" or ");
        }
        exp2.prettyPrint(stream, indentLevel);

    }
}
