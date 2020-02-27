package flix.runtime.fixpoint.ram;

import java.io.PrintStream;

public class UnaryBoolExp implements BoolExp {
    UnaryBoolOperator operator;
    Exp exp;

    public UnaryBoolExp(UnaryBoolOperator operator, Exp exp) {
        this.operator = operator;
        this.exp = exp;
    }

    @Override
    public void prettyPrint(PrintStream stream) {
        stream.print(operator + " ");
        exp.prettyPrint(stream);
    }
}
