package flix.runtime.fixpoint.ram.exp.bool;

import java.io.PrintStream;

public class AndBoolExp implements BoolExp {

    private final BoolExp leftExp;
    private final BoolExp rightExp;

    public AndBoolExp(BoolExp leftExp, BoolExp rightExp) {
        this.leftExp = leftExp;
        this.rightExp = rightExp;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        leftExp.prettyPrint(stream, indentLevel);
        stream.print(" and ");
        rightExp.prettyPrint(stream, indentLevel);
    }
}
