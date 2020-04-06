package flix.runtime.fixpoint.ram.exp.bool;

import java.io.PrintStream;

public final class NotBoolExp implements BoolExp {
    private final BoolExp exp;

    public NotBoolExp(BoolExp exp) {
        if (exp == null) throw new IllegalArgumentException("'epx' must be non-null");
        this.exp = exp;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print("Not ");
        exp.prettyPrint(stream, indentLevel);
    }

    public BoolExp getExp() {
        return exp;
    }
}
