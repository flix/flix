package flix.runtime.fixpoint.ram.exp.bool;

import java.io.PrintStream;

public final class NotBoolExp implements BoolExp {
    private final BoolExp exp;

    public NotBoolExp(BoolExp exp) {
        this.exp = exp;
    }

    @Override
    public void prettyPrint(PrintStream stream) {
        stream.print("Not ");
        exp.prettyPrint(stream);
    }
}
