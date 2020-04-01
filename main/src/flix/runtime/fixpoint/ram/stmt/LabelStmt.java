package flix.runtime.fixpoint.ram.stmt;

import java.io.PrintStream;

public class LabelStmt implements Stmt {

    private final String label;

    public LabelStmt(String label) {
        this.label = label;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print("\t".repeat(indentLevel) + "\\\\" + label);
    }
}
