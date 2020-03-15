package flix.runtime.fixpoint.ram.stmt;

import flix.runtime.fixpoint.ram.exp.bool.BoolExp;

import java.io.PrintStream;

public final class IfStmt implements Stmt {
    private final BoolExp boolExp;
    private final Stmt stmt;

    public IfStmt(BoolExp boolExp, Stmt stmt) {
        if (boolExp == null) throw new IllegalArgumentException("'boolExp' must be non-null");
        if (stmt == null) throw new IllegalArgumentException("'stmt' must be non-null");
        this.boolExp = boolExp;
        this.stmt = stmt;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print("\t".repeat(indentLevel));
        stream.print("if (");
        boolExp.prettyPrint(stream);
        stream.print(") then {\n");
        stmt.prettyPrint(stream, indentLevel + 1);
        stream.print("\n" + "\t".repeat(indentLevel) + "}");
    }
}
