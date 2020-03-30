package flix.runtime.fixpoint.ram.stmt;

import flix.runtime.fixpoint.ram.exp.bool.BoolExp;

import java.io.PrintStream;

public final class IfStmt implements Stmt {
    private final BoolExp guard;
    private final Stmt bodyStmt;

    public IfStmt(BoolExp guard, Stmt bodyStmt) {
        if (guard == null) throw new IllegalArgumentException("'boolExp' must be non-null");
        if (bodyStmt == null) throw new IllegalArgumentException("'stmt' must be non-null");
        this.guard = guard;
        this.bodyStmt = bodyStmt;
    }

    public BoolExp getGuard() {
        return guard;
    }

    public Stmt getBodyStmt() {
        return bodyStmt;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print("\t".repeat(indentLevel));
        stream.print("if (");
        guard.prettyPrint(stream, indentLevel);
        stream.print(") then {\n");
        bodyStmt.prettyPrint(stream, indentLevel + 1);
        stream.print("\n" + "\t".repeat(indentLevel) + "}");
    }
}
