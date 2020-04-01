package flix.runtime.fixpoint.ram.stmt;

import java.io.PrintStream;
import java.util.ArrayList;

/**
 * The statement "stmt1; stmt2"
 */
public final class SeqStmt implements Stmt {
    private final Stmt[] stmts;

    public SeqStmt(Stmt[] stmts) {
        if (stmts == null || stmts.length < 1)
            throw new IllegalArgumentException("'stmts' must be non-null and non-empty");
        this.stmts = stmts;
    }

    public SeqStmt(ArrayList<Stmt> stmts) {
        if (stmts == null || stmts.size() < 1)
            throw new IllegalArgumentException("'stmts' must be non-null and non-empty");
        this.stmts = stmts.toArray(Stmt[]::new);
    }

    public Stmt[] getStmts() {
        return stmts.clone();
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        for (int i = 0; i < stmts.length; i++) {
            Stmt stmt = stmts[i];
            if (stmt == null) {
                stream.println("PrettyPrint failed in SeqStmt at: " + (i + 1) + "\n but there was actually: " + stmts.length);
                return;
            }
            stmt.prettyPrint(stream, indentLevel);
            if (i < stmts.length - 1) {
                stream.print(";\n");
            }
        }
    }
}
