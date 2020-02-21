package flix.runtime.fixpoint.ram;

import java.io.PrintStream;

/**
 * The statement "stmt1; stmt2"
 */
public class SeqStmt implements Stmt {
    private Stmt[] stmts;

    public SeqStmt(Stmt[] stmts) {
        this.stmts = stmts;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        for (Stmt stmt : stmts) {
            stmt.prettyPrint(stream, indentLevel);

            stream.print(";\n");
        }
    }
}
