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
        for (int i = 0; i < stmts.length; i++) {
            Stmt stmt = stmts[i];
            stmt.prettyPrint(stream, indentLevel);
            if (i < stmts.length - 1) {
                stream.print(";\n");
            }
        }
        stream.print('\n');
    }
}
