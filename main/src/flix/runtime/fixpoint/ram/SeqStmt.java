package flix.runtime.fixpoint.ram;

/**
 * The statement "stmt1; stmt2"
 */
public class SeqStmt implements Stmt {
    private Stmt[] stmts;

    public SeqStmt(Stmt[] stmts) {
        this.stmts = stmts;
    }
}
