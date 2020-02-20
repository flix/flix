package flix.runtime.fixpoint.ram;

public class ProjectStmt implements Stmt {
    private RamTerm[] fact;
    private TableName table;

    public ProjectStmt(RamTerm[] fact, TableName table) {
        this.fact = fact;
        this.table = table;
    }
}
