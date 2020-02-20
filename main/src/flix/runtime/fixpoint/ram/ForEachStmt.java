package flix.runtime.fixpoint.ram;

public class ForEachStmt implements Stmt {
    private TableName name;
    private LocalVariable localVar;
    private Stmt body;

    public ForEachStmt(TableName name, LocalVariable localVar, Stmt body) {
        this.name = name;
        this.localVar = localVar;
        this.body = body;
    }
}
