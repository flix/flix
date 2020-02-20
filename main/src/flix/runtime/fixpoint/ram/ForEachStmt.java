package flix.runtime.fixpoint.ram;

import java.io.PrintStream;

public class ForEachStmt implements Stmt {
    private TableName name;
    private LocalVariable localVar;
    private Stmt body;

    public ForEachStmt(TableName name, LocalVariable localVar, Stmt body) {
        this.name = name;
        this.localVar = localVar;
        this.body = body;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print("\t".repeat(indentLevel));
        stream.print("for each " + name + " as " +
                localVar.getVarName() + " do:\n");
        body.prettyPrint(stream, indentLevel + 1);
    }
}
