package flix.runtime.fixpoint.ram.stmt;

import flix.runtime.fixpoint.ram.RowVariable;
import flix.runtime.fixpoint.ram.TableName;

import java.io.PrintStream;

public final class ForEachStmt implements Stmt {
    private final TableName name;
    private final RowVariable localVar;
    private final Stmt body;

    public ForEachStmt(TableName name, RowVariable localVar, Stmt body) {
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
