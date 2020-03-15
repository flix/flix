package flix.runtime.fixpoint.ram.stmt;

import flix.runtime.fixpoint.ram.TableName;
import flix.runtime.fixpoint.ram.exp.relation.RelationExp;

import java.io.PrintStream;

public final class AssignStmt implements Stmt {
    private final TableName name;
    private final RelationExp relationExp;

    public AssignStmt(TableName name, RelationExp relationExp) {
        if (name == null) throw new IllegalArgumentException("'name' must be non-null");
        if (relationExp == null) throw new IllegalArgumentException("'relationExp' must be non-null");
        this.name = name;
        this.relationExp = relationExp;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print("\t".repeat(indentLevel) + name.toString() + " := ");
        relationExp.prettyPrint(stream);
    }
}
