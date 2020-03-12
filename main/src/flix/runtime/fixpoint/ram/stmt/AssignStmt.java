package flix.runtime.fixpoint.ram.stmt;

import flix.runtime.fixpoint.ram.TableName;
import flix.runtime.fixpoint.ram.exp.relation.RelationExp;

import java.io.PrintStream;

public class AssignStmt implements Stmt {
    private TableName name;
    private RelationExp relationExp;

    public AssignStmt(TableName name, RelationExp relationExp) {
        this.name = name;
        this.relationExp = relationExp;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print("\t".repeat(indentLevel) + name.toString() + " := ");
        relationExp.prettyPrint(stream);
    }
}
