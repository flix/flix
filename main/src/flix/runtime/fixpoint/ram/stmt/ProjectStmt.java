package flix.runtime.fixpoint.ram.stmt;

import flix.runtime.fixpoint.ram.exp.relation.TableName;
import flix.runtime.fixpoint.ram.term.RamTerm;

import java.io.PrintStream;

public final class ProjectStmt implements Stmt {
    private final RamTerm[] facts;
    private final TableName table;

    public ProjectStmt(RamTerm[] terms, TableName table) {
        if (terms == null)
            throw new IllegalArgumentException("'terms' must be non-null");
        if (table == null) throw new IllegalArgumentException("'table' must be non-null");
        this.facts = terms;
        this.table = table;
    }

    public RamTerm[] getFacts() {
        return facts.clone();
    }

    public TableName getTable() {
        return table;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print("\t".repeat(indentLevel) +
                "project (");
        for (int i = 0; i < facts.length; i++) {
            RamTerm fact = facts[i];
            fact.prettyPrint(stream);
            if (i < facts.length - 1) {
                stream.print(", ");
            }
        }
        stream.print(") into ");
        table.prettyPrint(stream, indentLevel);
    }
}
