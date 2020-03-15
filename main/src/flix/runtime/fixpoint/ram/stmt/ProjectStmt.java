package flix.runtime.fixpoint.ram.stmt;

import flix.runtime.fixpoint.ram.RamTerm;
import flix.runtime.fixpoint.ram.TableName;

import java.io.PrintStream;

public final class ProjectStmt implements Stmt {
    private final RamTerm[] facts;
    private final TableName table;

    public ProjectStmt(RamTerm[] facts, TableName table) {
        this.facts = facts;
        this.table = table;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print("\t".repeat(indentLevel) +
                "project (");
        for (int i = 0; i < facts.length; i++) {
            RamTerm fact = facts[i];
            fact.prettyPrint(stream);
            if (i < facts.length - 1){
                stream.print(", ");
            }
        }
        stream.print(") into " + table);
    }
}
