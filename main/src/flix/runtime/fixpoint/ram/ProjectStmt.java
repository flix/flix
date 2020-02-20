package flix.runtime.fixpoint.ram;

import java.io.PrintStream;

public class ProjectStmt implements Stmt {
    private RamTerm[] facts;
    private TableName table;

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
            stream.print(fact);
            if (i < facts.length - 1){
                stream.print(", ");
            }
        }
        stream.print(") into " + table);
    }
}
