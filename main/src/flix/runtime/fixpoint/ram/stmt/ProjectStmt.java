package flix.runtime.fixpoint.ram.stmt;

import flix.runtime.fixpoint.ram.RamTerm;
import flix.runtime.fixpoint.ram.TableName;

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
            fact.prettyPrint(stream, indentLevel);
            if (i < facts.length - 1){
                stream.print(", ");
            }
        }
        stream.print(") into " + table);
    }
}
