package flix.runtime.fixpoint.ram.stmt;

import flix.runtime.fixpoint.ram.exp.bool.BoolExp;

import java.io.PrintStream;

public class IfStmt implements Stmt {
    BoolExp boolExp;
    Stmt stmt;

    public IfStmt(BoolExp boolExp, Stmt stmt) {
        this.boolExp = boolExp;
        this.stmt = stmt;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print("\t".repeat(indentLevel));
        stream.print("if (");
        boolExp.prettyPrint(stream);
        stream.print(") then {\n");
        stmt.prettyPrint(stream, indentLevel + 1);
        stream.print("\n" + "\t".repeat(indentLevel) + "}");
    }
}
