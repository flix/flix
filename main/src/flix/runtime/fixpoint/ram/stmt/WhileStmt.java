package flix.runtime.fixpoint.ram.stmt;

import flix.runtime.fixpoint.ram.exp.bool.BoolExp;

import java.io.PrintStream;

public final class WhileStmt implements Stmt {
    private final BoolExp condition;
    private final Stmt body;

    public WhileStmt(BoolExp condition, Stmt body) {
        if (condition == null) throw new IllegalArgumentException("'condition' must be non-null");
        if (body == null) throw new IllegalArgumentException("'body' must be non.null");
        this.condition = condition;
        this.body = body;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print("\t".repeat(indentLevel) + "while (");
        condition.prettyPrint(stream);
        stream.print("){\n");
        body.prettyPrint(stream, indentLevel + 1);
        stream.print('\n' + "\t".repeat(indentLevel) + '}');
    }
}
