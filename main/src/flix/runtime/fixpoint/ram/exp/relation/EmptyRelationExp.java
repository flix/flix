package flix.runtime.fixpoint.ram.exp.relation;

import java.io.PrintStream;

public final class EmptyRelationExp implements RelationExp {
    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print('Ø');
    }
}
