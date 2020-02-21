package flix.runtime.fixpoint.ram;

import java.io.PrintStream;

public class Attr implements RamTerm {
    private LocalVariable localVar;
    private int index;

    public Attr(LocalVariable localVar, int index) {
        this.localVar = localVar;
        this.index = index;
    }

    @Override
    public String toString() {
        return localVar.getVarName() +
                "[" + index + ']';
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentation) {
        stream.print(localVar.getVarName() +
                "[" + index + ']');
    }
}
