package flix.runtime.fixpoint.ram;

import java.io.PrintStream;

public class AttrTerm implements RamTerm {
    private LocalVariable localVar;
    private int index;

    public AttrTerm(LocalVariable localVar, int index) {
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
