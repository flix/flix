package flix.runtime.fixpoint.ram;

import java.io.PrintStream;
import java.util.Objects;

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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        AttrTerm attrTerm = (AttrTerm) o;
        return index == attrTerm.index &&
                localVar.equals(attrTerm.localVar);
    }

    @Override
    public int hashCode() {
        return Objects.hash(localVar, index);
    }
}
