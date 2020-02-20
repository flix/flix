package flix.runtime.fixpoint.ram;

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
}
