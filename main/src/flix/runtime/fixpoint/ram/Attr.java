package flix.runtime.fixpoint.ram;

public class Attr {
    private LocalVariable localVar;
    private int index;

    public Attr(LocalVariable localVar, int index) {
        this.localVar = localVar;
        this.index = index;
    }
}
