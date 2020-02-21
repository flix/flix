package flix.runtime.fixpoint.ram;

import flix.runtime.ProxyObject;

import java.io.PrintStream;

public class LitTerm implements RamTerm {
    private ProxyObject literal;

    public LitTerm(ProxyObject literal) {
        this.literal = literal;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentation) {
        stream.print(literal);
    }
}
