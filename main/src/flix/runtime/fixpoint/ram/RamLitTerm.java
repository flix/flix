package flix.runtime.fixpoint.ram;

import flix.runtime.ProxyObject;

import java.io.PrintStream;

public class RamLitTerm implements RamTerm {
    private ProxyObject literal;

    public RamLitTerm(ProxyObject literal) {
        this.literal = literal;
    }

    @Override
    public void prettyPrint(PrintStream stream) {
        stream.print(literal);
    }
}
