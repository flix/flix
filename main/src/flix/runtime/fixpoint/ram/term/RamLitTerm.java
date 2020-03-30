package flix.runtime.fixpoint.ram.term;

import flix.runtime.ProxyObject;

import java.io.PrintStream;

public final class RamLitTerm implements RamTerm {
    private final ProxyObject literal;

    public RamLitTerm(ProxyObject literal) {
        if (literal == null) throw new IllegalArgumentException("'literal' must be non-null");
        this.literal = literal;
    }

    public ProxyObject getLiteral() {
        return literal;
    }

    @Override
    public void prettyPrint(PrintStream stream) {
        stream.print(literal);
    }
}
