package flix.runtime.fixpoint.ram;

import flix.runtime.ProxyObject;

public class LitTerm implements RamTerm {
    private ProxyObject literal;

    public LitTerm(ProxyObject literal) {
        this.literal = literal;
    }
}
