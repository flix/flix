package ca.uwaterloo.flix.runtime.solver.api;

import java.util.function.Function;

/**
 * A proxy object wraps a raw Flix object with appropriate methods for equality, hashCode, and toString.
 */
public final class ProxyObject {

    // TODO: Document class.

    private final Object value;
    private final Function<Object[], Boolean> eq;
    private final Function<Object[], Integer> hash;
    private final Function<Object[], String> toStr;

    public ProxyObject(Object value, Function<Object[], Boolean> eq, Function<Object[], Integer> hash, Function<Object[], String> toStr) {
        //if (value == null) throw new IllegalArgumentException("'value' must be non-null.");

        this.value = value;
        this.eq = eq;
        this.hash = hash;
        this.toStr = toStr;
    }

    public Object getValue() {
        return value;
    }

    @Override
    public boolean equals(Object that) {
        if (eq == null)
            return this.getValue().equals(((ProxyObject) that).getValue());
        else
            return eq.apply(new Object[]{this.getValue(), ((ProxyObject) that).getValue()});
    }

    @Override
    public int hashCode() {
        if (hash == null)
            return this.getValue().hashCode();
        else
            return hash.apply(new Object[]{value});
    }

    @Override
    public String toString() {
        if (toStr == null)
            return this.getValue().toString();
        else
            return toStr.apply(new Object[]{value});
    }

}
