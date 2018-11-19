package ca.uwaterloo.flix.runtime.solver.api;

import java.util.function.Function;

/**
 * A proxy object wraps a raw Flix object with appropriate methods for equality, hashCode, and toString.
 */
public final class ProxyObject {

    // TODO: Document class.

    private final Object value;
    private final Function<Object[], ProxyObject> eq;
    private final Function<Object[], ProxyObject> hash;
    private final Function<Object[], ProxyObject> toStr;

    public ProxyObject(Object value, Function<Object[], ProxyObject> eq, Function<Object[], ProxyObject> hash, Function<Object[], ProxyObject> toStr) {
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
        else {
            ProxyObject result = eq.apply(new Object[]{this.getValue(), ((ProxyObject) that).getValue()});
            return (Boolean) result.getValue();
        }
    }

    @Override
    public int hashCode() {
        if (hash == null)
            return value.hashCode();
        else {
            ProxyObject result = hash.apply(new Object[]{value});
            return (Integer) result.getValue();
        }
    }

    @Override
    public String toString() {
        if (toStr == null)
            return value.toString();
        else {
            ProxyObject result = toStr.apply(new Object[]{value});
            return (String) result.getValue();
        }
    }

}
