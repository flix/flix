package flix.runtime;

import flix.runtime.value.Unit;

import java.util.function.Function;

/**
 * A wrapper for objects created natively by Flix with appropriate equals, hashCode, and toString methods.
 */
public final class ProxyObject {

    /**
     * The Unit value as a proxy object.
     */
    public static final ProxyObject UNIT = new ProxyObject(Unit.getInstance(), null, null, null);

    /**
     * The wrapped (possibly boxed) value.
     */
    private final Object value;

    /**
     * The equality function associated with the value.
     */
    private final Function<Object[], ProxyObject> eq;

    /**
     * The hash function associated with the value.
     */
    private final Function<Object[], ProxyObject> hash;

    /**
     * The toString function associated with the value.
     */
    private final Function<Object[], ProxyObject> toStr;

    /**
     * Constructs a proxy object of the given value and associated methods.
     *
     * @param value the value.
     * @param eq    the equality function. If null, uses the equality of the value itself.
     * @param hash  the equality function. If null, uses the hash of the value itself.
     * @param toStr the equality function. If null, uses the toString of the value itself.
     */
    public static ProxyObject of(Object value, Function<Object[], ProxyObject> eq, Function<Object[], ProxyObject> hash, Function<Object[], ProxyObject> toStr) {
        return new ProxyObject(value, eq, hash, toStr);
    }

    /**
     * Private constructor.
     */
    private ProxyObject(Object value, Function<Object[], ProxyObject> eq, Function<Object[], ProxyObject> hash, Function<Object[], ProxyObject> toStr) {
        this.value = value;
        this.eq = eq;
        this.hash = hash;
        this.toStr = toStr;
    }

    /**
     * Returns the underlying value.
     */
    public Object getValue() {
        return value;
    }

    /**
     * Returns `true` if `this` object is equal to `that` object. Uses `eq` if available. Otherwise the equality of the value itself.
     */
    @Override
    public boolean equals(Object that) {
        if (eq == null)
            return this.getValue().equals(((ProxyObject) that).getValue());
        else {
            ProxyObject result = eq.apply(new Object[]{this.getValue(), ((ProxyObject) that).getValue()});
            return (Boolean) result.getValue();
        }
    }

    /**
     * Returns the hash code of `this` object. Uses `hash` if available. Otherwise uses the hash of the value itself.
     */
    @Override
    public int hashCode() {
        if (hash == null)
            return value.hashCode();
        else {
            ProxyObject result = hash.apply(new Object[]{value});
            return (Integer) result.getValue();
        }
    }

    /**
     * Returns the string representation of `this` object. Uses `toStr` if available. Otherwise uses the toString of the value itself.
     */
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
