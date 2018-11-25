package flix.runtime.fixpoint;

import flix.runtime.ProxyObject;

import java.util.function.Function;

/**
 * Represents a constant function.
 */
public final class ConstantFunction implements Function<Object, ProxyObject> {

    /**
     * Constructs a constant function for the given `value`.
     */
    public static ConstantFunction of(ProxyObject value) {
        if (value == null)
            throw new RuntimeException("'value' must be non-null.");

        return new ConstantFunction(value);
    }

    /**
     * The value the function always returns.
     */
    private final ProxyObject value;

    /**
     * Private constructor.
     */
    private ConstantFunction(ProxyObject value) {
        this.value = value;
    }

    /**
     * Returns the value regardless of the argument.
     */
    @Override
    public ProxyObject apply(Object o) {
        return value;
    }

}
