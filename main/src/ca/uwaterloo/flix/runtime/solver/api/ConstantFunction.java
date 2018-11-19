package ca.uwaterloo.flix.runtime.solver.api;

import java.util.function.Function;

/**
 * Represents a constant function.
 */
public class ConstantFunction implements Function<Object, ProxyObject> {

    /**
     * The value the function always returns.
     */
    private final ProxyObject value;

    /**
     * Constructs a constant function for the given `value`.
     */
    public ConstantFunction(ProxyObject value) {
        if (value == null)
            throw new RuntimeException("'value' must be non-null.");

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
