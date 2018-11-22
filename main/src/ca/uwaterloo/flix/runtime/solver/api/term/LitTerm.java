package ca.uwaterloo.flix.runtime.solver.api.term;

import ca.uwaterloo.flix.runtime.solver.api.ProxyObject;

import java.util.concurrent.Callable;
import java.util.function.Function;

/**
 * Represents a literal.
 * <p>
 * The literal value is not directly accessible, but instead returned by invocation of the supplied function.
 */
public final class LitTerm implements Term {

    /**
     * The zero argument function which returns the literal when invoked.
     */
    private final Function<Object, ProxyObject> function;

    /**
     * Construct a literal term for the given function.
     */
    public LitTerm(Function<Object, ProxyObject> function) {
        if (function == null)
            throw new IllegalArgumentException("'function' must be non-null");

        this.function = function;
    }

    /**
     * Returns the function.
     */
    public Function<Object, ProxyObject> getFunction() {
        return function;
    }

    /**
     * Returns a human-readable representation of `this` term.
     */
    @Override
    public String toString() {
        return function.apply(new Object[1]).toString();
    }

}
