package ca.uwaterloo.flix.runtime.solver.api.term;

import ca.uwaterloo.flix.runtime.solver.api.ProxyObject;

import java.util.concurrent.Callable;

/**
 * Represents a literal.
 * <p>
 * The literal value is not directly accessible, but instead returned by invocation of the supplied function.
 */
public final class LitTerm implements Term {

    /**
     * The zero argument function which returns the literal when invoked.
     */
    private Callable<ProxyObject> function;

    /**
     * Construct a literal term for the given function.
     */
    public LitTerm(Callable<ProxyObject> function) {
        if (function == null)
            throw new IllegalArgumentException("'function' must be non-null");
        this.function = function;
    }

    /**
     * Returns the function.
     */
    public Callable<ProxyObject> getFunction() {
        return function;
    }

    /**
     * Returns a human-readable representation of `this` term.
     */
    @Override
    public String toString() {
        return "<lit>";
    }
}
