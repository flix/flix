package ca.uwaterloo.flix.runtime.solver.api.term;

import ca.uwaterloo.flix.runtime.solver.api.ProxyObject;
import flix.runtime.fixpoint.symbol.VarSym;

import java.util.function.Function;

/**
 * Represents a function application term.
 */
public final class AppTerm implements Term {

    /**
     * The function.
     */
    private final Function<Object[], ProxyObject> function;

    /**
     * The function arguments.
     */
    private final VarSym[] arguments;

    /**
     * Constructs a function application term for the given function and arguments.
     */
    public AppTerm(Function<Object[], ProxyObject> function, VarSym[] arguments) {
        if (function == null)
            throw new IllegalArgumentException("'function' must be non-null");
        if (arguments == null)
            throw new IllegalArgumentException("'arguments' must be non-null.");

        this.function = function;
        this.arguments = arguments;
    }

    /**
     * Returns the function.
     */
    public Function<Object[], ProxyObject> getFunction() {
        return function;
    }

    /**
     * Returns the function arguments.
     */
    public VarSym[] getArguments() {
        return arguments;
    }

    /**
     * Returns a human-readable representation of `this` term.
     */
    @Override
    public String toString() {
        return "f(<>)";
    }

}
