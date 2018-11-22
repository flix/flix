package ca.uwaterloo.flix.runtime.solver.api.predicate;

import ca.uwaterloo.flix.runtime.solver.api.ProxyObject;
import ca.uwaterloo.flix.runtime.solver.api.term.Term;
import flix.runtime.fixpoint.predicate.Predicate;

import java.util.function.Function;

/**
 * Represents a filter predicate with function `f` and arguments `terms`.
 */
public final class FilterPredicate implements Predicate {

    /**
     * The function.
     */
    private final Function<Object[], ProxyObject> function;

    /**
     * The function arguments.
     */
    private final Term[] arguments;

    /**
     * Constructs a filter predicate for the given function and function arguments.
     */
    public FilterPredicate(Function<Object[], ProxyObject> function, Term[] arguments) {
        if (function == null)
            throw new IllegalArgumentException("'function' must be non-null.");

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
     * Returns the arguments.
     */
    public Term[] getArguments() {
        return arguments;
    }

    /**
     * Returns a human-readable representation of `this` predicate.
     */
    @Override
    public String toString() {
        return "if <filter>";
    }

}
