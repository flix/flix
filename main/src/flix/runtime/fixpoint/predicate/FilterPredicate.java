package flix.runtime.fixpoint.predicate;

import flix.runtime.ProxyObject;
import flix.runtime.fixpoint.term.Term;

import java.util.function.Function;

/**
 * Represents a filter predicate with function `f` and arguments `terms`.
 */
public final class FilterPredicate implements Predicate {

    /**
     * Constructs a filter predicate for the given function and function arguments.
     */
    public static FilterPredicate of(Function<Object[], ProxyObject> function, Term[] arguments) {
        if (function == null)
            throw new IllegalArgumentException("'function' must be non-null.");

        if (arguments == null)
            throw new IllegalArgumentException("'arguments' must be non-null.");

        return new FilterPredicate(function, arguments);
    }

    /**
     * The function.
     */
    private final Function<Object[], ProxyObject> function;

    /**
     * The function arguments.
     */
    private final Term[] arguments;

    /**
     * Private constructor.
     */
    private FilterPredicate(Function<Object[], ProxyObject> function, Term[] arguments) {
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
        return "<filter>";
    }

}
