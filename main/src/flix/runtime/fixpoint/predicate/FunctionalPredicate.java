package flix.runtime.fixpoint.predicate;

import ca.uwaterloo.flix.runtime.solver.api.ProxyObject;
import flix.runtime.fixpoint.symbol.VarSym;
import flix.runtime.fixpoint.predicate.Predicate;

import java.util.function.Function;

/**
 * Represents a functional predicate of the form: sym <- function(args).
 */
public final class FunctionalPredicate implements Predicate {

    /**
     * Constructs a functional predicate for the given symbol, function, and function arguments.
     */
    public static FunctionalPredicate of(VarSym sym, Function<Object[], ProxyObject[]> function, VarSym[] arguments) {
        if (sym == null)
            throw new IllegalArgumentException("'sym' must be non-null.");
        if (function == null)
            throw new IllegalArgumentException("'function' must be non-null.");
        if (arguments == null) {
            throw new IllegalArgumentException("'arguments' must be non-null.");
        }

        return new FunctionalPredicate(sym, function, arguments);
    }

    /**
     * The variable.
     */
    private final VarSym sym;

    /**
     * The function.
     */
    private final Function<Object[], ProxyObject[]> function;

    /**
     * The function arguments.
     */
    private final VarSym[] arguments;

    /**
     * Private constructor.
     */
    private FunctionalPredicate(VarSym sym, Function<Object[], ProxyObject[]> function, VarSym[] arguments) {
        this.sym = sym;
        this.function = function;
        this.arguments = arguments;
    }

    /**
     * Returns the variable symbol.
     */
    public VarSym getVarSym() {
        return sym;
    }

    /**
     * Returns the function.
     */
    public Function<Object[], ProxyObject[]> getFunction() {
        return function;
    }

    /**
     * Returns the function arguments.
     */
    public VarSym[] getArguments() {
        return arguments;
    }

    /**
     * Returns a human-readable representation of `this` predicate.
     */
    @Override
    public String toString() {
        return "<functional>";
    }

}
