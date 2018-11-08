package ca.uwaterloo.flix.runtime.solver.api.predicate;

import ca.uwaterloo.flix.runtime.solver.api.ProxyObject;
import ca.uwaterloo.flix.runtime.solver.api.symbol.VarSym;
import flix.runtime.ReifiedSourceLocation;

import java.util.function.Function;

/**
 * Represents a functional predicate of the form: sym <- function(args).
 */
public final class FunctionalPredicate implements Predicate {

    /**
     * The variable.
     */
    private final VarSym sym;

    /**
     * The function.
     */
    private final Function<Object[], ProxyObject[]> function; // TODO: Why are the arguments not proxy objects?

    /**
     * The function arguments.
     */
    private final VarSym[] arguments;

    /**
     * The reified source location.
     */
    private final ReifiedSourceLocation location; // TODO: Do we want these?

    /**
     * Constructs a functional predicate for the given symbol, function, and function arguments.
     */
    public FunctionalPredicate(VarSym sym, Function<Object[], ProxyObject[]> function, VarSym[] arguments, ReifiedSourceLocation location) {
        if (sym == null)
            throw new IllegalArgumentException("'sym' must be non-null.");
        if (function == null)
            throw new IllegalArgumentException("'function' must be non-null.");
        if (arguments == null) {
            throw new IllegalArgumentException("'arguments' must be non-null.");
        }

        this.sym = sym;
        this.function = function;
        this.arguments = arguments;
        this.location = location;
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
