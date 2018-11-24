package ca.uwaterloo.flix.runtime.solver.api.symbol;

/**
 * A common super-type for predicate symbols.
 */
public interface PredSym {

    /**
     * Returns the name of the predicate symbol.
     */
    String getName();

    /**
     * Returns the parameterless version of the predicate symbol.
     */
    PredSym getParameterless();

}
