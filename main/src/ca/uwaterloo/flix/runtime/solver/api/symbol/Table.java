package ca.uwaterloo.flix.runtime.solver.api.symbol;

/**
 * A common super-type for predicate symbols.
 */
public interface Table {

    /**
     * Returns the name of the predicate symbol.
     */
    String getName();

}
