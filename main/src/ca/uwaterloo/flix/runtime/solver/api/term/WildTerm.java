package ca.uwaterloo.flix.runtime.solver.api.term;

/**
 * Represents a wildcard term.
 */
public final class WildTerm implements Term {

    /**
     * Returns a human-readable representation of `this` wildcard term.
     */
    @Override
    public String toString() {
        return "_";
    }

}
