package ca.uwaterloo.flix.runtime.solver.api.predicate;

/**
 * Represents the false predicate.
 */
public final class FalsePredicate implements Predicate {

    /**
     * Returns a human-readable representation of `this` predicate.
     */
    @Override
    public String toString() {
        return "false.";
    }

}
