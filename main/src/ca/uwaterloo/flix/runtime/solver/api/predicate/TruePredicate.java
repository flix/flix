package ca.uwaterloo.flix.runtime.solver.api.predicate;

/**
 * Represents the true predicate.
 */
public final class TruePredicate implements Predicate {

    /**
     * Returns a human-readable representation of `this` predicate.
     */
    @Override
    public String toString() {
        return "true.";
    }

}
