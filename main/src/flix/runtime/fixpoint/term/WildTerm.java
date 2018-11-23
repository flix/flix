package flix.runtime.fixpoint.term;

/**
 * Represents a wildcard term.
 */
public final class WildTerm implements Term {

    /**
     * Private singleton instance.
     */
    private static final WildTerm INSTANCE = new WildTerm();

    /**
     * Returns the singleton instance of the wildcard term.
     */
    public static WildTerm getSingleton() {
        return INSTANCE;
    }

    /**
     * Private constructor.
     */
    private WildTerm() {
        /* empty constructor */
    }

    /**
     * Returns a human-readable representation of `this` term.
     */
    @Override
    public String toString() {
        return "_";
    }

}
