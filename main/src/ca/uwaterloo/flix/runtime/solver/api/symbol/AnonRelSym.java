package ca.uwaterloo.flix.runtime.solver.api.symbol;

import ca.uwaterloo.flix.runtime.solver.api.Attribute;

/**
 * Represents a fresh instance of a relation symbol.
 * <p>
 * Note: Equality is defined by identity since anon relation symbols differ based on their instance.
 */
public final class AnonRelSym implements PredSym {

    /**
     * The parent relation symbol.
     */
    private final RelSym parent;

    /**
     * Constructs a fresh anon relation symbol from its parent.
     */
    public AnonRelSym(RelSym parent) {
        this.parent = parent;
    }

    /**
     * Returns the name of the parent relation symbol.
     */
    public String getName() {
        return parent.getName();
    }

    /**
     * Returns the attributes of the parent relation symbol.
     */
    public Attribute[] getAttributes() {
        return parent.getAttributes();
    }

}
