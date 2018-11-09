package ca.uwaterloo.flix.runtime.solver.api.symbol;

import ca.uwaterloo.flix.runtime.solver.api.Attribute;
import ca.uwaterloo.flix.runtime.solver.api.LatticeOps;

/**
 * Represents a fresh instance of a relation symbol.
 * <p>
 * Note: Equality is defined by identity since anon lattice symbols differ based on their instance.
 */
public final class AnonLatSym implements PredSym {

    /**
     * The parent lattice symbol.
     */
    private final LatSym parent;

    /**
     * Constructs a fresh anon lattice symbol with the given parent.
     */
    public AnonLatSym(LatSym parent) {
        this.parent = parent;
    }

    /**
     * Returns the name of the parent lattice symbol.
     */
    public String getName() {
        return parent.getName();
    }

    /**
     * Returns the keys of the parent lattice symbol.
     */
    public Attribute[] getKeys() {
        return parent.getKeys();
    }

    /**
     * Returns the value of the parent lattice symbol.
     */
    public Attribute getValue() {
        return parent.getValue();
    }

    /**
     * Returns the lattice operations associated with the parent lattice symbol.
     */
    public LatticeOps getOps() {
        return parent.getOps();
    }

    /* equality by identity */

}
