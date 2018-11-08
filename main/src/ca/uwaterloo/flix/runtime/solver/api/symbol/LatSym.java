package ca.uwaterloo.flix.runtime.solver.api.symbol;

import ca.uwaterloo.flix.runtime.solver.api.LatticeOps;
import ca.uwaterloo.flix.runtime.solver.api.Attribute;
import ca.uwaterloo.flix.runtime.solver.api.symbol.Table;

/**
 * Represents a lattice value.
 */
public final class LatSym implements Table {

    /**
     * The unique name of the lattice symbol.
     */
    private final String uniqueName;

    /**
     * The keys of the lattice symbol.
     */
    private final Attribute[] keys;

    /**
     * The value of the lattice symbol.
     */
    private final Attribute value;

    /**
     * The lattice operations associated with the lattice symbol.
     */
    private final LatticeOps ops;

    /**
     * Returns the lattice symbol with the given unique name.
     */
    public LatSym(String uniqueName, Attribute[] keys, Attribute value, LatticeOps ops) {
        if (uniqueName == null)
            throw new IllegalArgumentException("'uniqueName' must be non-null.");
        if (keys == null)
            throw new IllegalArgumentException("'keys' must be non-null.");
        if (value == null)
            throw new IllegalArgumentException("'value' must be non-null.");
        if (ops == null)
            throw new IllegalArgumentException("'ops' must be non-null.");

        this.uniqueName = uniqueName;
        this.keys = keys;
        this.value = value;
        this.ops = ops;
    }

    /**
     * Returns the name of the lattice symbol.
     */
    public String getName() {
        return uniqueName;
    }

    /**
     * Returns the keys of the lattice symbol.
     */
    public Attribute[] getKeys() {
        return keys;
    }

    /**
     * Returns the value of the lattice symbol.
     */
    public Attribute getValue() {
        return value;
    }

    /**
     * Returns the associated lattice operations.
     */
    public LatticeOps getOps() {
        return ops;
    }

}

