package ca.uwaterloo.flix.runtime.solver.api.symbol;

import ca.uwaterloo.flix.runtime.solver.api.LatticeOps;
import ca.uwaterloo.flix.runtime.solver.api.Attribute;

import java.util.HashMap;
import java.util.Map;

/**
 * Represents a lattice value.
 * <p>
 * Note: Equality is defined by identity due to the internal cache.
 */
public final class LatSym implements PredSym {

    /**
     * An internal cache of lattice symbols.
     * <p>
     * Note: We never have to garbage collect these since there is only a small finite number of global lattice symbols.
     */
    private static final Map<String, LatSym> INTERNAL_CACHE = new HashMap<>();

    /**
     * Returns the relation symbol with the given unique name.
     */
    public static LatSym getInstance(String uniqueName, Attribute[] keys, Attribute value, LatticeOps ops) {
        var lookup = INTERNAL_CACHE.get(uniqueName);
        if (lookup != null) {
            return lookup;
        }
        var sym = new LatSym(uniqueName, keys, value, ops);
        INTERNAL_CACHE.put(uniqueName, sym);
        return sym;
    }

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
    private LatSym(String uniqueName, Attribute[] keys, Attribute value, LatticeOps ops) {
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

    /* equality by identity */

}

