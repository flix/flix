package ca.uwaterloo.flix.runtime.solver.api.symbol;

import ca.uwaterloo.flix.runtime.solver.api.LatticeOps;
import flix.runtime.fixpoint.Attribute;
import flix.runtime.ProxyObject;
import flix.runtime.fixpoint.symbol.PredSym;

import java.util.HashMap;
import java.util.Map;

/**
 * Represents a parameterized lattice symbol.
 */
public final class LatSym implements PredSym {

    /**
     * An internal cache of lattice symbols.
     */
    private static final Map<NameAndParameter, LatSym> INTERNAL_CACHE = new HashMap<>();

    /**
     * Returns the lattice symbol for the given `name` with the given `parameter` and `attributes`.
     * <p>
     * The parameter may be null.
     */
    public static LatSym of(String name, ProxyObject parameter, Attribute[] keys, Attribute value, LatticeOps ops) {
        if (name == null)
            throw new IllegalArgumentException("'name' must be non-null.");
        if (keys == null)
            throw new IllegalArgumentException("'keys' must be non-null.");
        if (value == null)
            throw new IllegalArgumentException("'value' must be non-null.");
        if (ops == null)
            throw new IllegalArgumentException("'ops' must be non-null.");

        var key = new NameAndParameter(name, parameter);
        var lookup = INTERNAL_CACHE.get(key);
        if (lookup != null) {
            return lookup;
        }
        var sym = new LatSym(name, parameter, keys, value, ops);
        INTERNAL_CACHE.put(key, sym);
        return sym;
    }

    /**
     * The name of the lattice symbol.
     */
    private final String name;

    /**
     * The parameter of the lattice symbol.
     */
    private final ProxyObject parameter;

    /**
     * The keys of the lattice symbol.
     */
    private final Attribute[] keys;

    /**
     * The value of the lattice symbol.
     */
    private final Attribute value;

    /**
     * The lattice operations of the lattice symbol.
     */
    private final LatticeOps ops;

    /**
     * Constructs a fresh lattice symbol with the given `name` and `parameter`.
     */
    private LatSym(String name, ProxyObject parameter, Attribute[] keys, Attribute value, LatticeOps ops) {
        this.name = name;
        this.parameter = parameter;
        this.keys = keys;
        this.value = value;
        this.ops = ops;
    }

    /**
     * Returns the name of the lattice symbol.
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the parameter of the relation symbol.
     */
    public ProxyObject getParameter() {
        return parameter;
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

    /**
     * Returns the parameterless version of `this` lattice symbol.
     */
    public LatSym getParameterless() {
        return of(name, null, keys, value, ops);
    }

    /**
     * Returns a human-readable representation of `this` lattice symbol.
     */
    @Override
    public String toString() {
        if (parameter == null)
            return name;
        else
            return name + "<" + parameter.toString() + ">";
    }

    /* equality by identity */

}
