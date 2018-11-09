package ca.uwaterloo.flix.runtime.solver.api.symbol;

import ca.uwaterloo.flix.runtime.solver.api.Attribute;

import java.util.HashMap;
import java.util.Map;

/**
 * Represents a relation symbol.
 * <p>
 * Note: Equality is defined by identity due to the internal cache.
 */
public final class NamedRelSym implements RelSym {

    /**
     * An internal cache of relation symbols.
     * <p>
     * Note: We never have to garbage collect these since there is only a small finite number of global relation symbols.
     */
    private static final Map<String, NamedRelSym> INTERNAL_CACHE = new HashMap<>();

    /**
     * Returns the relation symbol with the given unique name.
     */
    public static NamedRelSym getInstance(String uniqueName, Attribute[] attributes) {
        var lookup = INTERNAL_CACHE.get(uniqueName);
        if (lookup != null) {
            return lookup;
        }
        var sym = new NamedRelSym(uniqueName, attributes);
        INTERNAL_CACHE.put(uniqueName, sym);
        return sym;
    }

    /**
     * The unique name of the relation.
     */
    private final String uniqueName;

    /**
     * The attributes of the relation symbol.
     */
    private final Attribute[] attributes;

    /**
     * Constructs a relation symbol for the given unique name and attributes.
     */
    private NamedRelSym(String uniqueName, Attribute[] attributes) {
        if (uniqueName == null)
            throw new IllegalArgumentException("'uniqueName' must be non-null.");
        if (attributes == null)
            throw new IllegalArgumentException("'attributes' must be non-null.");

        this.uniqueName = uniqueName;
        this.attributes = attributes;
    }

    /**
     * Returns the name of the relation symbol.
     */
    public String getName() {
        return uniqueName;
    }

    /**
     * Returns the attributes of the relation symbol.
     */
    public Attribute[] getAttributes() {
        return attributes;
    }

    /* equality by identity */

}
