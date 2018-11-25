package ca.uwaterloo.flix.runtime.solver.api.symbol;

import flix.runtime.fixpoint.Attribute;
import ca.uwaterloo.flix.runtime.solver.api.ProxyObject;
import flix.runtime.fixpoint.symbol.PredSym;

import java.util.HashMap;
import java.util.Map;

/**
 * Represents a parameterized relation symbol.
 */
public final class RelSym implements PredSym {

    /**
     * An internal cache of relation symbols.
     */
    private static final Map<NameAndParameter, RelSym> INTERNAL_CACHE = new HashMap<>();

    /**
     * Returns the relation symbol for the given `name` with the given `parameter` and `attributes`.
     * <p>
     * The parameter may be null.
     */
    public static RelSym of(String name, ProxyObject parameter, Attribute[] attributes) {
        if (name == null)
            throw new IllegalArgumentException("'name' must be non-null.");
        if (attributes == null)
            throw new IllegalArgumentException("'attributes' must be non-null.");

        var key = new NameAndParameter(name, parameter);
        var lookup = INTERNAL_CACHE.get(key);
        if (lookup != null) {
            return lookup;
        }
        var sym = new RelSym(name, parameter, attributes);
        INTERNAL_CACHE.put(key, sym);
        return sym;
    }

    /**
     * The unique name of the relation symbol.
     */
    private final String name;

    /**
     * The parameter of the relation symbol.
     */
    private final ProxyObject parameter;

    /**
     * The attributes of the relation symbol.
     */
    private final Attribute[] attributes;

    /**
     * The parameterless version of `this` relation symbol.
     */
    private final RelSym parameterless;

    /**
     * Constructs a fresh relation symbol with the given `name` and `parameter`.
     */
    private RelSym(String name, ProxyObject parameter, Attribute[] attributes) {
        this.name = name;
        this.parameter = parameter;
        this.attributes = attributes;
        if (this.parameter == ProxyObject.UNIT) {
            this.parameterless = this;
        } else {
            this.parameterless = of(name, ProxyObject.UNIT, attributes);
        }
    }

    /**
     * Returns the name of the relation symbol.
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
     * Returns the attributes of the relation symbol.
     */
    public Attribute[] getAttributes() {
        return attributes;
    }

    /**
     * Returns the parameterless version of `this` relation symbol.
     */
    public RelSym getParameterless() {
        return this.parameterless;
    }

    /**
     * Returns a human-readable representation of `this` relation symbol.
     */
    @Override
    public String toString() {
        if (parameter == null)
            return name;
        else
            return name + "(" + parameter.toString() + ")";
    }

    /* equality by identity */

}
