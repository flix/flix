package ca.uwaterloo.flix.runtime.solver.api.symbol;

import ca.uwaterloo.flix.runtime.solver.api.Attribute;
import ca.uwaterloo.flix.runtime.solver.api.ProxyObject;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Represents a parameterized relation symbol.
 */
public final class RelSym implements PredSym {

    /**
     * An internal class to represent a pair of a name and a parameter.
     * <p>
     * The parameter may be null.
     */
    private static final class NameAndParameter {
        private final String name;
        private final ProxyObject parameter;

        private NameAndParameter(String name, ProxyObject parameter) {
            this.name = name;
            this.parameter = parameter;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            NameAndParameter that = (NameAndParameter) o;
            return Objects.equals(name, that.name) &&
                    Objects.equals(parameter, that.parameter);
        }

        @Override
        public int hashCode() {
            return Objects.hash(name, parameter);
        }
    }

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
     * The unique name of the relation.
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
     * Constructs a relation symbol for the given unique name and attributes.
     */
    private RelSym(String name, ProxyObject parameter, Attribute[] attributes) {
        this.name = name;
        this.parameter = parameter;
        this.attributes = attributes;
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
     * Returns a human-readable representation of `this` predicate symbol.
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
