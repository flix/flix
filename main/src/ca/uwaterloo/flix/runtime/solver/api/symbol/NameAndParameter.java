package ca.uwaterloo.flix.runtime.solver.api.symbol;

import flix.runtime.ProxyObject;

import java.util.Objects;

/**
 * An internal class to represent a pair of a name and a parameter.
 * <p>
 * The parameter may be null.
 */
final class NameAndParameter {

    /**
     * The name. Must be non-null.
     */
    private final String name;

    /**
     * The parameter. May be null.
     */
    private final ProxyObject parameter;

    /**
     * Constructs a new name and parameter pair.
     */
    NameAndParameter(String name, ProxyObject parameter) {
        this.name = name;
        this.parameter = parameter;
    }

    /**
     * Returns `true` if `this` is equal to `that`.
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        NameAndParameter that = (NameAndParameter) o;
        return Objects.equals(name, that.name) &&
                Objects.equals(parameter, that.parameter);
    }

    /**
     * Returns the hash of `this` name and parameter.
     */
    @Override
    public int hashCode() {
        return Objects.hash(name, parameter);
    }
}
