package ca.uwaterloo.flix.runtime.solver.api.symbol;

import ca.uwaterloo.flix.runtime.solver.api.ProxyObject;

import java.util.Objects;

/**
 * An internal class to represent a pair of a name and a parameter.
 * <p>
 * The parameter may be null.
 */
final class NameAndParameter {
    private final String name;
    private final ProxyObject parameter;

    NameAndParameter(String name, ProxyObject parameter) {
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
