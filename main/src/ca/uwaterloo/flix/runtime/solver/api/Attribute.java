package ca.uwaterloo.flix.runtime.solver.api;

/**
 * Represents a relation or lattice attribute.
 */
public final class Attribute {

    /**
     * The name of the attribute.
     */
    private final String name;

    // TODO: Why not include equality here?

    /**
     * Constructs a new attribute with the given name.
     */
    public Attribute(String name) {
        if (name == null)
            throw new IllegalArgumentException("'name' must be non-null.");

        this.name = name;
    }

    /**
     * Returns the name of the attribute.
     */
    public String getName() {
        return name;
    }

}
