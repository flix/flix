package flix.runtime.fixpoint;

/**
 * Represents a relation or lattice attribute.
 */
public final class Attribute {

    /**
     * Constructs a new attribute with the given name.
     */
    public static Attribute of(String name) {
        if (name == null)
            throw new IllegalArgumentException("'name' must be non-null.");

        return new Attribute(name);
    }

    /**
     * The name of the attribute.
     */
    private final String name;

    /**
     * Private constructor.
     */
    private Attribute(String name) {
        this.name = name;
    }

    /**
     * Returns the name of the attribute.
     */
    public String getName() {
        return name;
    }

}
