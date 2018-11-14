package ca.uwaterloo.flix.runtime.solver.api.symbol;

/**
 * Represents a variable symbol.
 */
public final class VarSym {

    /**
     * The name of the variable symbol. (Used for debugging).
     */
    private final String name;

    /**
     * The index of the variable symbol. (Used for evaluation).
     */
    private final int index;

    /**
     * Constructs a new variable symbol with the given `name` and `index`.
     */
    public VarSym(String name, int index) {
        if (name == null)
            throw new IllegalArgumentException("'name' must be non-null.");
        if (index < 0)
            throw new IllegalArgumentException("'index' must be non-negative.");

        this.name = name;
        this.index = index;
    }

    /**
     * Returns the index of `this` symbol.
     */
    public int getIndex() {
        return index;
    }

    /**
     * Returns a human-readable representation of `this` variable symbol.
     */
    @Override
    public String toString() {
        return name + "#" + index;
    }

}
