package ca.uwaterloo.flix.runtime.solver.api.symbol;

/**
 * Represents a variable symbol.
 * <p>
 * Note: Two variable symbols are considered the same variable if they are the same object.
 */
public final class VarSym {

    private final String humanName;
    private int offset = -1; // TODO: We want VarSym to eventually be replaced by just an index.

    public VarSym(String humanName) {
        this.humanName = humanName;
    }

    public int getStackOffset() {
        return offset;
    }

    public void setStackOffset(int offset) {
        this.offset = offset;
    }

    /**
     * Returns a human-readable representation of `this` variable symbol.
     */
    @Override
    public String toString() {
        return humanName;
    }

}
