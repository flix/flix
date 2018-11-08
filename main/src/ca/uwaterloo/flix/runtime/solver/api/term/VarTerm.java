package ca.uwaterloo.flix.runtime.solver.api.term;

import ca.uwaterloo.flix.runtime.solver.api.symbol.VarSym;

/**
 * Represents a variable term.
 */
public final class VarTerm implements Term {

    /**
     * The variable symbol.
     */
    private final VarSym sym;

    /**
     * Constructs a variable term from the given variable symbol `sym`.
     */
    public VarTerm(VarSym sym) {
        if (sym == null)
            throw new IllegalArgumentException("'sym' must be non-null.");

        this.sym = sym;
    }

    /**
     * Returns the variable symbol of `this` variable term.
     */
    public VarSym getSym() {
        return sym;
    }

    /**
     * Returns a human-readable representation of `this` term.
     */
    @Override
    public String toString() {
        return sym.toString();
    }

}
