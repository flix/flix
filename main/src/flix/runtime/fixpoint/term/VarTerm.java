package flix.runtime.fixpoint.term;

import ca.uwaterloo.flix.runtime.solver.api.term.Term;
import flix.runtime.fixpoint.symbol.VarSym;

/**
 * Represents a variable term.
 */
public final class VarTerm implements Term {

    /**
     * Constructs a new variable term from the given variable symbol `sym`.
     */
    public static VarTerm of(VarSym sym) {
        if (sym == null)
            throw new IllegalArgumentException("'sym' must be non-null.");

        return new VarTerm(sym);
    }

    /**
     * The variable symbol.
     */
    private final VarSym sym;

    /**
     * Private constructor.
     */
    private VarTerm(VarSym sym) {
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
