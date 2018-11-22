package ca.uwaterloo.flix.runtime.solver.api.predicate;

import ca.uwaterloo.flix.runtime.solver.api.symbol.PredSym;
import ca.uwaterloo.flix.runtime.solver.api.symbol.VarSym;
import ca.uwaterloo.flix.runtime.solver.api.term.LitTerm;
import ca.uwaterloo.flix.runtime.solver.api.term.Term;

import java.util.Arrays;

/**
 * Represents an atom predicate of the form: sym(terms).
 */
public final class AtomPredicate implements Predicate {

    /**
     * The predicate symbol.
     */
    private final PredSym sym;

    /**
     * Whether the atom is negated.
     */
    private final boolean positive;

    /**
     * The terms of the atom.
     */
    private final Term[] terms;

    /**
     * Constructs an atom predicate for the given predicate symbol, with the given polarity, and terms.
     */
    public AtomPredicate(PredSym sym, boolean positive, Term[] terms) {
        if (sym == null)
            throw new IllegalArgumentException("'sym' must be non-null.");
        if (terms == null)
            throw new IllegalArgumentException("'terms' must be non-null.");

        this.sym = sym;
        this.positive = positive;
        this.terms = terms;
    }

    /**
     * Returns the symbol of `this` atom.
     */
    public PredSym getSym() {
        return sym;
    }

    /**
     * Returns `true` if `this` atom is positive.
     */
    public Boolean isPositive() {
        return positive;
    }

    /**
     * Returns `true` if `this` atom is negative.
     */
    public Boolean isNegative() {
        return !isPositive();
    }

    /**
     * Returns the terms of `this` atom.
     */
    public Term[] getTerms() {
        return terms;
    }

    /**
     * Returns `true` if `this` atom is ground.
     */
    public boolean isGround() {
        for (Term t : terms) {
            if (!(t instanceof LitTerm)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Returns `true` if `this` atom entails `that` atom.
     */
    public boolean entails(AtomPredicate that) {
        if (that == null)
            throw new IllegalArgumentException("'that' must be non-null.");

        // TODO: Lattice semantics.

        if (!this.sym.equals(that.sym)) {
            // Case 1: Symbols differ.
            return false;
        }

        if (this.isGround() && that.isGround()) {
            for (var i = 0; i < this.getTerms().length; i++) {
                var thisTerm = (LitTerm) this.getTerms()[i];
                var thatTerm = (LitTerm) that.getTerms()[i];
                var thisLit = thisTerm.getFunction().apply(new Object[1]);
                var thatLit = thatTerm.getFunction().apply(new Object[1]);
                if (!thisLit.equals(thatLit)) {
                    // Case 2: A literal differs.
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * Returns a human-readable representation of `this` predicate.
     */
    @Override
    public String toString() {
        return sym.toString() + "(" + Arrays.toString(terms) + ")";
    }
}
