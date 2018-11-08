package ca.uwaterloo.flix.runtime.solver.api.predicate;

import ca.uwaterloo.flix.runtime.solver.api.Table;
import ca.uwaterloo.flix.runtime.solver.api.symbol.VarSym;
import ca.uwaterloo.flix.runtime.solver.api.term.Term;

import java.util.Arrays;

/**
 * Represents an atom predicate of the form: sym(terms).
 */
public final class AtomPredicate implements Predicate {

    /**
     * The predicate symbol.
     */
    private final Table sym;

    /**
     * Whether the atom is negated.
     */
    private final Boolean positive;

    /**
     * The terms of the atom.
     */
    private final Term[] terms;

    // TODO: Temporary.
    public final VarSym[] index2sym;

    /**
     * Constructs an atom predicate for the given predicate symbol, with the given polarity, and terms.
     */
    public AtomPredicate(Table sym, Boolean positive, Term[] terms, VarSym[] index2sym) {
        if (sym == null)
            throw new IllegalArgumentException("'sym' must be non-null.");
        if (terms == null)
            throw new IllegalArgumentException("'terms' must be non-null.");

        this.sym = sym;
        this.positive = positive;
        this.terms = terms;
        this.index2sym = index2sym;
    }

    /**
     * Returns the symbol of `this` atom.
     */
    public Table getSym() {
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

    // TODO
    public VarSym getIndex2SymTEMPORARY(int i) {
        return index2sym[i];
    }

    /**
     * Returns a human-readable representation of `this` predicate.
     */
    @Override
    public String toString() {
        return sym.toString() + "(" + Arrays.toString(terms) + ")";
    }
}
