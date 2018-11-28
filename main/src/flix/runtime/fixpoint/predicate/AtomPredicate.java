/*
 * Copyright 2018 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package flix.runtime.fixpoint.predicate;

import flix.runtime.fixpoint.symbol.PredSym;
import flix.runtime.fixpoint.term.LitTerm;
import flix.runtime.fixpoint.term.Term;

import java.util.Arrays;

/**
 * Represents an atom predicate of the form: sym(terms).
 */
public final class AtomPredicate implements Predicate {

    /**
     * Constructs an atom predicate for the given predicate symbol, with the given polarity, and terms.
     */
    public static AtomPredicate of(PredSym sym, boolean positive, Term[] terms) {
        if (sym == null)
            throw new IllegalArgumentException("'sym' must be non-null.");
        if (terms == null)
            throw new IllegalArgumentException("'terms' must be non-null.");

        return new AtomPredicate(sym, positive, terms);
    }

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
     * Private constructor.
     */
    private AtomPredicate(PredSym sym, boolean positive, Term[] terms) {
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

        if (!this.isGround()) {
            throw new IllegalArgumentException("'this' must be ground.");
        }

        if (!that.isGround()) {
            throw new IllegalArgumentException("'that' must be ground.");
        }

        if (!this.sym.equals(that.sym)) {
            // Case 1: Symbols differ.
            return false;
        }

        // TODO: Lattice semantics.

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
