package ca.uwaterloo.flix.runtime.solver.api;

import ca.uwaterloo.flix.runtime.solver.api.predicate.*;
import flix.runtime.fixpoint.symbol.VarSym;
import flix.runtime.fixpoint.predicate.FalsePredicate;
import flix.runtime.fixpoint.predicate.Predicate;
import flix.runtime.fixpoint.predicate.TruePredicate;

import java.util.Arrays;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Represents a constraint.
 * <p>
 * A constraint is a horn clause that consists of
 * a sequence of universally quantified variables,
 * a head predicate, and a sequence of body predicates.
 */
public final class Constraint {

    /**
     * The universally quantified variables.
     */
    private final VarSym[] cparams;

    /**
     * The head predicate.
     */
    private final Predicate head;

    /**
     * The body predicates.
     */
    private final Predicate[] body;

    /**
     * The body atom predicates.
     */
    private final AtomPredicate[] bodyAtoms;

    /**
     * The body filter predicates.
     */
    private final FilterPredicate[] bodyFilters;

    /**
     * The body functional predicates.
     */
    private final FunctionalPredicate[] bodyFunctionals;

    /**
     * Numbers of times the constraint has been evaluated.
     */
    private final AtomicInteger hits = new AtomicInteger();

    /**
     * Number of nanoseconds spent during evaluation of the constraint.
     */
    private final AtomicLong time = new AtomicLong();

    /**
     * Constructs a constraint from the given quantified variables, head and body predicates.
     */
    public Constraint(VarSym[] cparams, Predicate head, Predicate[] body) {
        if (cparams == null)
            throw new IllegalArgumentException("'cparams' must be non-null.");
        if (head == null)
            throw new IllegalArgumentException("'head' must be non-null.");
        if (body == null)
            throw new IllegalArgumentException("'body' must be non-null.");

        //
        // A head predicate cannot be a filter predicate.
        //
        if (head instanceof FilterPredicate) {
            throw new IllegalArgumentException("A head predicate cannot be a filter predicate.");
        }

        //
        // A head predicate cannot be a functional predicate.
        //
        if (head instanceof FunctionalPredicate) {
            throw new IllegalArgumentException("A head predicate cannot be a filter predicate.");
        }

        //
        // A head predicate cannot be a negated atom predicate.
        //
        if (head instanceof AtomPredicate) {
            if (((AtomPredicate) head).isNegative()) {
                throw new IllegalArgumentException("A head predicate cannot be negated.");
            }
        }

        //
        // Assign fields.
        //
        this.cparams = cparams;
        this.head = head;
        this.body = body;

        //
        // Partition the body predicates based on their types. The joy of arrays.
        //
        var numberOfBodyAtoms = 0;
        var numberOfBodyFilters = 0;
        var numberOfBodyFunctionals = 0;
        for (Predicate b : body) {
            if (b instanceof AtomPredicate) {
                numberOfBodyAtoms++;
            } else if (b instanceof FilterPredicate) {
                numberOfBodyFilters++;
            } else if (b instanceof FunctionalPredicate) {
                numberOfBodyFunctionals++;
            }
        }

        this.bodyAtoms = new AtomPredicate[numberOfBodyAtoms];
        this.bodyFilters = new FilterPredicate[numberOfBodyFilters];
        this.bodyFunctionals = new FunctionalPredicate[numberOfBodyFunctionals];

        var bodyAtomCounter = 0;
        var bodyFilterounter = 0;
        var bodyFunctionalCounter = 0;
        for (Predicate b : body) {
            if (b instanceof AtomPredicate) {
                this.bodyAtoms[bodyAtomCounter++] = (AtomPredicate) b;
            } else if (b instanceof FilterPredicate) {
                this.bodyFilters[bodyFilterounter++] = (FilterPredicate) b;
            } else if (b instanceof FunctionalPredicate) {
                this.bodyFunctionals[bodyFunctionalCounter++] = (FunctionalPredicate) b;
            }
        }
    }

    // TODO: This cannot be right!!!!
    public int getStratum() {
        return 0;
    }

    /**
     * Returns `true` if the constraint is a fact.
     */
    public boolean isFact() {
        return body.length == 0;
    }

    /**
     * Returns `true` if the constraint is a rule.
     */
    public boolean isRule() {
        return !isFact();
    }

    /**
     * Returns `true` if `this` constraint is the true fact.
     */
    public boolean isTrueFact() {
        return isFact() && head instanceof TruePredicate;
    }

    /**
     * Returns the constraint parameters.
     */
    public VarSym[] getParams() {
        return this.cparams;
    }

    /**
     * Returns the number of variables in the constraint.
     */
    public int getNumberOfParameters() {
        return this.cparams.length;
    }

    /**
     * Returns the head predicate.
     */
    public Predicate getHeadPredicate() {
        return this.head;
    }

    /**
     * Returns the body predicates.
     */
    public Predicate[] getBodyPredicates() {
        return this.body;
    }

    /**
     * Returns the number of times the constraint has been evaluated.
     */
    public int getNumberOfHits() {
        return hits.get();
    }

    /**
     * Increments the number of times the constraint has been evaluated.
     */
    public void incrementNumberOfHits() {
        hits.getAndIncrement();
    }

    /**
     * Returns the number of nanoseconds spent during evaluation of the constraint.
     */
    public Long getElapsedTime() {
        return time.get();
    }

    /**
     * Increments the number of times the constraint has been evaluated.
     */
    public void incrementElapsedTime(Long ns) {
        time.addAndGet(ns);
    }


    /**
     * Returns all atom predicates in the constraint.
     */
    public AtomPredicate[] getAllAtoms() {
        if (head instanceof AtomPredicate) {
            var result = new AtomPredicate[bodyAtoms.length + 1];
            result[0] = (AtomPredicate) head;
            System.arraycopy(bodyAtoms, 0, result, 1, bodyAtoms.length);
            return result;
        } else {
            return getBodyAtoms();
        }
    }

    /**
     * Returns all atoms predicates in the body of the constraint.
     */
    public AtomPredicate[] getBodyAtoms() {
        return bodyAtoms;
    }

    /**
     * Returns the filter predicates in the body of the constraint.
     */
    public FilterPredicate[] getFilters() {
        return bodyFilters;
    }

    /**
     * Returns the functional predicates in the body of the constraint.
     */
    public FunctionalPredicate[] getFunctionals() {
        return bodyFunctionals;
    }

    /**
     * Returns `true` if `this` fact entails the given `that`.
     */
    public boolean entails(Constraint that) {
        if (that == null)
            throw new IllegalArgumentException("'that' must be non-null.");

        if (this.isFact() && that.isFact()) {
            if (this.head instanceof FalsePredicate) {
                return true;
            }
            if (this.head instanceof TruePredicate && that.head instanceof TruePredicate) {
                return true;
            }
            if (this.head instanceof AtomPredicate && that.head instanceof AtomPredicate) {
                var thisHead = (AtomPredicate) this.head;
                var thatHead = (AtomPredicate) that.head;
                return thisHead.entails(thatHead);
            }
        }

        return false;
    }

    /**
     * Returns a string representation of `this` constraint.
     */
    public String toString() {
        if (isFact()) {
            return head.toString() + ".";
        } else {
            return head.toString() + " :- " + Arrays.toString(body) + ".";
        }
    }

}
