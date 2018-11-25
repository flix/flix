package flix.runtime.fixpoint;

import flix.runtime.fixpoint.predicate.*;
import flix.runtime.fixpoint.symbol.VarSym;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Represents a constraint.
 */
public final class Constraint {

    /**
     * Constructs a constraint from the given quantified variables, head and body predicates.
     */
    public static Constraint of(VarSym[] cparams, Predicate head, Predicate[] body) {
        if (cparams == null)
            throw new IllegalArgumentException("'cparams' must be non-null.");
        if (head == null)
            throw new IllegalArgumentException("'head' must be non-null.");
        if (body == null)
            throw new IllegalArgumentException("'body' must be non-null.");

        return new Constraint(cparams, head, body);
    }

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
     * Private constructor.
     */
    private Constraint(VarSym[] cparams, Predicate head, Predicate[] body) {
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
        var bodyAtoms = new ArrayList<AtomPredicate>();
        var bodyFilters = new ArrayList<FilterPredicate>();
        var bodyFunctionals = new ArrayList<FunctionalPredicate>();
        for (Predicate b : body) {
            if (b instanceof AtomPredicate) {
                bodyAtoms.add((AtomPredicate) b);
            } else if (b instanceof FilterPredicate) {
                bodyFilters.add((FilterPredicate) b);
            } else if (b instanceof FunctionalPredicate) {
                bodyFunctionals.add((FunctionalPredicate) b);
            }
        }

        this.bodyAtoms = bodyAtoms.toArray(new AtomPredicate[0]);
        this.bodyFilters = bodyFilters.toArray(new FilterPredicate[0]);
        this.bodyFunctionals = bodyFunctionals.toArray(new FunctionalPredicate[0]);
    }

    /**
     * Returns `true` if `this` constraint is a fact.
     */
    public boolean isFact() {
        return body.length == 0;
    }

    /**
     * Returns `true` if `this` constraint is a rule.
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
     * Returns the constraint parameters of `this` constraint.
     */
    public VarSym[] getConstraintParameters() {
        return this.cparams;
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
     * Returns the number of times `this` constraint has been evaluated.
     */
    public int getNumberOfHits() {
        return hits.get();
    }

    /**
     * Increments the number of times `this` constraint has been evaluated.
     */
    public void incrementNumberOfHits() {
        hits.getAndIncrement();
    }

    /**
     * Returns the number of nanoseconds spent during evaluation of `this` constraint.
     */
    public Long getElapsedTime() {
        return time.get();
    }

    /**
     * Increments the number of times `this` constraint has been evaluated.
     */
    public void incrementElapsedTime(Long ns) {
        time.addAndGet(ns);
    }

    /**
     * Returns all atom predicates in `this` constraint.
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
     * Returns all atoms predicates in the body of `this` constraint.
     */
    public AtomPredicate[] getBodyAtoms() {
        return bodyAtoms;
    }

    /**
     * Returns the filter predicates in the body of `this` constraint.
     */
    public FilterPredicate[] getFilters() {
        return bodyFilters;
    }

    /**
     * Returns the functional predicates in the body of `this` constraint.
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
