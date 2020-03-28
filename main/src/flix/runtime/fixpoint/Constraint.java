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

package flix.runtime.fixpoint;

import flix.runtime.ReifiedSourceLocation;
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
    public static Constraint of(VarSym[] cparams, Predicate head, Predicate[] body, ReifiedSourceLocation loc) {
        if (cparams == null)
            throw new IllegalArgumentException("'cparams' must be non-null.");
        if (head == null)
            throw new IllegalArgumentException("'head' must be non-null.");
        if (body == null)
            throw new IllegalArgumentException("'body' must be non-null.");

        return new Constraint(cparams, head, body, loc);
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
     * The nullable source location of the constraint.
     */
    private final ReifiedSourceLocation loc;

    /**
     * The body atom predicates.
     */
    private final AtomPredicate[] bodyAtoms;

    /**
     * The body guard predicates.
     */
    private final GuardPredicate[] bodyGuards;

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
    private Constraint(VarSym[] cparams, Predicate head, Predicate[] body, ReifiedSourceLocation loc) {
        //
        // A head predicate cannot be a guard predicate.
        //
        if (head instanceof GuardPredicate) {
            throw new IllegalArgumentException("A head predicate cannot be a guard predicate.");
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
        this.loc = loc;

        //
        // Partition the body predicates based on their types. The joy of arrays.
        //
        var bodyAtoms = new ArrayList<AtomPredicate>();
        var bodyFilters = new ArrayList<GuardPredicate>();
        for (Predicate b : body) {
            if (b instanceof AtomPredicate) {
                bodyAtoms.add((AtomPredicate) b);
            } else if (b instanceof GuardPredicate) {
                bodyFilters.add((GuardPredicate) b);
            } else if (b instanceof UnionPredicate) {
                throw new IllegalArgumentException("A union predicate cannot occur in the body.");
            }
        }

        this.bodyAtoms = bodyAtoms.toArray(new AtomPredicate[0]);
        this.bodyGuards = bodyFilters.toArray(new GuardPredicate[0]);
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
     * Returns the nullable source location.
     */
    public ReifiedSourceLocation getSourceLocation() {
        return this.loc;
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
     * Returns the guard predicates in the body of `this` constraint.
     */
    public GuardPredicate[] getGuards() {
        return bodyGuards;
    }

    /**
     * Returns `true` if `this` fact entails the given `that`.
     */
    public boolean entails(Constraint that) {
        if (that == null)
            throw new IllegalArgumentException("'that' must be non-null.");

        if (this.isFact() && that.isFact()) {
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
