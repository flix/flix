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

import flix.runtime.fixpoint.predicate.AtomPredicate;
import flix.runtime.fixpoint.predicate.Predicate;
import flix.runtime.fixpoint.symbol.LatSym;
import flix.runtime.fixpoint.symbol.RelSym;
import flix.runtime.fixpoint.term.Term;
import flix.runtime.util.AsciiTable;

import java.util.*;

/**
 * Represents a system of constraints.
 */
public final class ConstraintSystem {

    /**
     * Constructs a new constraint system of the given constraint `c`.
     */
    // TODO: Deprecated
    public static ConstraintSystem of(Constraint c) {
        if (c == null)
            throw new IllegalArgumentException("'c' must be non-null.");

        return of(new Constraint[]{c});
    }

    /**
     * Constructs a new constraint system of the given constraints `cs`.
     */
    public static ConstraintSystem of(Constraint[] cs) {
        if (cs == null)
            throw new IllegalArgumentException("'cs' must be non-null.");

        var facts = new ArrayList<Constraint>();
        var rules = new ArrayList<Constraint>();
        for (Constraint c : cs) {
            if (c.isFact())
                facts.add(c);
            else
                rules.add(c);
        }
        return new ConstraintSystem(facts.toArray(new Constraint[0]), rules.toArray(new Constraint[0]));
    }

    /**
     * The array of facts.
     */
    private final Constraint[] facts;

    /**
     * The array of rules.
     */
    private final Constraint[] rules;

    /**
     * Private constructor.
     */
    private ConstraintSystem(Constraint[] facts, Constraint[] rules) {
        if (facts == null)
            throw new IllegalArgumentException("'facts' must be non-null.");
        if (rules == null)
            throw new IllegalArgumentException("'rules' must be non-null.");

        this.facts = facts;
        this.rules = rules;
    }

    /**
     * Returns all facts in `this` constraint system.
     */
    public Constraint[] getFacts() {
        return facts;
    }

    /**
     * Returns all rules in `this` constraint system.
     */
    public Constraint[] getRules() {
        return rules;
    }

    /**
     * Returns all constraints in `this` constraint system.
     */
    public Constraint[] getConstraints() {
        return concat(facts, rules);
    }

    /**
     * Returns all relation symbols in `this` constraint system.
     */
    public RelSym[] getRelationSymbols() {
        var result = new HashSet<RelSym>();
        for (AtomPredicate p : getAtomPredicates()) {
            if (p.getSym() instanceof RelSym) {
                result.add((RelSym) p.getSym());
            }
        }
        return result.toArray(new RelSym[0]);
    }

    /**
     * Returns all lattice symbols in `this` constraint system.
     */
    public LatSym[] getLatticeSymbols() {
        var result = new HashSet<LatSym>();
        for (AtomPredicate p : getAtomPredicates()) {
            if (p.getSym() instanceof LatSym) {
                result.add((LatSym) p.getSym());
            }
        }
        return result.toArray(new LatSym[0]);
    }

    /**
     * Returns all atom predicates in `this` constraint system.
     */
    private List<AtomPredicate> getAtomPredicates() {
        var result = new ArrayList<AtomPredicate>();
        for (Constraint c : facts) {
            for (Predicate p : c.getAllAtoms()) {
                if (p instanceof AtomPredicate) {
                    result.add((AtomPredicate) p);
                }
            }
        }
        for (Constraint c : rules) {
            for (Predicate p : c.getAllAtoms()) {
                if (p instanceof AtomPredicate) {
                    result.add((AtomPredicate) p);
                }
            }
        }
        return result;
    }

    @Override
    public String toString() {
        // Print a short summary if the constraint system contains both facts and rules.
        if (rules.length != 0) {
            return "<facts = " + facts.length + ", rules = " + rules.length + ">";
        }

        // Otherwise pretty print the facts.

        ///
        /// Relations
        ///
        Map<RelSym, String[]> relHeaders = new HashMap<>();
        for (RelSym relSym : getRelationSymbols()) {
            Attribute[] attributes = relSym.getAttributes();
            String[] headers = new String[attributes.length];
            for (int i = 0; i < headers.length; i++) {
                headers[i] = attributes[i].getName();
            }
            relHeaders.put(relSym, headers);
        }
        Map<RelSym, ArrayList<String[]>> relData = new HashMap<>();
        for (RelSym relSym : getRelationSymbols()) {
            relData.put(relSym, new ArrayList<>());
        }

        ///
        /// Lattices
        ///
        Map<LatSym, String[]> latHeaders = new HashMap<>();
        for (LatSym latSym : getLatticeSymbols()) {
            Attribute[] keys = latSym.getKeys();
            Attribute value = latSym.getValue();
            String[] headers = new String[keys.length + 1];
            for (int i = 0; i < keys.length; i++) {
                headers[i] = keys[i].getName();
            }
            headers[keys.length] = value.getName();
            latHeaders.put(latSym, headers);
        }
        Map<LatSym, ArrayList<String[]>> latData = new HashMap<>();
        for (LatSym latSym : getLatticeSymbols()) {
            latData.put(latSym, new ArrayList<>());
        }

        // Process all facts.
        for (Constraint fact : facts) {
            var headPred = fact.getHeadPredicate();
            if (headPred instanceof AtomPredicate) {
                var atom = (AtomPredicate) headPred;
                var terms = atom.getTerms();

                var sym = atom.getSym();
                if (sym instanceof RelSym) {
                    var relSym = (RelSym) sym;
                    var data = relData.get(relSym);
                    var row = new String[terms.length];
                    for (int i = 0; i < terms.length; i++) {
                        row[i] = terms[i].toString();
                    }
                    data.add(row);
                }

                if (sym instanceof LatSym) {
                    var latSym = (LatSym) sym;
                    var data = latData.get(latSym);
                    var row = new String[terms.length];
                    for (int i = 0; i < terms.length; i++) {
                        row[i] = terms[i].toString();
                    }
                    data.add(row);
                }
            }
        }

        // Construct a string builder to concatenate all the ascii tables into.
        var sb = new StringBuilder();

        ///
        /// Relations
        ///
        for (RelSym relSym : getRelationSymbols()) {
            // Retrieve the headers and data.
            String[] headers = relHeaders.get(relSym);
            String[][] data = relData.get(relSym).toArray(new String[0][]);
            AsciiTable table = new AsciiTable(headers, data);

            sb.append(table);
            sb.append("\n");
        }

        ///
        /// Lattices
        ///
        for (LatSym latSym : getLatticeSymbols()) {
            // Retrieve the headers and data.
            String[] headers = latHeaders.get(latSym);
            String[][] data = latData.get(latSym).toArray(new String[0][]);
            AsciiTable table = new AsciiTable(headers, data);

            sb.append(table);
            sb.append("\n");
        }

        return sb.toString();
    }

    /**
     * Returns the concatenation of the two given arrays.
     */
    private static <T> T[] concat(T[] fst, T[] snd) {
        T[] result = Arrays.copyOf(fst, fst.length + snd.length);
        System.arraycopy(snd, 0, result, fst.length, snd.length);
        return result;
    }

}
