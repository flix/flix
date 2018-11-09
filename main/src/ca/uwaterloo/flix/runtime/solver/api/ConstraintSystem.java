package ca.uwaterloo.flix.runtime.solver.api;

import ca.uwaterloo.flix.runtime.solver.api.predicate.AtomPredicate;
import ca.uwaterloo.flix.runtime.solver.api.predicate.Predicate;
import ca.uwaterloo.flix.runtime.solver.api.symbol.LatSym;
import ca.uwaterloo.flix.runtime.solver.api.symbol.RelSym;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * Represents a collection of constraints.
 */
public final class ConstraintSystem {

    /**
     * Returns the composition of `c1` with `c2`.
     */
    public static ConstraintSystem compose(ConstraintSystem c1, ConstraintSystem c2) {
        var facts = concat(c1.getFacts(), c2.getFacts());
        var rules = concat(c1.getRules(), c2.getRules());
        return new ConstraintSystem(facts, rules);
    }

    /**
     * The collection of facts.
     */
    private Constraint[] facts;

    /**
     * The collection of rules.
     */
    private Constraint[] rules;

    /**
     * Construct a new constraint system with the given constraint.
     */
    public ConstraintSystem(Constraint constraint) {
        if (constraint == null)
            throw new IllegalArgumentException("'constraint' must be non-null.");

        if (constraint.isFact()) {
            facts = new Constraint[]{constraint};
            rules = new Constraint[]{};
        } else {
            facts = new Constraint[]{};
            rules = new Constraint[]{constraint};
        }
    }

    /**
     * Constructs a new constraint system with the given constraints.
     */
    public ConstraintSystem(Constraint[] constraints) {
        var facts = new LinkedList<Constraint>();
        var rules = new LinkedList<Constraint>();
        for (Constraint c : constraints) {
            if (c.isFact())
                facts.add(c);
            else
                rules.add(c);
        }
        this.facts = facts.toArray(new Constraint[0]);
        this.rules = rules.toArray(new Constraint[0]);
    }

    /**
     * Constructs a new constraint system with the given facts and rules.
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
     * Returns all the facts in `this` constraint system as a new constraint system.
     */
    public ConstraintSystem getModel() {
        return new ConstraintSystem(facts, new Constraint[0]);
    }

    /**
     * Returns all relation symbols in `this` constraint system.
     */
    public RelSym[] getRelationSymbols() {
        var result = new LinkedList<RelSym>();
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
        var result = new LinkedList<LatSym>();
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
        var result = new LinkedList<AtomPredicate>();
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
        //        /**
//         * Returns a human readable representation the constraint set.
//         */
//        override def toString: String = {
//        if (!isOnlyFacts()) {
//        return constraints.mkString(", ")
//        }
////
//        val sb = new StringBuilder
//
//        // Group all facts by predicate symbol.
//        val constraintsByHead = constraints.groupBy(_.getHeadPredicate())
//        for ((predicate, constraints) <- constraintsByHead) {
//        predicate match {
//        case head: AtomPredicate =>
//        // Retrieve the name and attributes of the predicate.
//        val name = head.getSym().getName()
//        val attributes = head.getTerms().map(_ => "attr")
//
//        // Construct an ascii table with a column for each attribute.
//        val columns = attributes
//        val table = new AsciiTable().withTitle(name).withCols(columns: _*)
//
//        // Add each row to the ASCII table.
//        for (row <- constraints) {
//        row.getHeadPredicate() match {
//        case atom: AtomPredicate =>
//        table.mkRow(atom.getTerms().toList)
//        case head: TruePredicate => //  nop
//        case head: FalsePredicate => //  nop
//        case _ => throw new RuntimeException(s"Unexpected head predicate: '$predicate'.")
//        }
//        }
//
//        // Write the ASCII table to the string buffer.
//        val sw = new StringWriter()
//        table.write(new PrintWriter(sw))
//        sb.append(sw.toString)
//
//        case head: TruePredicate => //  nop
//        case head: FalsePredicate => //  nop
//        case _ => throw new RuntimeException(s"Unexpected head predicate: '$predicate'.")
//        }
//        }
//
//        sb.toString()
//        }
//
//        }
        return "<constraint system>";
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
