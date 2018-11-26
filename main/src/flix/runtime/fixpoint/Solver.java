package flix.runtime.fixpoint;

import ca.uwaterloo.flix.runtime.solver.DeltaSolver;
import flix.runtime.fixpoint.symbol.PredSym;
import flix.runtime.RuleError;
import flix.runtime.fixpoint.predicate.AtomPredicate;
import flix.runtime.fixpoint.predicate.Predicate;

import java.util.Arrays;
import java.util.LinkedList;

public final class Solver {

    /**
     * Returns the composition of `cs1` with `cs2`.
     */
    public static ConstraintSystem compose(ConstraintSystem cs1, ConstraintSystem cs2) {
        var constraints = concat(cs1.getConstraints(), cs2.getConstraints());
        return ConstraintSystem.of(constraints);
    }

    /**
     * Returns the projection of the given predicate symbol `sym` of the given constraint system `cs`.
     */
    public static ConstraintSystem project(PredSym sym, ConstraintSystem cs) {
        if (sym == null)
            throw new IllegalArgumentException("'sym' must be non-null.");
        if (cs == null)
            throw new IllegalArgumentException("'cs' must be non-null.");

        // Collect all facts with `sym` in its head.
        var result = new LinkedList<Constraint>();
        for (Constraint fact : cs.getFacts()) {
            Predicate head = fact.getHeadPredicate();
            if (head instanceof AtomPredicate) {
                if (((AtomPredicate) head).getSym() == sym) {
                    result.add(fact);
                }
            }
        }

        Constraint[] facts = result.toArray(new Constraint[0]);
        Constraint[] rules = new Constraint[0];
        Constraint[] constraints = concat(facts, rules);
        return ConstraintSystem.of(constraints);
    }

    /**
     * Solves the given constraint system `cs` with the given stratification `stf` and options `o`.
     */
    public static ConstraintSystem solve(ConstraintSystem cs, Stratification stf, Options o) {
        ca.uwaterloo.flix.runtime.solver.Solver solver = new ca.uwaterloo.flix.runtime.solver.Solver(cs, stf, o);
        ConstraintSystem result = solver.solve();
        System.out.println(result.toString());
        return result;
    }

    /**
     * Checks the given constraint system `cs` with the given stratification `stf` and options `o`.
     */
    public static boolean check(ConstraintSystem cs, Stratification stf, Options o) {
        try {
            ca.uwaterloo.flix.runtime.solver.Solver solver = new ca.uwaterloo.flix.runtime.solver.Solver(cs, stf, o);
            solver.solve();
            return true;
        } catch (RuleError e) {
            return false;
        }
    }

    /**
     * Delta Solves the given constraint system `cs` with the given stratification `stf` and options `o`.
     */
    public static String deltaSolve(ConstraintSystem cs, Stratification stf, Options o) {
        DeltaSolver deltaSolver = new DeltaSolver(cs, stf, o);
        return deltaSolver.deltaSolve(); // TODO: Should return a constraint system and not a string.
    }

    /**
     * Returns `true` if all facts in `cs2` are included in `cs1`.
     */
    public static boolean entails(ConstraintSystem cs1, ConstraintSystem cs2) {
        var entails = true;
        for (Constraint fact2 : cs2.getFacts()) {
            var found = false;
            for (Constraint fact1 : cs1.getFacts()) {
                if (fact1.entails(fact2)) {
                    found = true;
                }
            }
            if (!found) {
                entails = false;
            }
        }
        return entails;
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
