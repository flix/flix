package ca.uwaterloo.flix.runtime.solver.api;

import ca.uwaterloo.flix.runtime.solver.DeltaSolver;
import ca.uwaterloo.flix.runtime.solver.FixpointOptions;
import ca.uwaterloo.flix.runtime.solver.Solver;
import ca.uwaterloo.flix.runtime.solver.api.symbol.PredSym;
import flix.runtime.RuleError;
import flix.runtime.fixpoint.predicate.AtomPredicate;
import flix.runtime.fixpoint.predicate.Predicate;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;

public final class SolverApi {

    /**
     * Returns the composition of `s1` with `s2`.
     */
    public static ConstraintSystem compose(ConstraintSystem s1, ConstraintSystem s2) {
        var facts = concat(s1.getFacts(), s2.getFacts());
        var rules = concat(s1.getRules(), s2.getRules());
        return new ConstraintSystem(facts, rules);
    }

    /**
     * Returns the projection of the given predicate symbol `sym` of the given constraint system `s`.
     */
    public static ConstraintSystem project(PredSym sym, ConstraintSystem s) {
        if (sym == null)
            throw new IllegalArgumentException("'sym' must be non-null.");
        if (s == null)
            throw new IllegalArgumentException("'s' must be non-null.");

        // Collect all facts with `sym` in its head.
        var facts = new LinkedList<Constraint>();
        for (Constraint fact : s.getFacts()) {
            Predicate head = fact.getHeadPredicate();
            if (head instanceof AtomPredicate) {
                if (((AtomPredicate) head).getSym() == sym) {
                    facts.add(fact);
                }
            }
        }

        return new ConstraintSystem(facts.toArray(new Constraint[0]), new Constraint[0]);
    }

    /**
     * Solves the given constraint system `cs` with the given stratification `stf` and fixpoint options `fo`.
     */
    public static ConstraintSystem solve(ConstraintSystem cs, Stratification stf, FixpointOptions fo) {
        Solver solver = new Solver(cs, stf, fo);
        ConstraintSystem result = solver.solve();
        System.out.println(result.toString());
        return result;
    }

    /**
     * Checks the given constraint system `cs` with the given stratification `stf` and fixpoint options `fo`.
     */
    public static boolean check(ConstraintSystem cs, Stratification stf, FixpointOptions fo) {
        try {
            Solver solver = new Solver(cs, stf, fo);
            solver.solve();
            return true;
        } catch (RuleError e) {
            return false;
        }
    }

    /**
     * Delta Solves the given constraint system `cs` with the given stratification `stf` and fixpoint options `fo`.
     */
    public static String deltaSolve(ConstraintSystem cs, Stratification stf, FixpointOptions fo) {
        // TODO: Update return type.
        DeltaSolver deltaSolver = new DeltaSolver(cs, stf, fo);
        return deltaSolver.deltaSolve();
    }

    /**
     * Returns `true` if all facts in `cs2` are included in `sc1`.
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
