package ca.uwaterloo.flix.runtime.solver.api;

import ca.uwaterloo.flix.runtime.solver.DeltaSolver;
import ca.uwaterloo.flix.runtime.solver.FixpointOptions;
import ca.uwaterloo.flix.runtime.solver.Solver;
import flix.runtime.RuleError;

import java.util.Collections;
import java.util.HashSet;

public final class SolverApi {

    /**
     * Solves the given constraint system `c` with the given stratification `stf` and fixpoint options `o`.
     */
    public static ConstraintSystem solve(ConstraintSystem s, Stratification stf, FixpointOptions o) {
        Solver solver = new Solver(s, o);
        ConstraintSystem result = solver.solve();
        System.out.println(result.toString());
        return result;
    }

    /**
     * Checks the given constraint system `c` with the given stratification `stf` and fixpoint options `o`.
     */
    public static boolean check(ConstraintSystem s, Stratification stf, FixpointOptions o) {
        try {
            Solver solver = new Solver(s, o);
            solver.solve();
            return true;
        } catch (RuleError e) {
            return false;
        }
    }

    /**
     * Delta Solves the given constraint system `c` with the given stratification `stf` and fixpoint options `o`.
     */
    public static String deltaSolve(ConstraintSystem s, Stratification stf, FixpointOptions o) {
        // TODO: Update return type.
        DeltaSolver deltaSolver = new DeltaSolver(s, o);
        return deltaSolver.deltaSolve();
    }

    /**
     * Returns `true` if all facts in `s2` are included in `s1`.
     */
    public static boolean entails(ConstraintSystem s1, ConstraintSystem s2) {
        var entails = true;
        for (Constraint fact2 : s2.getFacts()) {
            var found = false;
            for (Constraint fact1 : s1.getFacts()) {
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

}
