package ca.uwaterloo.flix.runtime.solver.api;

import ca.uwaterloo.flix.runtime.solver.DeltaSolver;
import ca.uwaterloo.flix.runtime.solver.FixpointOptions;
import ca.uwaterloo.flix.runtime.solver.Solver;
import flix.runtime.RuleError;

import java.util.Collections;
import java.util.HashSet;

public final class SolverApi {

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

}
