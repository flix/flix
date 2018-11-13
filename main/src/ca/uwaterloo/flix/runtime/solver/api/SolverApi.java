package ca.uwaterloo.flix.runtime.solver.api;

import ca.uwaterloo.flix.runtime.solver.DeltaSolver;
import ca.uwaterloo.flix.runtime.solver.FixpointOptions;
import ca.uwaterloo.flix.runtime.solver.Solver;
import flix.runtime.RuleError;

public final class SolverApi {

    /**
     * Solves the given constraint system `c` with the given fixpoint options `o`.
     */
    public static ConstraintSystem solve(ConstraintSystem s, FixpointOptions o) {
        Solver solver = new Solver(s, o);
        ConstraintSystem result = solver.solve();
        System.out.println(result.toString());
        return result;
    }

    /**
     * Checks the given constraint system `c` with the given fixpoint options `o`.
     */
    public static boolean check(ConstraintSystem s, FixpointOptions o) {
        try {
            Solver solver = new Solver(s, o);
            solver.solve();
            return true;
        } catch (RuleError e) {
            return false;
        }
    }

    /**
     * Delta Solves the given constraint system `c` with the given fixpoint options `o`.
     */
    public static String deltaSolve(ConstraintSystem s, FixpointOptions o) {
        // TODO: Update return type.
        DeltaSolver deltaSolver = new DeltaSolver(s, o);
        return deltaSolver.deltaSolve();
    }

}
