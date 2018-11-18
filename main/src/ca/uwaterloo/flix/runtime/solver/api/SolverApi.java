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

    /**
     * Returns `true` if all facts in `s2` are included in `s1`.
     */
    public static boolean entails(ConstraintSystem s1, ConstraintSystem s2) {
        var facts1 = s1.getFacts();
        var facts2 = s2.getFacts();
        // TODO: Not yet implemented.
        throw new RuntimeException();
    }

}
