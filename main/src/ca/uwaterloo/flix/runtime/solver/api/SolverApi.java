package ca.uwaterloo.flix.runtime.solver.api;

import ca.uwaterloo.flix.runtime.solver.DeltaSolver;
import ca.uwaterloo.flix.runtime.solver.FixpointOptions;
import ca.uwaterloo.flix.runtime.solver.Solver;
import flix.runtime.RuleError;

import java.util.Collections;
import java.util.HashSet;

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
        var facts1 = new HashSet<Constraint>();
        var facts2 = new HashSet<Constraint>();

        Collections.addAll(facts1, s1.getFacts());
        Collections.addAll(facts2, s2.getFacts());

        facts2.removeAll(facts1);

        // If facts2 is empty then every fact in s2 appears in s1.
        return facts2.isEmpty();
    }

}
