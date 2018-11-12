package ca.uwaterloo.flix.runtime.solver.api;

import ca.uwaterloo.flix.runtime.solver.FixpointOptions;
import ca.uwaterloo.flix.runtime.solver.Solver;

public final class SolverApi {

    public static ConstraintSystem solve(ConstraintSystem s, FixpointOptions o) {
        Solver solver = new Solver(s, o);
        return solver.solve();
    }

}
