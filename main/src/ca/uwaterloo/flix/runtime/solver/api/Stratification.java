package ca.uwaterloo.flix.runtime.solver.api;

import ca.uwaterloo.flix.runtime.solver.api.symbol.PredSym;

import java.util.Map;

public class Stratification {

    private final Map<PredSym, Integer> stratification;

    public Stratification(Map<PredSym, Integer> stratification) {
        this.stratification = stratification;
    }

    // TODO: Implement and use in solver.

}
