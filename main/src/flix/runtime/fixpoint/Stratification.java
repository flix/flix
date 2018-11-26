package flix.runtime.fixpoint;

import flix.runtime.fixpoint.symbol.PredSym;

import java.util.HashMap;
import java.util.Map;

/**
 * Represents a stratification.
 */
public final class Stratification {

    /**
     * A map from parameterless predicate symbols to their stratum.
     */
    private final Map<PredSym, Integer> stratification = new HashMap<>();

    /**
     * Returns the stratum of the given predicate symbol `sym`.
     */
    public int getStratum(PredSym sym) {
        if (sym == null)
            throw new IllegalArgumentException("'sym' must be non-null.");

        // Retrieve the stratum.
        var result = stratification.get(sym.getParameterless());
        if (result == null) {
            return 0; // TODO: Need to verify that this matches the control flow analysis.
        }

        return result;
    }

    /**
     * Sets the stratum of the given predicate symbol `sym` to the given stratum `stratum`.
     */
    public void setStratum(PredSym sym, int stratum) {
        if (sym == null)
            throw new IllegalArgumentException("'sym' must be non-null.");

        stratification.put(sym.getParameterless(), stratum);
    }

}
