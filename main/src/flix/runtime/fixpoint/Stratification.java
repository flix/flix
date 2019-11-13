/*
 * Copyright 2018 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package flix.runtime.fixpoint;

import flix.runtime.fixpoint.symbol.PredSym;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Represents a stratification.
 */
public final class Stratification {

    /**
     * A map from predicate symbols to their stratum.
     */
    private final Map<PredSym, Integer> stratification = new HashMap<>();

    /**
     * Returns the stratum of the given predicate symbol `sym`.
     */
    public int getStratum(PredSym sym) {
        if (sym == null)
            throw new IllegalArgumentException("'sym' must be non-null.");

        // Retrieve the stratum.
        var result = stratification.get(sym);
        if (result == null) {
            return 0;
        }

        return result;
    }

    /**
     * Returns the highest stratum.
     */
    public int getMaxStratum() {
        if (stratification.isEmpty()) {
            return 0;
        }
        return Collections.max(stratification.values());
    }

    /**
     * Sets the stratum of the given predicate symbol `sym` to the given stratum `stratum`.
     */
    public void setStratum(PredSym sym, int stratum) {
        if (sym == null)
            throw new IllegalArgumentException("'sym' must be non-null.");

        stratification.put(sym, stratum);
    }

}
