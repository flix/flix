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

package flix.runtime.fixpoint.term;

import flix.runtime.fixpoint.symbol.VarSym;

import java.util.Objects;

/**
 * Represents a variable term.
 */
public final class VarTerm implements Term {

    /**
     * Constructs a new variable term from the given variable symbol `sym`.
     */
    public static VarTerm of(VarSym sym) {
        if (sym == null)
            throw new IllegalArgumentException("'sym' must be non-null.");

        return new VarTerm(sym);
    }

    /**
     * The variable symbol.
     */
    private final VarSym sym;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        VarTerm varTerm = (VarTerm) o;
        return sym.equals(varTerm.sym);
    }

    @Override
    public int hashCode() {
        return Objects.hash(sym);
    }

    /**
     * Private constructor.
     */
    private VarTerm(VarSym sym) {
        this.sym = sym;
    }

    /**
     * Returns the variable symbol of `this` variable term.
     */
    public VarSym getSym() {
        return sym;
    }

    /**
     * Returns a human-readable representation of `this` term.
     */
    @Override
    public String toString() {
        return sym.toString();
    }

}
