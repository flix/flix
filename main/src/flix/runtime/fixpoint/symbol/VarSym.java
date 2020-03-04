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

package flix.runtime.fixpoint.symbol;

import java.util.Objects;

/**
 * Represents a variable symbol.
 */
public final class VarSym {

    /**
     * Constructs a new variable symbol with the given `name` and `index`.
     */
    public static VarSym of(String name, int index) {
        if (name == null)
            throw new IllegalArgumentException("'name' must be non-null.");
        if (index < 0)
            throw new IllegalArgumentException("'index' must be non-negative.");

        return new VarSym(name, index);
    }

    /**
     * The name of the variable symbol. (Used for debugging).
     */
    private final String name;

    /**
     * The index of the variable symbol. (Used for evaluation).
     */
    private final int index;

    /**
     * Private constructor.
     */
    private VarSym(String name, int index) {
        this.name = name;
        this.index = index;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        VarSym varSym = (VarSym) o;
        return index == varSym.index &&
                name.equals(varSym.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, index);
    }

    /**
     * Returns the index of `this` symbol.
     */
    public int getIndex() {
        return index;
    }

    /**
     * Returns a human-readable representation of `this` variable symbol.
     */
    @Override
    public String toString() {
        return name + "#" + index;
    }

}
