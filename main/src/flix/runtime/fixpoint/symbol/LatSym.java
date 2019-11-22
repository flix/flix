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

import flix.runtime.fixpoint.LatticeOps;

import java.util.Objects;

/**
 * Represents a lattice symbol.
 */
public final class LatSym implements PredSym {

    /**
     * Returns the lattice symbol for the given `name`.
     */
    public static LatSym of(String name, /* nullable */ String[] attributes, LatticeOps ops) {
        if (name == null)
            throw new IllegalArgumentException("'name' must be non-null.");
        if (ops == null)
            throw new IllegalArgumentException("'ops' must be non-null.");

        return new LatSym(name, attributes, ops);
    }

    /**
     * The name of the lattice symbol.
     */
    private final String name;

    /**
     * The optional attributes of the lattice symbol.
     */
    private final String[] attributes;

    /**
     * The lattice operations of the lattice symbol.
     */
    private final LatticeOps ops;

    /**
     * Constructs a fresh lattice symbol with the given `name`, `attributes`, and `ops`.
     */
    private LatSym(String name, String[] attributes, LatticeOps ops) {
        this.name = name.intern();
        this.attributes = attributes;
        this.ops = ops;
    }

    /**
     * Returns the name of the lattice symbol.
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the attributes of the relation symbol.
     */
    public String[] getAttributes() {
        return attributes;
    }

    /**
     * Returns the associated lattice operations.
     */
    public LatticeOps getOps() {
        return ops;
    }

    /**
     * Returns a human-readable representation of `this` lattice symbol.
     */
    @Override
    public String toString() {
        return name;
    }

    /**
     * Equality defined by on the symbol name.
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        LatSym latSym = (LatSym) o;
        return Objects.equals(name, latSym.name);
    }

    /**
     * Equality defined by on the symbol name.
     */
    @Override
    public int hashCode() {
        return Objects.hash(name);
    }
}
