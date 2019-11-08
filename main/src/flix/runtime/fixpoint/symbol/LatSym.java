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

import java.util.HashMap;
import java.util.Map;

/**
 * Represents a lattice symbol.
 */
public final class LatSym implements PredSym {

    /**
     * An internal cache of lattice symbols.
     */
    private static final Map<String, LatSym> INTERNAL_CACHE = new HashMap<>();

    /**
     * Returns the lattice symbol for the given `name`.
     */
    public synchronized static LatSym of(String name, int arity, /* nullable */ String[] attributes, LatticeOps ops) {
        if (name == null)
            throw new IllegalArgumentException("'name' must be non-null.");
        if (attributes != null && attributes.length != arity)
            throw new IllegalArgumentException("'attributes' must have the same length as 'arity'.");
        if (ops == null)
            throw new IllegalArgumentException("'ops' must be non-null.");

        var lookup = INTERNAL_CACHE.get(name);
        if (lookup != null) {
            return lookup;
        }
        var sym = new LatSym(name, arity, attributes, ops);
        INTERNAL_CACHE.put(name, sym);
        return sym;
    }

    /**
     * The name of the lattice symbol.
     */
    private final String name;

    /**
     * The arity of the lattice symbol.
     */
    private final int arity;

    /**
     * The optional attributes of the lattice symbol.
     */
    private final String[] attributes;

    /**
     * The lattice operations of the lattice symbol.
     */
    private final LatticeOps ops;

    /**
     * Constructs a fresh lattice symbol with the given `name`, `arity`, `attributes`, and `ops`.
     */
    private LatSym(String name, int arity, String[] attributes, LatticeOps ops) {
        this.name = name;
        this.arity = arity;
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
     * Returns the arity of the lattice symbol.
     */
    public int getArity() {
        return arity;
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

    /* equality by identity */

}
