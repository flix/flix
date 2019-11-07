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
import flix.runtime.fixpoint.Attribute;
import flix.runtime.ProxyObject;
import flix.runtime.value.Unit;

import java.util.HashMap;
import java.util.Map;

/**
 * Represents a parameterized lattice symbol.
 */
public final class LatSym implements PredSym {

    /**
     * An internal cache of lattice symbols.
     */
    private static final Map<String, LatSym> INTERNAL_CACHE = new HashMap<>();

    /**
     * Returns the lattice symbol for the given `name` and `attributes`.
     */
    public synchronized static LatSym of(String name, Attribute[] keys, Attribute value, LatticeOps ops) {
        if (name == null)
            throw new IllegalArgumentException("'name' must be non-null.");
        if (keys == null)
            throw new IllegalArgumentException("'keys' must be non-null.");
        if (value == null)
            throw new IllegalArgumentException("'value' must be non-null.");
        if (ops == null)
            throw new IllegalArgumentException("'ops' must be non-null.");

        var lookup = INTERNAL_CACHE.get(name);
        if (lookup != null) {
            return lookup;
        }
        var sym = new LatSym(name, keys, value, ops);
        INTERNAL_CACHE.put(name, sym);
        return sym;
    }

    /**
     * The name of the lattice symbol.
     */
    private final String name;

    /**
     * The keys of the lattice symbol.
     */
    private final Attribute[] keys;

    /**
     * The value of the lattice symbol.
     */
    private final Attribute value;

    /**
     * The lattice operations of the lattice symbol.
     */
    private final LatticeOps ops;

    /**
     * Constructs a fresh lattice symbol with the given `name`.
     */
    private LatSym(String name, Attribute[] keys, Attribute value, LatticeOps ops) {
        this.name = name;
        this.keys = keys;
        this.value = value;
        this.ops = ops;
    }

    /**
     * Returns the name of the lattice symbol.
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the keys of the lattice symbol.
     */
    public Attribute[] getKeys() {
        return keys;
    }

    /**
     * Returns the value of the lattice symbol.
     */
    public Attribute getValue() {
        return value;
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
