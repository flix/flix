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

import java.util.HashMap;
import java.util.Map;

/**
 * Represents a relation symbol.
 */
public final class RelSym implements PredSym {

    /**
     * An internal cache of relation symbols.
     */
    private static final Map<String, RelSym> INTERNAL_CACHE = new HashMap<>();

    /**
     * Returns the relation symbol for the given `name` and `attributes`.
     */
    public synchronized static RelSym of(String name, String[] attributes) {
        if (name == null)
            throw new IllegalArgumentException("'name' must be non-null.");
        if (attributes == null)
            throw new IllegalArgumentException("'attributes' must be non-null.");

        var lookup = INTERNAL_CACHE.get(name);
        if (lookup != null) {
            return lookup;
        }
        var sym = new RelSym(name, attributes);
        INTERNAL_CACHE.put(name, sym);
        return sym;
    }

    /**
     * The unique name of the relation symbol.
     */
    private final String name;

    /**
     * The attributes of the relation symbol.
     */
    private final String[] attributes;

    /**
     * Constructs a fresh relation symbol with the given `name`.
     */
    private RelSym(String name, String[] attributes) {
        this.name = name;
        this.attributes = attributes;
    }

    /**
     * Returns the name of the relation symbol.
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
     * Returns a human-readable representation of `this` relation symbol.
     */
    @Override
    public String toString() {
        return name;
    }

    /* equality by identity */

}
