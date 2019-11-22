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
 * Represents a relation symbol.
 */
public final class RelSym implements PredSym {

    /**
     * Returns the relation symbol for the given `name`.
     */
    public static RelSym of(String name, /* nullable */ String[] attributes) {
        if (name == null)
            throw new IllegalArgumentException("'name' must be non-null.");

        return new RelSym(name, attributes);
    }

    /**
     * The unique name of the relation symbol.
     */
    private final String name;

    /**
     * The optional attributes of the relation symbol.
     */
    private final String[] attributes;

    /**
     * Constructs a fresh relation symbol with the given `name` and `attributes`.
     */
    private RelSym(String name, String[] attributes) {
        this.name = name.intern();
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


    /**
     * Equality defined on the symbol name.
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        RelSym relSym = (RelSym) o;
        return Objects.equals(name, relSym.name);
    }

    /**
     * Equality defined on the symbol name.
     */
    @Override
    public int hashCode() {
        return Objects.hash(name);
    }
}
