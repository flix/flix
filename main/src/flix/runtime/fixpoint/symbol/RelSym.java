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

import flix.runtime.fixpoint.Attribute;
import flix.runtime.ProxyObject;
import flix.runtime.value.Unit;

import java.util.HashMap;
import java.util.Map;

/**
 * Represents a parameterized relation symbol.
 */
public final class RelSym implements PredSym {

    /**
     * An internal cache of relation symbols.
     */
    private static final Map<NameAndParameter, RelSym> INTERNAL_CACHE = new HashMap<>();

    /**
     * Returns the relation symbol for the given `name` with the given `parameter` and `attributes`.
     * <p>
     * The parameter may be null.
     */
    public synchronized static RelSym of(String name, ProxyObject parameter, Attribute[] attributes) {
        if (name == null)
            throw new IllegalArgumentException("'name' must be non-null.");
        if (attributes == null)
            throw new IllegalArgumentException("'attributes' must be non-null.");

        var key = new NameAndParameter(name, parameter);
        var lookup = INTERNAL_CACHE.get(key);
        if (lookup != null) {
            return lookup;
        }
        var sym = new RelSym(name, parameter, attributes);
        INTERNAL_CACHE.put(key, sym);
        return sym;
    }

    /**
     * The unique name of the relation symbol.
     */
    private final String name;

    /**
     * The parameter of the relation symbol.
     */
    private final ProxyObject parameter;

    /**
     * The attributes of the relation symbol.
     */
    private final Attribute[] attributes;

    /**
     * Constructs a fresh relation symbol with the given `name` and `parameter`.
     */
    private RelSym(String name, ProxyObject parameter, Attribute[] attributes) {
        this.name = name;
        this.parameter = parameter;
        this.attributes = attributes;
    }

    /**
     * Returns the name of the relation symbol.
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the parameter of the relation symbol.
     */
    public ProxyObject getParameter() {
        return parameter;
    }

    /**
     * Returns the attributes of the relation symbol.
     */
    public Attribute[] getAttributes() {
        return attributes;
    }

    /**
     * Returns the parameterless version of `this` relation symbol.
     */
    public RelSym getParameterless() {
        return of(name, null, attributes);
    }

    /**
     * Returns a human-readable representation of `this` relation symbol.
     */
    @Override
    public String toString() {
        if (parameter == null || parameter.getValue() == Unit.getInstance())
            return name;
        else
            return name + "<" + parameter.toString() + ">";
    }

    /* equality by identity */

}
