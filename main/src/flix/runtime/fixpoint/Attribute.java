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

/**
 * Represents a relation or lattice attribute.
 */
public final class Attribute {

    /**
     * Constructs a new attribute with the given name.
     */
    public static Attribute of(String name) {
        if (name == null)
            throw new IllegalArgumentException("'name' must be non-null.");

        return new Attribute(name);
    }

    /**
     * The name of the attribute.
     */
    private final String name;

    /**
     * Private constructor.
     */
    private Attribute(String name) {
        this.name = name;
    }

    /**
     * Returns the name of the attribute.
     */
    public String getName() {
        return name;
    }

}
