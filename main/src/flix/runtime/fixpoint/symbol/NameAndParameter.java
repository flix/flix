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

import flix.runtime.ProxyObject;

import java.util.Objects;

/**
 * An internal class to represent a pair of a name and a parameter.
 * <p>
 * The parameter may be null.
 */
final class NameAndParameter {

    /**
     * The name. Must be non-null.
     */
    private final String name;

    /**
     * The parameter. May be null.
     */
    private final ProxyObject parameter;

    /**
     * Constructs a new name and parameter pair.
     */
    NameAndParameter(String name, ProxyObject parameter) {
        this.name = name;
        this.parameter = parameter;
    }

    /**
     * Returns `true` if `this` is equal to `that`.
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        NameAndParameter that = (NameAndParameter) o;
        return Objects.equals(name, that.name) &&
                Objects.equals(parameter, that.parameter);
    }

    /**
     * Returns the hash of `this` name and parameter.
     */
    @Override
    public int hashCode() {
        return Objects.hash(name, parameter);
    }
}
