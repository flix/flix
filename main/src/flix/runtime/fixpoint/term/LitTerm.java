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

import flix.runtime.ProxyObject;

import java.util.Objects;
import java.util.function.Function;

/**
 * Represents a literal.
 * <p>
 * The literal value is not directly accessible, but instead returned by invocation of the supplied function.
 */
public final class LitTerm implements Term {

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        LitTerm litTerm = (LitTerm) o;
        return function.equals(litTerm.function);
    }

    @Override
    public int hashCode() {
        return Objects.hash(function);
    }

    /**
     * Construct a literal term for the given function.
     */
    public static LitTerm of(Function<Object, ProxyObject> function) {
        if (function == null)
            throw new IllegalArgumentException("'function' must be non-null");

        return new LitTerm(function);
    }

    /**
     * The zero argument function which returns the literal when invoked.
     */
    private final Function<Object, ProxyObject> function;

    /**
     * Private constructor.
     */
    private LitTerm(Function<Object, ProxyObject> function) {
        this.function = function;
    }

    /**
     * Returns the function.
     */
    public Function<Object, ProxyObject> getFunction() {
        return function;
    }

    /**
     * Returns a human-readable representation of `this` term.
     */
    @Override
    public String toString() {
        return function.apply(new Object[1]).toString();
    }

}
