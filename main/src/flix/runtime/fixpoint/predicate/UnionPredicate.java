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

package flix.runtime.fixpoint.predicate;

import flix.runtime.ProxyObject;
import flix.runtime.fixpoint.term.Term;

import java.util.function.Function;

/**
 * Represents a union predicate with function `f` and arguments `terms`.
 */
public final class UnionPredicate implements Predicate {

    /**
     * Constructs a union predicate for the given function and function arguments.
     */
    public static UnionPredicate of(Function<Object[], ProxyObject> function, Term[] arguments) {
        if (function == null)
            throw new IllegalArgumentException("'function' must be non-null.");

        if (arguments == null)
            throw new IllegalArgumentException("'arguments' must be non-null.");

        return new UnionPredicate(function, arguments);
    }

    /**
     * The function.
     */
    private final Function<Object[], ProxyObject> function;

    /**
     * The function arguments.
     */
    private final Term[] arguments;

    /**
     * Private constructor.
     */
    private UnionPredicate(Function<Object[], ProxyObject> function, Term[] arguments) {
        this.function = function;
        this.arguments = arguments;
    }

    /**
     * Returns the function.
     */
    public Function<Object[], ProxyObject> getFunction() {
        return function;
    }

    /**
     * Returns the arguments.
     */
    public Term[] getArguments() {
        return arguments;
    }

    /**
     * Returns a human-readable representation of `this` predicate.
     */
    @Override
    public String toString() {
        return "<union>";
    }

}
