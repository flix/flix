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
import flix.runtime.fixpoint.symbol.VarSym;

import java.util.Arrays;
import java.util.Objects;
import java.util.function.Function;

/**
 * Represents a function application term.
 */
public final class AppTerm implements Term {

    /**
     * Constructs a function application term for the given function and arguments.
     */
    public static AppTerm of(Function<Object[], ProxyObject> function, VarSym[] arguments) {
        if (function == null)
            throw new IllegalArgumentException("'function' must be non-null");
        if (arguments == null)
            throw new IllegalArgumentException("'arguments' must be non-null.");

        return new AppTerm(function, arguments);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        AppTerm appTerm = (AppTerm) o;
        return function.equals(appTerm.function) &&
                Arrays.equals(arguments, appTerm.arguments);
    }

    @Override
    public int hashCode() {
        int result = Objects.hash(function);
        result = 31 * result + Arrays.hashCode(arguments);
        return result;
    }

    /**
     * The function.
     */
    private final Function<Object[], ProxyObject> function;

    /**
     * The function arguments.
     */
    private final VarSym[] arguments;

    /**
     * Private constructor.
     */
    private AppTerm(Function<Object[], ProxyObject> function, VarSym[] arguments) {
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
     * Returns the function arguments.
     */
    public VarSym[] getArguments() {
        return arguments;
    }

    /**
     * Returns a human-readable representation of `this` term.
     */
    @Override
    public String toString() {
        return "f(...)";
    }

}
