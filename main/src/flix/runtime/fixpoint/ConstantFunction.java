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

import flix.runtime.ProxyObject;

import java.util.function.Function;

/**
 * Represents a constant function.
 */
public final class ConstantFunction implements Function<Object, ProxyObject> {

    /**
     * Constructs a constant function for the given `value`.
     */
    public static ConstantFunction of(ProxyObject value) {
        if (value == null)
            throw new RuntimeException("'value' must be non-null.");

        return new ConstantFunction(value);
    }

    /**
     * The value the function always returns.
     */
    private final ProxyObject value;

    /**
     * Private constructor.
     */
    private ConstantFunction(ProxyObject value) {
        this.value = value;
    }

    /**
     * Returns the value regardless of the argument.
     */
    @Override
    public ProxyObject apply(Object o) {
        return value;
    }

}
