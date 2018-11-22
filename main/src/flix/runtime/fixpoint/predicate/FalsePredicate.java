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

/**
 * Represents the false predicate.
 */
public final class FalsePredicate implements Predicate {

    /**
     * Private singleton instance.
     */
    private static final FalsePredicate INSTANCE = new FalsePredicate();

    /**
     * Private constructor.
     */
    private FalsePredicate() {
        /* empty constructor */
    }

    /**
     * Returns the singleton instance of the false predicate.
     */
    public static FalsePredicate getSingleton() {
        return INSTANCE;
    }

    /**
     * Returns a human-readable representation of `this` predicate.
     */
    @Override
    public String toString() {
        return "false.";
    }

    /* equality by identity. */

}
