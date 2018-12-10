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
 * A collection of lattice operations.
 */
public final class LatticeOps {

    /**
     * Returns a new lattice ops object for the given lattice components.
     */
    public static LatticeOps of(Function<Object[], ProxyObject> bot, Function<Object[], ProxyObject> equ, Function<Object[], ProxyObject> leq, Function<Object[], ProxyObject> lub, Function<Object[], ProxyObject> glb) {
        if (bot == null)
            throw new IllegalArgumentException("'bot' must be non-null");
        if (equ == null)
            throw new IllegalArgumentException("'equ' must be non-null");
        if (leq == null)
            throw new IllegalArgumentException("'leq' must be non-null");
        if (lub == null)
            throw new IllegalArgumentException("'lub' must be non-null");
        if (glb == null)
            throw new IllegalArgumentException("'glb' must be non-null");

        return new LatticeOps(bot, equ, leq, lub, glb);
    }

    /**
     * Returns the bottom element.
     */
    private Function<Object[], ProxyObject> bot;

    /**
     * Returns the equality function.
     */
    private Function<Object[], ProxyObject> equ;

    /**
     * Returns the partial order function.
     */
    private Function<Object[], ProxyObject> leq;

    /**
     * Returns the least upper bound function.
     */
    private Function<Object[], ProxyObject> lub;

    /**
     * Returns the greatest lower bound function.
     */
    private Function<Object[], ProxyObject> glb;

    /**
     * Private constructor.
     */
    private LatticeOps(Function<Object[], ProxyObject> bot, Function<Object[], ProxyObject> equ, Function<Object[], ProxyObject> leq, Function<Object[], ProxyObject> lub, Function<Object[], ProxyObject> glb) {
        this.bot = bot;
        this.equ = equ;
        this.leq = leq;
        this.lub = lub;
        this.glb = glb;
    }

    /**
     * Returns the bottom element.
     */
    public ProxyObject bot() {
        return this.bot.apply(new Object[1]);
    }

    /**
     * Returns `true` if `x` is equal to `y`.
     */
    public boolean equ(Object x, Object y) {
        return (Boolean) this.equ.apply(new Object[]{x, y}).getValue();
    }

    /**
     * Returns `true` if `x` is less than or equal to `y`.
     */
    public boolean leq(Object x, Object y) {
        return (Boolean) this.leq.apply(new Object[]{x, y}).getValue();
    }

    /**
     * Returns the least upper bound of `x` and `y`.
     */
    public ProxyObject lub(Object x, Object y) {
        return this.lub.apply(new Object[]{x, y});
    }

    /**
     * Returns the greatest lower bound of `x` and `y`.
     */
    public ProxyObject glb(Object x, Object y) {
        return this.glb.apply(new Object[]{x, y});
    }

}
