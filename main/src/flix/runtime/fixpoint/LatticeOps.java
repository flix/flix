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
public interface LatticeOps {

    /**
     * Returns the bottom element.
     */
    ProxyObject bot();

    /**
     * Returns the equality function.
     */
    Function<Object[], ProxyObject> equ();

    /**
     * Returns the partial order function.
     */
    Function<Object[], ProxyObject> leq();

    /**
     * Returns the least upper bound function.
     */
    Function<Object[], ProxyObject> lub();

    /**
     * Returns the greatest lower bound function.
     */
    Function<Object[], ProxyObject> glb();

}
