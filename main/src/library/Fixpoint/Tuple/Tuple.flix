/*
 * Copyright 2021 Benjamin Dahse
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

namespace Fixpoint/Tuple {

    ///
    /// Uses unsafe immutable array internally for performance.
    ///
    pub opaque enum Tuple[v] {
        case Tuple(Array[v, Static])
    }

    // NB: Unsafe instance.
    instance Eq[Tuple[v]] with Eq[v] {
        pub def eq(x: Tuple[v], y: Tuple[v]): Bool =
            let Tuple.Tuple(a) = x;
            let Tuple.Tuple(b) = y;
            unsafe_cast Array.sameElements(a, b) as _ \ {}
    }

    // NB: Unsafe instance.
    instance Order[Tuple[v]] with Order[v] {
        pub def compare(x: Tuple[v], y: Tuple[v]): Comparison =
            let Tuple.Tuple(a) = x;
            let Tuple.Tuple(b) = y;
            unsafe_cast Array.compare(a, b) as _ \ {}
    }

    ///
    /// Creates an empty tuple consisting of the elements in `l`.
    ///
    pub def new(l: f[v]): Tuple[v] with Foldable[f] =
        unsafe_cast Tuple.Tuple(Foldable.toArray(Static, l)) as _ \ {}

    ///
    /// Converts `t` to a list.
    ///
    pub def toList(t: Tuple[v]): List[v] = unsafe_cast {
        let Tuple.Tuple(a) = t;
        Array.toList(a)
    } as _ \ {}

    ///
    /// Returns the length of `t`.
    ///
    pub def length(t: Tuple[v]): Int32 =
        let Tuple.Tuple(a) = t;
        Array.length(a)

    ///
    /// Returns the value at index `i` in `t`.
    /// Throws a runtime exception if `v` is out of bounds.
    ///
    pub def valueAt(t: Tuple[v], v: Int32): v = unsafe_cast {
        let Tuple.Tuple(a) = t;
        a[v]
    } as _ \ {}
}
