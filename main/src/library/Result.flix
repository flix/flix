/*
 * Copyright 2016 Liam Palmer
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

namespace Result {

    ///
    /// Returns `true` iff `r` is `Ok(v)`.
    ///
    def isOk[t, e](r: Result[t, e]): Bool = match r with {
        case Ok(v) => true
        case Err(w) => false
    }

    ///
    /// Returns `true` iff `r` is `Err(w)`.
    ///
    def isErr[t, e](r: Result[t, e]): Bool = match r with {
        case Ok(v) => false
        case Err(w) => true
    }

    ///
    /// Returns `v` if `r` is `Ok(v)`. Otherwise returns `d`.
    ///
    def withDefault[t, e](r: Result[t, e], d: t): t = match r with {
        case Ok(v) => v
        case Err(w) => d
    }

    ///
    /// Returns `Ok(f(v))` if `r` is `Ok(v)`. Otherwise returns `r`.
    ///
    def map[t1, t2, e](f: t1 -> t2, r: Result[t1, e]): Result[t2, e] = match r with {
        case Ok(v) => Ok(f(v))
        case Err(w) => r
    }

    ///
    /// Returns `f(v)` if `r` is `Ok(v)`. Otherwise returns `r`.
    ///
    def flatMap[t1, t2, e](f: t1 -> Result[t2, e], r: Result[t1, e]): Result[t2, e] = match r with {
        case Ok(v) => f(v)
        case Err(w) => r
    }

    ///
    /// Returns `r2` if `r1` is `Ok(v)`. Otherwise returns `r1`.
    ///
    def and[t, e](r1: Result[t, e], r2: Result[t, e]): Result[t, e] = match r1 with {
        case Ok(v) => r2
        case Err(w) => r1
    }

    ///
    /// Returns `r1` if `r1` is `Ok(v)`. Otherwise returns `r2`.
    ///
    def or[t, e](r1: Result[t, e], r2: Result[t, e]): Result[t, e] = match r1 with {
        case Ok(v) => r1
        case Err(w) => r2
    }

    ///
    /// Returns `1` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `0`.
    ///
    def count[t, e](f: t -> Bool, r: Result[t, e]): Int32 = match r with {
        case Ok(v) => if (f(v)) 1 else 0
        case Err(w) => 0
    }

    ///
    /// Returns `f(z, v)` if `r` is `Ok(v)`. Otherwise returns `z`.
    ///
    def foldLeft[t, e, a](f: (a, t) -> a, z: a, r: Result[t, e]): a = match r with {
        case Ok(v) => f(z, v)
        case Err(w) => z
    }

    ///
    /// Returns `f(v, z)` if `r` is `Ok(v)`. Otherwise returns `z`.
    ///
    def foldRight[t, e, a](f: (t, a) -> a, r: Result[t, e], z: a): a = match r with {
        case Ok(v) => f(v, z)
        case Err(w) => z
    }
}