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
    def isOk[a, b](r: Result[a, b]): Bool = match r with {
        case Ok(v) => true
        case Err(e) => false
    }

    ///
    /// Returns `true` iff `r` is `Err(e)`.
    ///
    def isErr[a, b](r: Result[a, b]): Bool = match r with {
        case Ok(v) => false
        case Err(e) => true
    }

    ///
    /// Returns `v` if `r` is `Ok(v)`. Undefined semantics if `r` is `Err(e)`.
    ///
    def ok[a, b](r: Result[a, b]): a = match r with {
        case Ok(v) => v
    }

    ///
    /// Returns `e` if `r` is `Err(e)`. Undefined semantics if `r` is `Ok(v)`.
    ///
    def err[a, b](r: Result[a, b]): b = match r with {
        case Err(e) => e
    }

    ///
    /// Returns `v` if `r` is `Ok(v)`. Otherwise returns `d`.
    ///
    def okWithDefault[a, b](r: Result[a, b], d: a): a = match r with {
        case Ok(v) => v
        case Err(e) => d
    }

    ///
    /// Returns `e` if `r` is `Err(e)`. Otherwise returns `d`.
    ///
    def errWithDefault[a, b](r: Result[a, b], d: b): b = match r with {
        case Ok(v) => d
        case Err(e) => e
    }

    ///
    /// Returns `Ok(f(v))` if `r` is `Ok(v)`.  Otherwise returns `r`.
    ///
    def mapOk[a, b, c](r: Result[a, b], f: a -> c): Result[c, b] = match r with {
        case Ok(v) => Ok(f(v))
        case Err(e) => r
    }

    ///
    /// Returns `Err(f(e))` if `r` is `Err(e)`.  Otherwise returns `r`.
    ///
    def mapErr[a, b, c](r: Result[a, b], f: b -> c): Result[a, c] = match r with {
        case Ok(v) => r
        case Err(e) => Err(f(e))
    }

    ///
    /// Returns `r2` if `r1` is `Ok(v)`.  Otherwise returns `r1`.
    ///
    def and[a, b](r1: Result[a, b], r2: Result[a, b]): Result[a, b] = match r1 with {
        case Ok(v) => r2
        case Err(e) => r1
    }

    ///
    /// Returns `r1` if `r1` is `Ok(v)`.  Otherwise returns `r2`.
    ///
    def or[a, b](r1: Result[a, b], r2: Result[a, b]): Result[a, b] = match r1 with {
        case Ok(v) => r1
        case Err(e) => r2
    }

    ///
    /// Returns `1` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `0`.
    ///
    def count[a, b](f: a -> Bool, r: Result[a, b]): Int32 = match r with {
        case Ok(v) => if (f(v)) 1 else 0
        case Err(e) => 0
    }

    ///
    /// Returns `f(v)` if `r` is `Ok(v)`. Otherwise returns `z`.
    ///
    def foldLeft[a, b, c](f: a -> b, z: b, r: Result[a, c]): b = match r with {
        case Ok(v) => f(v)
        case Err(e) => z
    }

    ///
    /// Returns `f(v)` if `r` is `Ok(v)`. Otherwise returns `z`.
    ///
    def foldRight[a, b, c](f: a -> b, r: Result[a, c], z: b): b = match r with {
        case Ok(v) => f(v)
        case Err(e) => z
    }
}