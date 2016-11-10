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
    /// Returns `v` if `r` is `Ok(v)`. Undefined semantics if `r` is `Err(w)`.
    ///
    def get[t, e](r: Result[t, e]): t = match r with {
        case Ok(v) => v
    }

    ///
    /// Returns `v` if `r` is `Ok(v)`. Otherwise returns `d`.
    ///
    def getWithDefault[t, e](r: Result[t, e], d: t): t = match r with {
        case Ok(v) => v
        case Err(w) => d
    }

    ///
    /// Returns `r1` if it is `Ok(v)`. Otherwise returns `r2`.
    ///
    def withDefault[t, e1, e2](r1: Result[t, e1], r2: Result[t, e2]): Result[t, e2] = match r1 with {
        case Ok(v) => r1
        case Err(w) => r2
    }

    ///
    /// Returns `true` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `false`.
    ///
    def exists[t, e](f: t -> Bool, r: Result[t, e]): Bool = getWithDefault(map(f, r), false)

    ///
    /// Returns `true` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true` or if `r` is `Err(w)`.
    /// Otherwise returns `false`.
    ///
    def forall[t, e](f: t -> Bool, r: Result[t, e]): Bool = getWithDefault(map(f, r), true)

    ///
    /// Returns `r` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to true. Otherwise returns `Err(d)`.
    ///
    def filter[t, e](f: t -> Bool, r: Result[t, e], d: e): Result[t, e] = match r with {
        case Ok(v) => if (f(v)) r else Err(d)
        case Err(w) => Err(d)
    }

    ///
    /// Returns `Ok(f(v))` if `r` is `Ok(v)`. Otherwise returns `r`.
    ///
    def map[t1, t2, e](f: t1 -> t2, r: Result[t1, e]): Result[t2, e] = match r with {
        case Ok(v) => Ok(f(v))
        case Err(w) => r
    }

    ///
    /// Returns `Ok(f(v1, v2))` if `r1` is `Ok(v1)` and `r2` is `Ok(v2)`.
    /// Otherwise returns the first of `r1` and `r2` that is `Err(w)`.
    ///
    def map2[t1, t2, t3, e](f: (t1, t2) -> t3, r1: Result[t1, e], r2: Result[t2, e]): Result[t3, e] = match (r1, r2) with {
        case (Err(w1), _) => r1
        case (_, Err(w2)) => r2
        case (Ok(v1), Ok(v2)) => Ok(f(v1, v2))
    }

    ///
    /// Returns `f(v)` if `r` is `Ok(v)`. Otherwise returns `r`.
    ///
    def flatMap[t1, t2, e](f: t1 -> Result[t2, e], r: Result[t1, e]): Result[t2, e] = match r with {
        case Ok(v) => f(v)
        case Err(w) => r
    }

    ///
    /// Returns `f(v1, v2)` if `r1` is `Ok(v1)` and `r2` is `Ok(v2)`.
    /// Otherwise returns the first of `r1` and `r2` that is `Err(w)`.
    ///
    def flatMap2[t1, t2, t3, e](f: (t1, t2) -> Result[t3, e], r1: Result[t1, e], r2: Result[t2, e]): Result[t3, e] = match (r1, r2) with {
        case (Err(w1), _) => r1
        case (_, Err(w2)) => r2
        case (Ok(v1), Ok(v2)) => f(v1, v2)
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
    /// Returns `r` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `Err(d)`.
    ///
    def find[t, e](f: t -> Bool, r: Result[t, e], d: e): Result[t, e] = match r with {
        case Ok(v) => if (f(v)) r else Err(d)
        case Err(w) => Err(d)
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

    ///
    /// Returns a one-element list of the value `v` if `r` is `Ok(v)`. Otherwise returns the empty list.
    ///
    def toList[t, e](r: Result[t, e]): List[t] = match r with {
        case Ok(v) => v :: Nil
        case Err(w) => Nil
    }

    ///
    /// Returns a one-element set of the value `v` if `r` is `Ok(v)`. Otherwise returns the empty set.
    ///
    def toSet[t, e](r: Result[t, e]): Set[t] = match r with {
        case Ok(v) => #{v}
        case Err(w) => #{}
    }

    ///
    /// Returns `Some(v)` if `r` is `Ok(v)`. Otherwise returns `None`.
    ///
    def toOption[t, e](r: Result[t, e]): Option[t] = match r with {
        case Ok(v) => Some(v)
        case Err(w) => None
    }

    ///
    /// Returns `Ok((v1, v2))` if `r1` is `Ok(v1)` and `r2` is `Ok(v2)`.
    /// Otherwise returns the first of `r1` and `r2` that is `Err(w)`.
    ///
//    def zip[t1, t2, e](r1: Result[t1, e], r2: Result[t2, e]): Result[(t1, t2), e] = match (r1, r2) with {
//        case (Err(w1), _) => r1
//        case (_, Err(w2)) => r2
//        case (Ok(v1), Ok(v2)) => Ok((v1, v2))
//    }

    ///
    /// Returns `(Ok(v1), Ok(v2))` if `r` is `Ok((v1, v2))`. Otherwise returns `(r, r)`.
    ///
//    def unzip[t1, t2, e](r: Result[(t1, t2), e]): (Result[t1, e], Result[t2, e]) = match r with {
//        case Ok((v1, v2)) => (Ok(v1), Ok(v2))
//        case Err(w) => (r, r)
//    }
}