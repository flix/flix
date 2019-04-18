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

///
/// The Result type.
///
/// A result represents a successful value or an error value.
/// The constructor `Ok(v)` represents the successful value `v`,
/// whereas the constructor `Err(v)` represents the error value `v`.
///
enum Result[t, e] {
    case Ok(t),
    case Err(e)
}

namespace Result {

    ///
    /// Returns `true` iff `r` is `Ok(v)`.
    ///
    pub def isOk[t, e](r: Result[t, e]): Bool = match r with {
        case Ok(_) => true
        case Err(_) => false
    }

    ///
    /// Returns `true` iff `r` is `Err(w)`.
    ///
    pub def isErr[t, e](r: Result[t, e]): Bool = match r with {
        case Ok(_) => false
        case Err(_) => true
    }

    ///
    /// Returns `v` if `r` is `Ok(v)`. Otherwise returns `d`.
    ///
    pub def getWithDefault[t, e](r: Result[t, e], d: t): t = match r with {
        case Ok(v) => v
        case Err(_) => d
    }

    ///
    /// Returns `Ok(v)` if `r1` is `Ok(v)`. Otherwise returns `r2`.
    ///
    pub def withDefault[t, e1, e2](r1: Result[t, e1], r2: Result[t, e2]): Result[t, e2] = match r1 with {
        case Ok(v) => Ok(v)
        case Err(_) => r2
    }

    ///
    /// Returns `Ok(y)` if `r` is `Ok(x)`. Otherwise returns `r`.
    ///
    pub def replace[t, e](x: t, y: t, r: Result[t, e]): Result[t, e] = match r with {
        case Ok(v) => Ok(if (v == x) y else v)
        case Err(_) => r
    }

    ///
    /// Returns `true` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `false`.
    ///
    pub def exists[t, e](f: t -> Bool, r: Result[t, e]): Bool = getWithDefault(map(f, r), false)

    ///
    /// Returns `true` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true` or if `r` is `Err(w)`.
    /// Otherwise returns `false`.
    ///
    pub def forall[t, e](f: t -> Bool, r: Result[t, e]): Bool = getWithDefault(map(f, r), true)

    ///
    /// Returns `Ok(f(v))` if `r` is `Ok(v)`. Returns `Err(w)` if `r` is `Err(w)`.
    ///
    pub def map[t1, t2, e](f: t1 -> t2, r: Result[t1, e]): Result[t2, e] = match r with {
        case Ok(v) => Ok(f(v))
        case Err(w) => Err(w)
    }

    ///
    /// Returns `f(v)` if `r` is `Ok(v)`. Returns `Err(w)` if `r` is `Err(w)`.
    ///
    pub def flatMap[t1, t2, e](f: t1 -> Result[t2, e], r: Result[t1, e]): Result[t2, e] = match r with {
        case Ok(v) => f(v)
        case Err(w) => Err(w)
    }

    ///
    /// Returns `1` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `0`.
    ///
    pub def count[t, e](f: t -> Bool, r: Result[t, e]): Int32 = match r with {
        case Ok(v) => if (f(v)) 1 else 0
        case Err(_) => 0
    }

    ///
    /// Returns `Some(v)` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `None`.
    ///
    pub def find[t, e](f: t -> Bool, r: Result[t, e]): Option[t] = match r with {
        case Ok(v) => if (f(v)) Some(v) else None
        case Err(_) => None
    }

    ///
    /// Returns `f(z, v)` if `r` is `Ok(v)`. Otherwise returns `z`.
    ///
    pub def foldLeft[t, e, a](f: (a, t) -> a, z: a, r: Result[t, e]): a = match r with {
        case Ok(v) => f(z, v)
        case Err(_) => z
    }

    ///
    /// Returns `f(v, z)` if `r` is `Ok(v)`. Otherwise returns `z`.
    ///
    pub def foldRight[t, e, a](f: (t, a) -> a, r: Result[t, e], z: a): a = match r with {
        case Ok(v) => f(v, z)
        case Err(_) => z
    }

    ///
    /// Returns a one-element list of the value `v` if `r` is `Ok(v)`. Otherwise returns the empty list.
    ///
    pub def toList[t, e](r: Result[t, e]): List[t] = match r with {
        case Ok(v) => v :: Nil
        case Err(_) => Nil
    }

    ///
    /// Returns a one-element set of the value `v` if `r` is `Ok(v)`. Otherwise returns the empty set.
    ///
    pub def toSet[t, e](r: Result[t, e]): Set[t] = match r with {
        case Ok(v) => #{v}
        case Err(_) => #{}
    }

    ///
    /// Returns a singleton map with the mapping `k -> v` if `o` is `Ok((k, v))`. Otherwise returns the empty map.
    ///
    pub def toMap[k, v, e](r: Result[(k, v), e]): Map[k, v] = match r with {
        case Ok((k, v)) => Map.singleton(k, v)
        case Err(_) => @{}
    }

    ///
    /// Returns `Some(v)` if `r` is `Ok(v)`. Otherwise returns `None`.
    ///
    pub def toOption[t, e](r: Result[t, e]): Option[t] = match r with {
        case Ok(v) => Some(v)
        case Err(_) => None
    }






    ///
    /// Returns `true` if and only if `r1` and `r2` and equal.
    ///
    pub def eq[t, e](r1: Result[t, e], r2: Result[t, e]): Bool = match (r1, r2) with {
        case (Ok(v1), Ok(v2)) => v1 == v2
        case (Err(w1), Err(w2)) => w1 == w2
        case _ => false
    }
}