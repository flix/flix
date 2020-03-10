/*
 * Copyright 2019 Liam Palmer, Magnus Madsen
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
    pub def isOk(r: Result[t, e]): Bool = match r {
        case Ok(_) => true
        case Err(_) => false
    }

    ///
    /// Returns `true` iff `r` is `Err(w)`.
    ///
    pub def isErr(r: Result[t, e]): Bool = match r {
        case Ok(_) => false
        case Err(_) => true
    }

    ///
    /// Returns `v` if `r` is `Ok(v)`. Otherwise returns `d`.
    ///
    pub def getWithDefault(r: Result[t, e], d: t): t = match r {
        case Ok(v) => v
        case Err(_) => d
    }

    ///
    /// Returns `Ok(v)` if `r1` is `Ok(v)`. Otherwise returns `r2`.
    ///
    pub def withDefault(r1: Result[t, e1], r2: Result[t, e2]): Result[t, e2] = match r1 {
        case Ok(v) => Ok(v)
        case Err(_) => r2
    }

    ///
    /// Returns `Ok(y)` if `r` is `Ok(x)`. Otherwise returns `r`.
    ///
    pub def replace(x: t, y: t, r: Result[t, e]): Result[t, e] = match r {
        case Ok(v) => Ok(if (v == x) y else v)
        case Err(_) => r
    }

    ///
    /// Returns `true` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `false`.
    ///
    /// The function `f` must be pure.
    ///
    pub def exists(f: t -> Bool, r: Result[t, e]): Bool = match r {
        case Ok(t)  => f(t)
        case Err(_) => false
    }

    ///
    /// Returns `true` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true` or if `r` is `Err(w)`.
    /// Otherwise returns `false`.
    ///
    /// The function `f` must be pure.
    ///
    pub def forall(f: t -> Bool, r: Result[t, e]): Bool = match r {
        case Ok(t)  => f(t)
        case Err(_) => true
    }

    ///
    /// Returns `Ok(f(v))` if `r` is `Ok(v)`. Returns `Err(w)` if `r` is `Err(w)`.
    ///
    pub def map(f: t1 -> t2 & f, r: Result[t1, e]): Result[t2, e] & f = match r {
        case Ok(v) => Ok(f(v))
        case Err(w) => Err(w)
    }

    ///
    /// Returns `f(v)` if `r` is `Ok(v)`. Returns `Err(w)` if `r` is `Err(w)`.
    ///
    pub def flatMap(f: t1 -> Result[t2, e] & f, r: Result[t1, e]): Result[t2, e] & f = match r {
        case Ok(v) => f(v)
        case Err(w) => Err(w)
    }

    ///
    /// Returns `1` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `0`.
    ///
    /// The function `f` must be pure.
    ///
    pub def count(f: t -> Bool, r: Result[t, e]): Int32 = match r {
        case Ok(v) => if (f(v)) 1 else 0
        case Err(_) => 0
    }

    ///
    /// Returns `Some(v)` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `None`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: t -> Bool, r: Result[t, e]): Option[t] = match r {
        case Ok(v) => if (f(v)) Some(v) else None
        case Err(_) => None
    }

    ///
    /// Returns `f(z, v)` if `r` is `Ok(v)`. Otherwise returns `z`.
    ///
    pub def foldLeft(f: (a, t) -> a & f, z: a, r: Result[t, e]): a & f = match r {
        case Ok(v) => f(z, v)
        case Err(_) => z
    }

    ///
    /// Returns `f(v, z)` if `r` is `Ok(v)`. Otherwise returns `z`.
    ///
    pub def foldRight(f: (t, a) -> a & f, r: Result[t, e], z: a): a & f = match r {
        case Ok(v) => f(v, z)
        case Err(_) => z
    }

    ///
    /// Returns a one-element list of the value `v` if `r` is `Ok(v)`. Otherwise returns the empty list.
    ///
    pub def toList(r: Result[t, e]): List[t] = match r {
        case Ok(v) => v :: Nil
        case Err(_) => Nil
    }

    ///
    /// Returns a one-element set of the value `v` if `r` is `Ok(v)`. Otherwise returns the empty set.
    ///
    pub def toSet(r: Result[t, e]): Set[t] = match r {
        case Ok(v)  => Set.singleton(v)
        case Err(_) => Set.empty()
    }

    ///
    /// Returns a singleton map with the mapping `k -> v` if `o` is `Ok((k, v))`. Otherwise returns the empty map.
    ///
    pub def toMap(r: Result[(k, v), e]): Map[k, v] = match r {
        case Ok((k, v)) => Map.singleton(k, v)
        case Err(_)     => Map.empty()
    }

    ///
    /// Returns `Some(v)` if `r` is `Ok(v)`. Otherwise returns `None`.
    ///
    pub def toOption(r: Result[t, e]): Option[t] = match r {
        case Ok(v)  => Some(v)
        case Err(_) => None
    }

    ///
    /// Applies `f` to `v` if `r` is `Ok(v)`. Otherwise does nothing.
    ///
    /// The function `f` must be impure.
    ///
    pub def foreach(f: a ~> Unit, r: Result[t, e]): Unit & Impure = match r {
        case Ok(v)  => f(v)
        case Err(_) => ()
    }

    ///
    /// Returns `true` if and only if `r1` and `r2` and equal.
    ///
    def __eq(r1: Result[t, e], r2: Result[t, e]): Bool = match (r1, r2) {
        case (Ok(v1), Ok(v2)) => v1 == v2
        case (Err(w1), Err(w2)) => w1 == w2
        case _ => false
    }

    ///
    /// Returns the result of a three-way comparison between `r1` and `r2`.
    ///
    /// Returns n < 0 if `r1` is smaller than `r2`.
    /// Returns n = 0 if `r1` is equal to `r2`.
    /// Returns n > 0 if `r1` is greater than `r2`.
    ///
    def __cmp(r1: Result[t, e], r2: Result[t, e]): Int32 = match (r1, r2) {
        case (Ok(v1), Ok(v2))   =>  v1 <=> v2
        case (Err(v1), Err(v2)) =>  v1 <=> v2
        case (Ok(_), Err(_))    =>  -1
        case (Err(_), Ok(_))    =>  +1
    }


    /////////////////////////////////////////////////////////////////////////////
    // Lints: Simplifications                                                  //
    /////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////
    // Lints: Use Cases                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @lint
    def useExists(): Bool =
        forall [t, e] (f: t -> Bool, r: Result[t, e]).
            getWithDefault(map(f, r), false) === exists(f, r)

    @lint
    def useForall(): Bool =
        forall [t, e] (f: t -> Bool, r: Result[t, e]).
            getWithDefault(map(f, r), true) === forall(f, r)

}