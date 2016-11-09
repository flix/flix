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

namespace Option {

    ///
    /// Returns `true` iff `o` is `None`.
    ///
    def null[a](o: Option[a]): Bool = match o with {
        case None => true
        case Some(v) => false
    }

    ///
    /// Returns `v` if `o` is `Some(v)`. Undefined semantics if `o` is `None`.
    ///
    def get[a](o: Option[a]): a = match o with {
        case Some(v) => v
    }

    ///
    /// Returns `v` if `o` is `Some(v).` Otherwise returns `d`.
    ///
    def getWithDefault[a](o: Option[a], d: a): a = match o with {
        case None => d
        case Some(v) => v
    }

    ///
    /// Returns `true` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `false`.
    ///
    def exists[a](f: a -> Bool, o: Option[a]): Bool = getWithDefault(map(f, o), false)

    ///
    /// Returns `true` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true` or if `o` is `None`.
    /// Otherwise returns `false`.
    ///
    def forall[a](f: a -> Bool, o: Option[a]): Bool = getWithDefault(map(f, o), true)

    ///
    /// Returns `o` if `o` is `Some(v)` and the predicate `f(v)` is true. Otherwise returns `None`.
    ///
    def filter[a](f: a -> Bool, o: Option[a]): Option[a] = match o with {
        case None => None
        case Some(v) => if (f(v)) o else None
    }

    ///
    /// Returns `Some(f(v))` if `o` is `Some(v)`. Otherwise returns `None`.
    ///
    def map[a,b](f: a -> b, o: Option[a]): Option[b] = match o with {
        case None => None
        case Some(v) => Some(f(v))
    }

    ///
    /// Returns `Some(f(v1, v2))` if `o1` is `Some(v1)` and `o2` is `Some(v2)`. Otherwise returns `None`.
    ///
    def map2[a,b,c](f: (a, b) -> c, o1: Option[a], o2: Option[b]): Option[c] = match (o1, o2) with {
        case (None, _) => None
        case (_, None) => None
        case (Some(v1), Some(v2)) => Some(f(v1, v2))
    }

    ///
    /// Returns `f(v)` if `o` is `Some(v)`. Otherwise returns `None`.
    ///
    def flatMap[a,b](f: a -> Option[b], o: Option[a]): Option[b] = match o with {
        case None => None
        case Some(v) => f(v)
    }

    ///
    /// Returns `f(v1, v2)` if `o1` is `Some(v1)` and `o2` is `Some(v2)`. Otherwise returns `None`.
    ///
    def flatMap2[a,b,c](f: (a, b) -> Option[c], o1: Option[a], o2: Option[b]): Option[c] = match (o1, o2) with {
        case (None, _) => None
        case (_, None) => None
        case (Some(v1), Some(v2)) => f(v1, v2)
    }

    ///
    /// Returns a one-element list of the value `v` if o is `Some(v)`. Otherwise returns the empty list.
    ///
    def toList[a](o: Option[a]): List[a] = match o with {
        case None => Nil
        case Some(v) => v :: Nil
    }

    ///
    /// Returns a one-element set of the value `v` if `o` is `Some(v)`. Otherwise returns the empty set.
    ///
    def toSet[a](o: Option[a]): Set[a] = match o with {
        case None => #{}
        case Some(v) => #{v}
    }

    ///
    /// Returns `o1` if it is `Some(v)`. Otherwise returns `o2`.
    ///
    def withDefault[a](o1: Option[a], o2: Option[a]): Option[a] = match o1 with {
        case None => o2
        case Some(v) => o1
    }

    ///
    /// Returns `Ok(v)` if `o` is `Some(v)`. Otherwise returns `Err(z)`.
    ///
    def toOk[t, e](o: Option[t], z: e): Result[t, e] = match o with {
        case None => Err(z)
        case Some(v) => Ok(v)
    }

    ///
    /// Returns `Err(v)` if `o` is `Some(v)`. Otherwise returns `Ok(z)`.
    ///
    def toErr[t,e](o: Option[e], z: t): Result[t,e] = match o with {
        case None => Ok(z)
        case Some(v) => Err(v)
    }

    ///
    /// Returns `1` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `0`.
    ///
    def count[a](f: a -> Bool, o: Option[a]): Int32 = match o with {
        case None => 0
        case Some(v) => if (f(v)) 1 else 0
    }

    ///
    /// Returns `o` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `None`.
    ///
    def find[a](f: a -> Bool, o: Option[a]): Option[a] = match o with {
        case None => None
        case Some(v) => if (f(v)) o else None
    }

    ///
    /// Returns `v` if `o` is `Some(v)`. Otherwise returns `None`.
    ///
    def flatten[a](o: Option[Option[a]]): Option[a] = match o with {
        case None => None
        case Some(v) => v
    }

    ///
    /// Returns `f(z, v)` if `o` is `Some(v)`. Otherwise returns `z`.
    ///
    def foldLeft[a, b](f: (b, a) -> b, z: b, o: Option[a]): b = match o with {
        case None => z
        case Some(v) => f(z, v)
    }

    ///
    /// Returns `f(v, z)` if `o` is `Some(v)`. Otherwise returns `z`.
    ///
    def foldRight[a, b](f: (a, b) -> b, o: Option[a], z: b): b = match o with {
        case None => z
        case Some(v) => f(v, z)
    }

    ///
    /// Returns `true` iff `o` is `None`.
    ///
    def isNone[a](o: Option[a]): Bool = match o with {
        case None => true
        case Some(v) => false
    }

    ///
    /// Returns `true` iff `o` is `Some(v)`.
    ///
    def isSome[a](o: Option[a]): Bool = match o with {
        case None => false
        case Some(v) => true
    }

    ///
    /// Returns `Some((v1, v2))` if `o1` is `Some(v1)` and `o2` is `Some(v2)`. Otherwise returns `None`.
    ///
    def zip[a, b](o1: Option[a], o2: Option[b]): Option[(a, b)] = match (o1, o2) with {
        case (None, _) => None
        case (_, None) => None
        case (Some(v1), Some(v2)) => Some((v1, v2))
    }

    ///
    /// Returns `(Some(v1), Some(v2))` if `o` is `Some((v1, v2))`. Otherwise returns `(None, None)`.
    ///
    def unzip[a, b](o: Option[(a, b)]): (Option[a], Option[b]) = match o with {
        case None => (None, None)
        case Some((v1, v2)) => (Some(v1), Some(v2))
    }
}