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
/// The Option type.
///
/// An option represents an optional value.
/// The constructor `None` represents an absent value,
/// whereas the constructor `Some(v)` represents the value `v`.
///
enum Option[t] {
    case None,
    case Some(t)
}

namespace Option {

    ///
    /// Returns `true` iff `o` is `None`.
    ///
    pub def isEmpty[a](o: Option[a]): Bool = match o with {
        case None => true
        case Some(_) => false
    }

    ///
    /// Returns `true` iff `o` is `None`.
    ///
    pub def isNone[a](o: Option[a]): Bool = match o with {
        case None => true
        case Some(_) => false
    }

    ///
    /// Returns `true` iff `o` is `Some(v)`.
    ///
    pub def isSome[a](o: Option[a]): Bool = match o with {
        case None => false
        case Some(_) => true
    }

    ///
    /// Returns `v` if `o` is `Some(v).` Otherwise returns `d`.
    ///
    pub def getWithDefault[a](o: Option[a], d: a): a = match o with {
        case None => d
        case Some(v) => v
    }

    ///
    /// Returns `o1` if it is `Some(v)`. Otherwise returns `o2`.
    ///
    pub def withDefault[a](o1: Option[a], o2: Option[a]): Option[a] = match o1 with {
        case None => o2
        case Some(_) => o1
    }

    ///
    /// Returns `Some(y)` if `o` is `Some(x)`. Otherwise returns `o`.
    ///
    pub def replace[a](x: a, y: a, o: Option[a]): Option[a] = match o with {
        case None => o
        case Some(v) => Some(if (v == x) y else v)
    }

    ///
    /// Returns `true` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `false`.
    ///
    pub def exists[a](f: a -> Bool, o: Option[a]): Bool = getWithDefault(map(f, o), false)

    ///
    /// Returns `true` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true` or if `o` is `None`.
    /// Otherwise returns `false`.
    ///
    pub def forall[a](f: a -> Bool, o: Option[a]): Bool = getWithDefault(map(f, o), true)

    ///
    /// Returns `o` if `o` is `Some(v)` and the predicate `f(v)` is true. Otherwise returns `None`.
    ///
    pub def filter[a](f: a -> Bool, o: Option[a]): Option[a] = match o with {
        case None => None
        case Some(v) => if (f(v)) o else None
    }

    ///
    /// Returns `Some(f(v))` if `o` is `Some(v)`. Otherwise returns `None`.
    ///
    pub def map[a, b](f: a -> b, o: Option[a]): Option[b] = match o with {
        case None => None
        case Some(v) => Some(f(v))
    }

    ///
    /// Returns `Some(f(v1, v2))` if `o1` is `Some(v1)` and `o2` is `Some(v2)`. Otherwise returns `None`.
    ///
    pub def map2[a, b, c](f: (a, b) -> c, o1: Option[a], o2: Option[b]): Option[c] = match (o1, o2) with {
        case (None, _) => None
        case (_, None) => None
        case (Some(v1), Some(v2)) => Some(f(v1, v2))
    }

    ///
    /// Returns `f(v)` if `o` is `Some(v)`. Otherwise returns `None`.
    ///
    pub def flatMap[a, b](f: a -> Option[b], o: Option[a]): Option[b] = match o with {
        case None => None
        case Some(v) => f(v)
    }

    ///
    /// Returns `f(v1, v2)` if `o1` is `Some(v1)` and `o2` is `Some(v2)`. Otherwise returns `None`.
    ///
    pub def flatMap2[a, b, c](f: (a, b) -> Option[c], o1: Option[a], o2: Option[b]): Option[c] = match (o1, o2) with {
        case (None, _) => None
        case (_, None) => None
        case (Some(v1), Some(v2)) => f(v1, v2)
    }

    ///
    /// Returns `1` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `0`.
    ///
    pub def count[a](f: a -> Bool, o: Option[a]): Int32 = match o with {
        case None => 0
        case Some(v) => if (f(v)) 1 else 0
    }

    ///
    /// Returns `o` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `None`.
    ///
    pub def find[a](f: a -> Bool, o: Option[a]): Option[a] = match o with {
        case None => None
        case Some(v) => if (f(v)) o else None
    }

    ///
    /// Returns `v` if `o` is `Some(v)`. Otherwise returns `None`.
    ///
    pub def flatten[a](o: Option[Option[a]]): Option[a] = match o with {
        case None => None
        case Some(v) => v
    }

    ///
    /// Returns `f(z, v)` if `o` is `Some(v)`. Otherwise returns `z`.
    ///
    pub def foldLeft[a, b](f: (b, a) -> b, z: b, o: Option[a]): b = match o with {
        case None => z
        case Some(v) => f(z, v)
    }

    ///
    /// Returns `f(v, z)` if `o` is `Some(v)`. Otherwise returns `z`.
    ///
    pub def foldRight[a, b](f: (a, b) -> b, o: Option[a], z: b): b = match o with {
        case None => z
        case Some(v) => f(v, z)
    }

    ///
    /// Returns a one-element list of the value `v` if `o` is `Some(v)`. Otherwise returns the empty list.
    ///
    pub def toList[a](o: Option[a]): List[a] = match o with {
        case None => Nil
        case Some(v) => v :: Nil
    }

    ///
    /// Returns a one-element set of the value `v` if `o` is `Some(v)`. Otherwise returns the empty set.
    ///
    pub def toSet[a](o: Option[a]): Set[a] = match o with {
        case None => #{}
        case Some(v) => #{v}
    }

    ///
    /// Returns a singleton map with the mapping `k -> v` if `o` is `Some((k, v))`. Otherwise returns the empty map.
    ///
    pub def toMap[k,v](o: Option[(k, v)]): Map[k, v] = match o with {
        case None => @{}
        case Some((k, v)) => Map.singleton(k, v)
    }

    ///
    /// Returns `Some((v1, v2))` if `o1` is `Some(v1)` and `o2` is `Some(v2)`. Otherwise returns `None`.
    ///
    pub def zip[a, b](o1: Option[a], o2: Option[b]): Option[(a, b)] = match (o1, o2) with {
        case (None, _) => None
        case (_, None) => None
        case (Some(v1), Some(v2)) => Some((v1, v2))
    }

    ///
    /// Returns `(Some(v1), Some(v2))` if `o` is `Some((v1, v2))`. Otherwise returns `(None, None)`.
    ///
    pub def unzip[a, b](o: Option[(a, b)]): (Option[a], Option[b]) = match o with {
        case None => (None, None)
        case Some((v1, v2)) => (Some(v1), Some(v2))
    }



    ///
    /// Returns `true` if and only if `o1` and `o2` and equal.
    ///
    pub def eq[a](o1: Option[a], o2: Option[a]): Bool = match (o1, o2) with {
        case (None, None) => true
        case (Some(v1), Some(v2)) => v1 == v2
        case _ => false
    }

}
