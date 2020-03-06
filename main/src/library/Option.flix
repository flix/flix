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
    pub def isEmpty(o: Option[a]): Bool = match o {
        case None => true
        case Some(_) => false
    }

    ///
    /// Returns `v` if `o` is `Some(v).` Otherwise returns `d`.
    ///
    pub def getWithDefault(o: Option[a], d: a): a = match o {
        case None => d
        case Some(v) => v
    }

    ///
    /// Returns `o1` if it is `Some(v)`. Otherwise returns `o2`.
    ///
    pub def withDefault(o1: Option[a], o2: Option[a]): Option[a] = match o1 {
        case None => o2
        case Some(_) => o1
    }

    ///
    /// Returns `Some(y)` if `o` is `Some(x)`. Otherwise returns `o`.
    ///
    pub def replace(x: a, y: a, o: Option[a]): Option[a] = match o {
        case None => o
        case Some(v) => Some(if (v == x) y else v)
    }

    ///
    /// Returns `true` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `false`.
    ///
    /// The function `f` must be pure.
    ///
    pub def exists(f: a -> Bool, o: Option[a]): Bool = getWithDefault(map(f, o), false)

    ///
    /// Returns `true` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true` or if `o` is `None`.
    ///
    /// Otherwise returns `false`.
    ///
    /// The function `f` must be pure.
    ///
    pub def forall(f: a -> Bool, o: Option[a]): Bool = getWithDefault(map(f, o), true)

    ///
    /// Returns `o` if `o` is `Some(v)` and the predicate `f(v)` is true. Otherwise returns `None`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filter(f: a -> Bool, o: Option[a]): Option[a] = match o {
        case None => None
        case Some(v) => if (f(v)) o else None
    }

    ///
    /// Returns `Some(f(v))` if `o` is `Some(v)`. Otherwise returns `None`.
    ///
    pub def map(f: a -> b & e, o: Option[a]): Option[b] & e = match o {
        case None => None
        case Some(v) => Some(f(v))
    }

    ///
    /// Returns `Some(f(v1, v2))` if `o1` is `Some(v1)` and `o2` is `Some(v2)`. Otherwise returns `None`.
    ///
    pub def map2(f: (a, b) -> c & e, o1: Option[a], o2: Option[b]): Option[c] & e = match (o1, o2) {
        case (None, _) => None
        case (_, None) => None
        case (Some(v1), Some(v2)) => Some(f(v1, v2))
    }

    ///
    /// Returns `f(v)` if `o` is `Some(v)`. Otherwise returns `None`.
    ///
    pub def flatMap(f: a -> Option[b] & e, o: Option[a]): Option[b] & e = match o {
        case None => None
        case Some(v) => f(v)
    }

    ///
    /// Returns `f(v1, v2)` if `o1` is `Some(v1)` and `o2` is `Some(v2)`. Otherwise returns `None`.
    ///
    pub def flatMap2(f: (a, b) -> Option[c] & e, o1: Option[a], o2: Option[b]): Option[c] & e = match (o1, o2) {
        case (None, _) => None
        case (_, None) => None
        case (Some(v1), Some(v2)) => f(v1, v2)
    }

    ///
    /// Returns `1` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `0`.
    ///
    /// The function `f` must be pure.
    ///
    pub def count(f: a -> Bool, o: Option[a]): Int32 = match o {
        case None => 0
        case Some(v) => if (f(v)) 1 else 0
    }

    ///
    /// Returns `o` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `None`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: a -> Bool, o: Option[a]): Option[a] = match o {
        case None => None
        case Some(v) => if (f(v)) o else None
    }

    ///
    /// Returns `v` if `o` is `Some(v)`. Otherwise returns `None`.
    ///
    pub def flatten(o: Option[Option[a]]): Option[a] = match o {
        case None => None
        case Some(v) => v
    }

    ///
    /// Returns `f(z, v)` if `o` is `Some(v)`. Otherwise returns `z`.
    ///
    pub def foldLeft(f: (b, a) -> b & e, z: b, o: Option[a]): b & e = match o {
        case None => z
        case Some(v) => f(z, v)
    }

    ///
    /// Returns `f(v, z)` if `o` is `Some(v)`. Otherwise returns `z`.
    ///
    pub def foldRight(f: (a, b) -> b & e, o: Option[a], z: b): b & e = match o {
        case None => z
        case Some(v) => f(v, z)
    }

    ///
    /// Returns `Some(v1 :: v2 :: ... :: vn)` if each of `xs_i` is `Some(v_i)`. Otherwise returns `None`.
    ///
    pub def sequence(xs: List[Option[a]]): Option[List[a]] = match xs {
        case Nil            => Some(Nil)
        case None :: _      => None
        case Some(y) :: ys  => map(zs -> y :: zs, sequence(ys))
    }

    ///
    /// Returns `Some(v1 :: v2 :: ... v :: vn)` if each of `f(xs_i)` is `Some(v_i)`. Otherwise returns `None`.
    ///
    pub def traverse(f: a -> Option[b] & e, xs: List[a]): Option[List[b]] & e = match xs {
        case Nil        => Some(Nil)
        case y :: ys    =>
            let* z = f(y);
            let* zs = traverse(f, ys);
            Some(z :: zs)
    }

    ///
    /// Returns a one-element list of the value `v` if `o` is `Some(v)`. Otherwise returns the empty list.
    ///
    pub def toList(o: Option[a]): List[a] = match o {
        case None => Nil
        case Some(v) => v :: Nil
    }

    ///
    /// Returns a one-element set of the value `v` if `o` is `Some(v)`. Otherwise returns the empty set.
    ///
    pub def toSet(o: Option[a]): Set[a] = match o {
        case None       => Set.empty()
        case Some(v)    => Set.singleton(v)
    }

    ///
    /// Returns a singleton map with the mapping `k -> v` if `o` is `Some((k, v))`. Otherwise returns the empty map.
    ///
    pub def toMap(o: Option[(k, v)]): Map[k, v] = match o {
        case None           => Map.empty()
        case Some((k, v))   => Map.singleton(k, v)
    }

    ///
    /// Returns `Some((v1, v2))` if `o1` is `Some(v1)` and `o2` is `Some(v2)`. Otherwise returns `None`.
    ///
    pub def zip(o1: Option[a], o2: Option[b]): Option[(a, b)] = match (o1, o2) {
        case (None, _) => None
        case (_, None) => None
        case (Some(v1), Some(v2)) => Some((v1, v2))
    }

    ///
    /// Returns `(Some(v1), Some(v2))` if `o` is `Some((v1, v2))`. Otherwise returns `(None, None)`.
    ///
    pub def unzip(o: Option[(a, b)]): (Option[a], Option[b]) = match o {
        case None => (None, None)
        case Some((v1, v2)) => (Some(v1), Some(v2))
    }

    ///
    /// Applies `f` to `v` if `o` is `Some(v)`. Otherwise does nothing.
    ///
    /// The function `f` must be impure.
    ///
    pub def foreach(f: a ~> Unit, o: Option[a]): Unit & Impure = match o {
        case None    => ()
        case Some(v) => f(v)
    }

    ///
    /// Returns `true` if and only if `o1` and `o2` and equal.
    ///
    def __eq(o1: Option[a], o2: Option[a]): Bool = match (o1, o2) {
        case (None, None)           => true
        case (Some(v1), Some(v2))   => v1 == v2
        case _ => false
    }

    ///
    /// Returns the result of a three-way comparison between `o1` and `o2`.
    ///
    /// Returns n < 0 if `o1` is smaller than `o2`.
    /// Returns n = 0 if `o1` is equal to `o2`.
    /// Returns n > 0 if `o1` is greater than `o2`.
    ///
    def __cmp(o1: Option[a], o2: Option[a]): Int32 = match (o1, o2) {
        case (None, None)           =>  0
        case (None, _)              => -1
        case (Some(v1), Some(v2))   => v1 <=> v2
        case _                      => +1
    }

}
