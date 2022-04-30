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
pub enum Option[t] with Eq, Order, ToString {
    case None,
    case Some(t)
}

instance Hash[Option[a]] with Hash[a] {
    pub def hash(o: Option[a]): Int32 = match o {
        case Some(y) => 919 * Hash.hash(y)
        case None => 1046527
    }
}

instance Functor[Option] {
    pub def map(f: a -> b & ef, o: Option[a]): Option[b] & ef = Option.map(f, o)
}

instance Applicative[Option] {
    pub def point(x: a) : Option[a] = Option.point(x)

    pub def ap(f: Option[a -> b & e], x: Option[a]) : Option[b] & e = Option.ap(f, x)

    // Same as the default implementation using `map` and `ap` but with less indirection.
    override pub def liftA2(f: t1 -> t2 -> r & e, x1: Option[t1], x2: Option[t2]): Option[r] & e = Option.lift2(f, x1, x2)
    override pub def liftA3(f: t1 -> t2 -> t3 -> r & e, x1: Option[t1], x2: Option[t2], x3: Option[t3]): Option[r] & e = Option.lift3(f, x1, x2, x3)
    override pub def liftA4(f: t1 -> t2 -> t3 -> t4 -> r & e, x1: Option[t1], x2: Option[t2], x3: Option[t3], x4: Option[t4]): Option[r] & e = Option.lift4(f, x1, x2, x3, x4)
    override pub def liftA5(f: t1 -> t2 -> t3 -> t4 -> t5 -> r & e, x1: Option[t1], x2: Option[t2], x3: Option[t3], x4: Option[t4], x5: Option[t5]): Option[r] & e = Option.lift5(f, x1, x2, x3, x4, x5)
}

instance Monad[Option] {
    pub def flatMap(f: a -> Option[b] & ef, x : Option[a]) : Option[b] & ef = Option.flatMap(f, x)
}

instance Foldable[Option] {
    pub def foldLeft(f: (b, a) -> b & ef, s: b, o: Option[a]): b & ef = Option.foldLeft(f, s, o)
    pub def foldRight(f: (a, Unit -> b & ef) -> b & ef, s: b, o: Option[a]): b & ef = Option.foldRight(f, o, s)
}

instance Traversable[Option] {
    pub def traverse(f: a -> m[b] & ef, t: Option[a]): m[Option[b]] & ef with Applicative[m] =
        use Applicative.{point};
        use Functor.{map};
        match t {
            case None => point(None)
            case Some(a) => f(a) |> map(Some)
        }

    pub override def sequence(t: Option[m[a]]): m[Option[a]] with Applicative[m] =
        use Applicative.{point};
        use Functor.{map};
        match t {
            case Some(ma) => map(Some, ma)
            case None => point(None)
        }

}

instance SemiGroup[Option[a]] with SemiGroup[a] {
    pub def combine(x: Option[a], y: Option[a]): Option[a] = match (x, y) {
        case (a, None)              => a
        case (None, b)              => b
        case (Some(x1), Some(y1))   => Some(SemiGroup.combine(x1, y1))
    }
}

instance CommutativeSemiGroup[Option[a]] with CommutativeSemiGroup[a]


instance Monoid[Option[a]] with Monoid[a] {
    pub def empty(): Option[a] = None
}

instance CommutativeMonoid[Option[a]] with CommutativeMonoid[a]

instance Iterable[Option] {
    pub def iterator(t: Option[a]): Iterator[a] & Impure =
        Option.toIterator(t)
}

namespace Option {

    ///
    /// Returns `true` iff `o` is `None`.
    ///
    @Time(1) @Space(1)
    pub def isEmpty(o: Option[a]): Bool = match o {
        case None    => true
        case Some(_) => false
    }

    ///
    /// Returns `v` if `o` is `Some(v).` Otherwise returns `d`.
    ///
    @Time(1) @Space(1)
    pub def getWithDefault(d: a, o: Option[a]): a = match o {
        case None    => d
        case Some(v) => v
    }

    ///
    /// Returns `o` if it is `Some(v)`. Otherwise returns `default`.
    ///
    @Time(1) @Space(1)
    pub def withDefault(default: {default :: Option[a]}, o: Option[a]): Option[a] = match o {
        case None    => default.default
        case Some(_) => o
    }

    ///
    /// Returns `Some(to)` if `o` is `Some(from)`. Otherwise returns `o`.
    ///
    @Time(1) @Space(1)
    pub def replace(from: {from :: a}, to: {to :: a}, o: Option[a]): Option[a] with Eq[a] = match o {
        case None    => o
        case Some(v) => Some(if (v == from.from) to.to else v)
    }

    ///
    /// Returns `true` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `false`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f)) @Space(space(f))
    pub def exists(f: a -> Bool, o: Option[a]): Bool = match o {
        case None    => false
        case Some(v) => f(v)
    }

    ///
    /// Returns `true` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true` or if `o` is `None`.
    ///
    /// Otherwise returns `false`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f)) @Space(space(f))
    pub def forall(f: a -> Bool, o: Option[a]): Bool = match o {
        case None    => true
        case Some(v) => f(v)
    }

    ///
    /// Returns `o` if `o` is `Some(v)` and the predicate `f(v)` is true. Otherwise returns `None`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f)) @Space(space(f))
    pub def filter(f: a -> Bool, o: Option[a]): Option[a] = match o {
        case None    => None
        case Some(v) => if (f(v)) o else None
    }

    ///
    /// Returns `Some(f(v))` if `o` is `Some(v)`. Otherwise returns `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def map(f: a -> b & ef, o: Option[a]): Option[b] & ef = match o {
        case None    => None
        case Some(v) => Some(f(v))
    }

    ///
    /// Returns `Some(x)`.
    ///
    pub def point(x: a) : Option[a] = Some(x)

    ///
    /// If both arguments are `Some`, return a `Some` containing the result of applying the function inside
    /// `f` to the value inside `x`. Otherwise return `None`.
    ///
    pub def ap(f: Option[a -> b & e], x: Option[a]) : Option[b] & e =
        match f {
            case None    => None
            case Some(g) => match x {
                case None    => None
                case Some(y) => Some(g(y))
            }
        }

    ///
    /// Returns `f(v)` if `o` is `Some(v)`. Otherwise returns `None`.
    ///
    pub def flatMap(f: a -> Option[b] & ef, o: Option[a]): Option[b] & ef = match o {
        case None    => None
        case Some(v) => f(v)
    }

    ///
    /// Returns `1` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `0`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f)) @Space(space(f))
    pub def count(f: a -> Bool, o: Option[a]): Int32 = match o {
        case None    => 0
        case Some(v) => if (f(v)) 1 else 0
    }

    ///
    /// Returns `v` if `o` is `Some(v)` else `0`.
    ///
    pub def sum(o: Option[Int32]): Int32 =
        Foldable.sum(o)

    ///
    /// Returns `f(v)` if `o` is `Some(v)` else `0`.
    ///
    pub def sumWith(f: a -> Int32 & ef, o: Option[a]): Int32 & ef =
        Foldable.sumWith(f, o)

    ///
    /// Returns `v` if `o` is `Some(v)` else `0`.
    ///
    pub def product(o: Option[Int32]): Int32 =
        Foldable.product(o)

    ///
    /// Returns `f(v)` if `o` is `Some(v)` else `0`.
    ///
    pub def productWith(f: a -> Int32 & ef, o: Option[a]): Int32 & ef =
        Foldable.productWith(f, o)

    ///
    /// Returns `o` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `None`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f)) @Space(space(f))
    pub def find(f: a -> Bool, o: Option[a]): Option[a] = match o {
        case None    => None
        case Some(v) => if (f(v)) o else None
    }

    ///
    /// Returns `v` if `o` is `Some(v)`. Otherwise returns `None`.
    ///
    @Time(1) @Space(1)
    pub def flatten(o: Option[Option[a]]): Option[a] = match o {
        case None    => None
        case Some(v) => v
    }

    ///
    /// Returns the result of applying `combine` to all the elements in `o`, using `empty` as the initial value.
    ///
    pub def fold(o: Option[a]): a with Monoid[a] = Foldable.fold(o)

    ///
    /// Returns `f(z, v)` if `o` is `Some(v)`. Otherwise returns `z`.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, z: b, o: Option[a]): b & ef = match o {
        case None    => z
        case Some(v) => f(z, v)
    }

    ///
    /// Returns `f(v, z)` if `o` is `Some(v)`. Otherwise returns `z`.
    ///
    /// A `foldRight` allows early termination by not calling the continuation.
    ///
    pub def foldRight(f: (a, Unit -> b & ef) -> b & ef, o: Option[a], z: b): b & ef = match o {
        case None    => z
        case Some(v) => f(v, _ -> z as & ef)
    }

    ///
    /// Returns `Some(v1 :: v2 :: ... :: vn)` if each of `xs_i` is `Some(v_i)`. Otherwise returns `None`.
    ///
    @Time(List.length(xs)) @Space(List.length(xs))
    pub def sequence(xs: List[Option[a]]): Option[List[a]] =
        def loop(ll, sk) = match ll {
            case Nil            => sk(Nil)
            case None :: _      => None
            case Some(y) :: rs  => loop(rs, ys -> sk(y :: ys))
        };
        loop(xs, ys -> Some(ys))

    ///
    /// Returns `Some(v1 :: v2 :: ... v :: vn)` if each of `f(l_i)` is `Some(v_i)`. Otherwise returns `None`.
    ///
    pub def traverse(f: a -> Option[b] & ef, l: List[a]): Option[List[b]] & ef =
        def loop(ll, sk) = match ll {
            case Nil     => sk(Nil)
            case x :: xs => match f(x) {
                case None    => None
                case Some(y) => loop(xs, ys -> sk(y :: ys))
            }
        };
        loop(l, ys -> Some(ys))

    ///
    /// Returns `Some()` if each of `f(l_i)` is `Some(_)`. Otherwise returns `None`.
    ///
    /// This function is the "forgetful" version of `traverse`, use it when the you want the effect
    /// of applying `f` to each element but do not care about collecting the results.
    ///
    pub def traverseX(f: a -> Option[b] & ef, l: List[a]): Option[Unit] & ef =
        def loop(ll) = match ll {
            case Nil     => Some()
            case x :: xs => match f(x) {
                case None    => None
                case Some(_) => loop(xs)
            }
        };
        loop(l)

    ///
    /// Returns the result of applying `f` to a start value `s` and the elements in `l`
    /// going from left to right.
    ///
    /// If at any step applying `f` fails (i.e. it produces a `None` value) the traversal
    /// of `l` is short-circuited and `None` is returned.
    ///
    /// If `f` is successfully applied to all the elements in `l` the result is of the form:
    /// `Some(f(...f(f(s, x1), x2)..., xn))`.
    ///
    pub def foldLeftM(f: (b, a) -> Option[b] & ef, s: b, l: List[a]): Option[b] & ef =
        def loop(ll, acc) = match ll {
            case Nil     => Some(acc)
            case x :: xs => match f(acc, x) {
                case Some(s1) => loop(xs, s1)
                case None     => None
            }
        };
        loop(l, s)

    ///
    /// Returns the result of applying `f` to a start value `s` and the elements in `l`
    /// going from right to left.
    ///
    /// If at any step applying `f` fails (i.e. it produces a `None` value) the traversal
    /// of `l` is short-circuited and `None` is returned.
    ///
    /// If `f` is successfully applied to al elements in `l` the result is of the form:
    /// `Some(f(x1, ...f(xn-1, f(xn, s))...))`.
    ///
    pub def foldRightM(f: (a, b) -> Option[b] & ef, s: b, l: List[a]): Option[b] & ef =
        def loop(ll, k) = match ll {
            case Nil     => k(s)
            case x :: xs => loop(xs, s1 -> match f(x, s1) {
                case Some(s2) => k(s2)
                case None     => None
            })
        };
        loop(l, s1 -> Some(s1) as & ef)

    ///
    /// Returns a one-element list of the value `v` if `o` is `Some(v)`. Otherwise returns the empty list.
    ///
    @Time(1) @Space(1)
    pub def toList(o: Option[a]): List[a] = match o {
        case None    => Nil
        case Some(v) => v :: Nil
    }

    ///
    /// Returns a one-element set of the value `v` if `o` is `Some(v)`. Otherwise returns the empty set.
    ///
    @Time(1) @Space(1)
    pub def toSet(o: Option[a]): Set[a] with Order[a] = match o {
        case None    => Set.empty()
        case Some(v) => Set.singleton(v)
    }

    ///
    /// Returns a singleton map with the mapping `k -> v` if `o` is `Some((k, v))`. Otherwise returns the empty map.
    ///
    @Time(1) @Space(1)
    pub def toMap(o: Option[(k, v)]): Map[k, v] with Order[k] = match o {
        case None         => Map.empty()
        case Some((k, v)) => Map.singleton(k, v)
    }

    ///
    /// Returns a map with elements of `s` as keys and `f` applied as values.
    ///
    pub def toMapWith(f: a -> b, s: Option[a]): Map[a, b] with Order[a] =
        Option.foldRight((x, k) -> Map.insert(x, f(x), k()), s, Map.empty())

    ///
    /// Returns the Option value `Ok(v)` if `o` is `Some(v)`. Otherwise returns `Err(e)`.
    ///
    @Time(1) @Space(1)
    pub def toOk(o: Option[t], e: e): Result[t, e] = match o {
        case None    => Err(e)
        case Some(a) => Ok(a)
    }

    ///
    /// Returns the Option value `Err(e)` if `o` is `Some(e)`. Otherwise returns `Ok(d)`.
    ///
    @Time(1) @Space(1)
    pub def toErr(o: Option[e], d: t): Result[t, e] = match o {
        case None    => Ok(d)
        case Some(e) => Err(e)
    }

    ///
    /// Returns the Validation value `Success(v)` if `o` is `Some(v)`. Otherwise lifts `e` into Validation's `Failure`.
    ///
    @Time(1) @Space(1)
    pub def toSuccess(o: Option[t], e: e): Validation[t, e] = match o {
        case None    => Failure(Nec.singleton(e))
        case Some(a) => Success(a)
    }

    ///
    /// Returns `e` into Validation's `Failure` if `o` is `Some(e)`. Otherwise returns `Success(d)`.
    ///
    @Time(1) @Space(1)
    pub def toFailure(o: Option[e], d: t): Validation[t, e] = match o {
        case None    => Success(d)
        case Some(e) => Failure(Nec.singleton(e))
    }

    ///
    /// Returns `Some((v1, v2))` if `o1` is `Some(v1)` and `o2` is `Some(v2)`. Otherwise returns `None`.
    ///
    @Time(1) @Space(1)
    pub def zip(o1: Option[a], o2: Option[b]): Option[(a, b)] = match (o1, o2) {
        case (None, _)            => None
        case (_, None)            => None
        case (Some(v1), Some(v2)) => Some((v1, v2))
    }

    ///
    /// Returns `(Some(v1), Some(v2))` if `o` is `Some((v1, v2))`. Otherwise returns `(None, None)`.
    ///
    @Time(1) @Space(1)
    pub def unzip(o: Option[(a, b)]): (Option[a], Option[b]) = match o {
        case None           => (None, None)
        case Some((v1, v2)) => (Some(v1), Some(v2))
    }

    ///
    /// Applies `f` to `v` if `o` is `Some(v)`. Otherwise does nothing.
    ///
    @Time(time(f)) @Space(space(f))
    pub def foreach(f: a -> Unit & ef, o: Option[a]): Unit & ef = match o {
        case None    => ()
        case Some(v) => f(v)
    }

    ///
    /// Applies the binary function `f` to the values in `o1` and `o2`.
    ///
    /// Returns `None` if either `o1` or `o2` are `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift2(f: (t1, t2) -> u & ef, o1: Option[t1], o2: Option[t2]): Option[u] & ef =
        ap(map(f, o1), o2)

    ///
    /// Applies the ternary function `f` to the values in `o1`, `o2` and `o3`.
    ///
    /// Returns `None` if any of `o1`, `o2` and `o3` are `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift3(f: (t1, t2, t3) -> u & ef, o1: Option[t1], o2: Option[t2], o3: Option[t3]): Option[u] & ef =
        ap(lift2(f, o1, o2), o3)

    ///
    /// Applies the 4-ary function `f` to the values in `o1`, `o2`, `o3` and `o4`.
    ///
    /// Returns `None` if any of `o1`, `o2`, `o3` and `o4` are `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift4(f: (t1, t2, t3, t4) -> u & ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4]): Option[u] & ef =
        ap(lift3(f, o1, o2, o3), o4)

    ///
    /// Applies the 5-ary function `f` to the values in `o1`, `o2`, ... `o5`.
    ///
    /// Returns `None` if any of `o1`, `o2`, ... `o5` are `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift5(f: (t1, t2, t3, t4, t5) -> u & ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4], o5: Option[t5]): Option[u] & ef =
        ap(lift4(f, o1, o2, o3, o4), o5)

    ///
    /// Applies the 6-ary function `f` to the values in `o1`, `o2`, ... `o6`.
    ///
    /// Returns `None` if any of `o1`, `o2`, ... `o6` are `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift6(f: (t1, t2, t3, t4, t5, t6) -> u & ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4], o5: Option[t5], o6: Option[t6]): Option[u] & ef =
        ap(lift5(f, o1, o2, o3, o4, o5), o6)

    ///
    /// Applies the 7-ary function `f` to the values in `o1`, `o2`, ... `o7`.
    ///
    /// Returns `None` if any of `o1`, `o2`, ... `o7` are `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift7(f: (t1, t2, t3, t4, t5, t6, t7) -> u & ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4], o5: Option[t5], o6: Option[t6], o7: Option[t7]): Option[u] & ef=
        ap(lift6(f, o1, o2, o3, o4, o5, o6), o7)

    ///
    /// Applies the 8-ary function `f` to the values in `o1`, `o2`, ... `o8`.
    ///
    /// Returns `None` if any of `o1`, `o2`, ... `o8` are `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift8(f: (t1, t2, t3, t4, t5, t6, t7, t8) -> u & ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4], o5: Option[t5], o6: Option[t6], o7: Option[t7], o8: Option[t8]): Option[u] & ef =
        ap(lift7(f, o1, o2, o3, o4, o5, o6, o7), o8)

    ///
    /// Applies the 9-ary function `f` to the values in `o1`, `o2`, ... `o9`.
    ///
    /// Returns `None` if any of `o1`, `o2`, ... `o9` are `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift9(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9) -> u & ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4], o5: Option[t5], o6: Option[t6], o7: Option[t7], o8: Option[t8], o9: Option[t9]): Option[u] & ef =
        ap(lift8(f, o1, o2, o3, o4, o5, o6, o7, o8), o9)

    ///
    /// Applies the 10-ary function `f` to the values in `o1`, `o2`, ... `o10`.
    ///
    /// Returns `None` if any of `o1`, `o2`, ... `o10` are `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift10(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) -> u & ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4], o5: Option[t5], o6: Option[t6], o7: Option[t7], o8: Option[t8], o9: Option[t9], o10: Option[t10]): Option[u] & ef =
        ap(lift9(f, o1, o2, o3, o4, o5, o6, o7, o8, o9), o10)

    ///
    /// Returns an iterator over `o` with 1 element or an empty iterator if `o` is `None`.
    ///
    pub def toIterator(o: Option[a]): Iterator[a] & Impure = match o {
        case None    => Iterator.empty()
        case Some(x) => Iterator.singleton(x)
    }

}
