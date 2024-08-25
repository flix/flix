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
pub enum Option[t] with Eq, Order, ToString, Sendable {
    case None,
    case Some(t)
}

instance LowerBound[Option[t]] {
    pub def minValue(_: Unit): Option[t] = None
}

instance PartialOrder[Option[t]] with PartialOrder[t] {
    pub def lessEqual(x: Option[t], y: Option[t]): Bool = match (x,y) {
       case (None, _)           => true
       case (Some(a), Some(b))  => PartialOrder.lessEqual(a,b)
       case _                   => false
   }
}

instance JoinLattice[Option[t]] with JoinLattice[t] {
    pub def leastUpperBound(x: Option[t], y: Option[t]): Option[t] = match (x,y) {
       case (None, _)           => y
       case (_, None)           => x
       case (Some(a), Some(b))  => Some(JoinLattice.leastUpperBound(a,b))
   }
}

instance MeetLattice[Option[t]] with MeetLattice[t] {
    pub def greatestLowerBound(x: Option[t], y: Option[t]): Option[t] = match (x,y) {
       case (None, _)           => None
       case (_, None)           => None
       case (Some(a), Some(b))  => Some(MeetLattice.greatestLowerBound(a, b))
   }
}

instance Hash[Option[a]] with Hash[a] {
    pub def hash(o: Option[a]): Int32 = match o {
        case None    => 1046527
        case Some(y) => 919 * Hash.hash(y)
    }
}

instance Functor[Option] {
    pub def map(f: a -> b \ ef, o: Option[a]): Option[b] \ ef = Option.map(f, o)
}

instance Applicative[Option] {
    pub def point(x: a) : Option[a] = Option.point(x)

    pub def ap(f: Option[a -> b \ ef], x: Option[a]) : Option[b] \ ef = Option.ap(f, x)

    // Same as the default implementation using `map` and `ap` but with less indirection.
    pub redef map2(f: t1 -> t2 -> r \ ef, x1: Option[t1], x2: Option[t2]): Option[r] \ ef = Option.map2(f, x1, x2)
    pub redef map3(f: t1 -> t2 -> t3 -> r \ ef, x1: Option[t1], x2: Option[t2], x3: Option[t3]): Option[r] \ ef = Option.map3(f, x1, x2, x3)
    pub redef map4(f: t1 -> t2 -> t3 -> t4 -> r \ ef, x1: Option[t1], x2: Option[t2], x3: Option[t3], x4: Option[t4]): Option[r] \ ef = Option.map4(f, x1, x2, x3, x4)
    pub redef map5(f: t1 -> t2 -> t3 -> t4 -> t5 -> r \ ef, x1: Option[t1], x2: Option[t2], x3: Option[t3], x4: Option[t4], x5: Option[t5]): Option[r] \ ef = Option.map5(f, x1, x2, x3, x4, x5)
}

instance Monad[Option] {
    pub def flatMap(f: a -> Option[b] \ ef, x : Option[a]) : Option[b] \ ef = Option.flatMap(f, x)
}

instance MonadZero[Option] {
    pub def empty(): Option[a] = None
}

instance MonadZip[Option] {
    pub def zipWith(f: (a, b) -> c \ ef, ma: Option[a], mb: Option[b]): Option[c] \ ef = Applicative.map2(f, ma, mb)
    pub override def zip(ma: Option[a], mb: Option[b]): Option[(a, b)] = Applicative.map2((x, y) -> (x, y), ma, mb)

    pub def zipWithA(f: (a, b) -> f[c] \ ef, x: Option[a], y: Option[b]): f[Option[c]] \ ef with Applicative[f] =
        use Functor.{<$>};
        match (x, y) {
            case (Some(x1), Some(y1)) => Some <$> f(x1, y1)
            case (_, _)               => Applicative.point(None)
        }
}

instance Foldable[Option] {
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, o: Option[a]): b \ ef = Option.foldLeft(f, s, o)
    pub def foldRight(f: (a, b) -> b \ ef, s: b, o: Option[a]): b \ ef = Option.foldRight(f, o, s)
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, s: b, o: Option[a]): b \ ef = Option.foldRightWithCont(f, o, s)
}

instance UnorderedFoldable[Option] {
    pub def foldMap(f: a -> b \ ef, o: Option[a]): b \ ef with CommutativeMonoid[b] = Option.foldMap(f, o)
}

instance Traversable[Option] {
    pub def traverse(f: a -> m[b] \ ef, t: Option[a]): m[Option[b]] \ ef with Applicative[m] =
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

instance Filterable[Option] {
    pub def filterMap(f: a -> Option[b] \ ef, o: Option[a]): Option[b] \ ef = Option.flatMap(f, o)
    pub override def filter(f: a -> Bool \ ef, o: Option[a]): Option[a] \ ef = Option.filter(f, o)
}

instance Witherable[Option]

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

instance Iterable[Option[a]] {
    type Elm = a
    pub def iterator(rc: Region[r], o: Option[a]): Iterator[a, r, r] \ r = Option.iterator(rc, o)
}

mod Option {

    ///
    /// Returns the empty option variant `None`.
    ///
    pub def empty(): Option[a] = None

    ///
    /// Returns `true` iff `o` is `None`.
    ///
    pub def isEmpty(o: Option[a]): Bool = match o {
        case None    => true
        case Some(_) => false
    }

    ///
    /// Returns `true` iff `o` is `Some`.
    ///
    pub def nonEmpty(o: Option[a]): Bool = not isEmpty(o)

    ///
    /// Returns `v` if `o` is `Some(v).` Otherwise returns `d`.
    ///
    pub def getWithDefault(d: a, o: Option[a]): a = match o {
        case None    => d
        case Some(v) => v
    }

    ///
    /// Returns `o` if it is `Some(v)`. Otherwise returns `default`.
    ///
    pub def withDefault(default: {default = Option[a]}, o: Option[a]): Option[a] = match o {
        case None    => default#default
        case Some(_) => o
    }

    ///
    /// Returns `Some(dst)` if `o` is `Some(src)`. Otherwise returns `o`.
    ///
    pub def replace(src: {src = a}, dst: {dst = a}, o: Option[a]): Option[a] with Eq[a] = match o {
        case None    => o
        case Some(v) => Some(if (v == src#src) dst#dst else v)
    }

    ///
    /// Returns `true` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `false`.
    ///
    pub def exists(f: a -> Bool \ ef, o: Option[a]): Bool \ ef = match o {
        case None    => false
        case Some(v) => f(v)
    }

    ///
    /// Returns `true` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true` or if `o` is `None`.
    ///
    /// Otherwise returns `false`.
    ///
    pub def forAll(f: a -> Bool \ ef, o: Option[a]): Bool \ ef = match o {
        case None    => true
        case Some(v) => f(v)
    }

    ///
    /// Returns `o` if `o` is `Some(v)` and the predicate `f(v)` is true. Otherwise returns `None`.
    ///
    pub def filter(f: a -> Bool \ ef, o: Option[a]): Option[a] \ ef = match o {
        case None    => None
        case Some(v) => if (f(v)) o else None
    }

    ///
    /// Returns `Some(f(v))` if `o` is `Some(v)`. Otherwise returns `None`.
    ///
    pub def map(f: a -> b \ ef, o: Option[a]): Option[b] \ ef = match o {
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
    pub def ap(f: Option[a -> b \ ef], x: Option[a]) : Option[b] \ ef =
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
    pub def flatMap(f: a -> Option[b] \ ef, o: Option[a]): Option[b] \ ef = match o {
        case None    => None
        case Some(v) => f(v)
    }

    ///
    /// Returns `1` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `0`.
    ///
    pub def count(f: a -> Bool \ ef, o: Option[a]): Int32 \ ef = match o {
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
    pub def sumWith(f: a -> Int32 \ ef, o: Option[a]): Int32 \ ef =
        Foldable.sumWith(f, o)

    ///
    /// Returns `o` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `None`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: a -> Bool, o: Option[a]): Option[a] = match o {
        case None    => None
        case Some(v) => if (f(v)) o else None
    }

    ///
    /// Returns `v` if `o` is `Some(v)`. Otherwise returns `None`.
    ///
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
    pub def foldLeft(f: (b, a) -> b \ ef, z: b, o: Option[a]): b \ ef = match o {
        case None    => z
        case Some(v) => f(z, v)
    }

    ///
    /// Returns `f(v, z)` if `o` is `Some(v)`. Otherwise returns `z`.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, o: Option[a], z: b): b \ ef = match o {
        case None    => z
        case Some(v) => f(v, z)
    }

    ///
    /// Returns `f(v, z)` if `o` is `Some(v)`. Otherwise returns `z`.
    ///
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, o: Option[a], z: b): b \ ef = match o {
        case None    => z
        case Some(v) => f(v, constant(z))
    }

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, o: Option[a]): b \ ef with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), o)

    ///
    /// Returns `Some(v1 :: v2 :: ... :: vn)` if each of `xs_i` is `Some(v_i)`. Otherwise returns `None`.
    ///
    pub def sequence(l: List[Option[a]]): Option[List[a]] =
        def loop(ll, k) = match ll {
            case Nil            => k(Nil)
            case None :: _      => None
            case Some(x) :: xs  => loop(xs, ks -> k(x :: ks))
        };
        loop(l, ks -> Some(ks))

    ///
    /// Returns `Some(v1 :: v2 :: ... v :: vn)` if each of `f(l_i)` is `Some(v_i)`. Otherwise returns `None`.
    ///
    pub def traverse(f: a -> Option[b] \ ef, l: List[a]): Option[List[b]] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => match f(x) {
                case None    => None
                case Some(y) => loop(xs, ys -> k(y :: ys))
            }
        };
        loop(l, ks -> Some(ks))

    ///
    /// Returns `Some()` if each of `f(l_i)` is `Some(_)`. Otherwise returns `None`.
    ///
    /// This function is the "forgetful" version of `traverse`, use it when the you want the effect
    /// of applying `f` to each element but do not care about collecting the results.
    ///
    pub def traverseX(f: a -> Option[b] \ ef, l: List[a]): Option[Unit] \ ef = match l {
        case Nil     => Some()
        case x :: xs => match f(x) {
            case None    => None
            case Some(_) => traverseX(f, xs)
        }
    }

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
    pub def foldLeftM(f: (b, a) -> Option[b] \ ef, s: b, l: List[a]): Option[b] \ ef = match l {
        case Nil     => Some(s)
        case x :: xs => match f(s, x) {
            case Some(s1) => foldLeftM(f, s1, xs)
            case None     => None
        }
    }

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
    pub def foldRightM(f: (a, b) -> Option[b] \ ef, s: b, l: List[a]): Option[b] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(s)
            case x :: xs => loop(xs, s1 -> match f(x, s1) {
                case Some(s2) => k(s2)
                case None     => None
            })
        };
        loop(l, s1 -> checked_ecast(Some(s1)))

    ///
    /// Returns a one-element list of the value `v` if `o` is `Some(v)`. Otherwise returns the empty list.
    ///
    pub def toList(o: Option[a]): List[a] = match o {
        case None    => Nil
        case Some(v) => v :: Nil
    }

    ///
    /// Returns a one-element set of the value `v` if `o` is `Some(v)`. Otherwise returns the empty set.
    ///
    pub def toSet(o: Option[a]): Set[a] with Order[a] = match o {
        case None    => Set.empty()
        case Some(v) => Set.singleton(v)
    }

    ///
    /// Returns a singleton map with the mapping `k -> v` if `o` is `Some((k, v))`. Otherwise returns the empty map.
    ///
    pub def toMap(o: Option[(k, v)]): Map[k, v] with Order[k] = match o {
        case None         => Map.empty()
        case Some((k, v)) => Map.singleton(k, v)
    }

    ///
    /// Returns a map with elements of `s` as keys and `f` applied as values.
    ///
    pub def toMapWith(f: a -> b, s: Option[a]): Map[a, b] with Order[a] =
        Option.foldRight((x, acc) -> Map.insert(x, f(x), acc), s, Map.empty())

    ///
    /// Returns the Option value `Ok(v)` if `o` is `Some(v)`. Otherwise returns `Err(e)`.
    ///
    pub def toOk(e: e, o: Option[t]): Result[e, t] = match o {
        case None    => Err(e)
        case Some(a) => Ok(a)
    }

    ///
    /// Returns the Option value `Err(e)` if `o` is `Some(e)`. Otherwise returns `Ok(d)`.
    ///
    pub def toErr(d: t, o: Option[e]): Result[e, t] = match o {
        case None    => Ok(d)
        case Some(e) => Err(e)
    }

    ///
    /// Returns the Validation value `Success(v)` if `o` is `Some(v)`. Otherwise lifts `e` into Validation's `Failure`.
    ///
    pub def toSuccess(e: e, o: Option[t]): Validation[e, t] = match o {
        case None    => Validation.Failure(Nec.singleton(e))
        case Some(a) => Validation.Success(a)
    }

    ///
    /// Returns `e` into Validation's `Failure` if `o` is `Some(e)`. Otherwise returns `Success(d)`.
    ///
    pub def toFailure(d: t, o: Option[e]): Validation[e, t] = match o {
        case None    => Validation.Success(d)
        case Some(e) => Validation.Failure(Nec.singleton(e))
    }

    ///
    /// Returns `Some((v1, v2))` if `o1` is `Some(v1)` and `o2` is `Some(v2)`. Otherwise returns `None`.
    ///
    pub def zip(o1: Option[a], o2: Option[b]): Option[(a, b)] = match (o1, o2) {
        case (None, _)            => None
        case (_, None)            => None
        case (Some(v1), Some(v2)) => Some((v1, v2))
    }

    ///
    /// Returns `(Some(v1), Some(v2))` if `o` is `Some((v1, v2))`. Otherwise returns `(None, None)`.
    ///
    pub def unzip(o: Option[(a, b)]): (Option[a], Option[b]) = match o {
        case None           => (None, None)
        case Some((v1, v2)) => (Some(v1), Some(v2))
    }

    ///
    /// Applies `f` to `v` if `o` is `Some(v)`. Otherwise does nothing.
    ///
    pub def forEach(f: a -> Unit \ ef, o: Option[a]): Unit \ ef = match o {
        case None    => ()
        case Some(v) => f(v)
    }

    ///
    /// Applies the binary function `f` to the values in `o1` and `o2`.
    ///
    /// Returns `None` if either `o1` or `o2` are `None`.
    ///
    pub def map2(f: (t1, t2) -> u \ ef, o1: Option[t1], o2: Option[t2]): Option[u] \ ef =
        ap(map(f, o1), o2)

    ///
    /// Applies the ternary function `f` to the values in `o1`, `o2` and `o3`.
    ///
    /// Returns `None` if any of `o1`, `o2` and `o3` are `None`.
    ///
    pub def map3(f: (t1, t2, t3) -> u \ ef, o1: Option[t1], o2: Option[t2], o3: Option[t3]): Option[u] \ ef =
        ap(map2(f, o1, o2), o3)

    ///
    /// Applies the 4-ary function `f` to the values in `o1`, `o2`, `o3` and `o4`.
    ///
    /// Returns `None` if any of `o1`, `o2`, `o3` and `o4` are `None`.
    ///
    pub def map4(f: (t1, t2, t3, t4) -> u \ ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4]): Option[u] \ ef =
        ap(map3(f, o1, o2, o3), o4)

    ///
    /// Applies the 5-ary function `f` to the values in `o1`, `o2`, ... `o5`.
    ///
    /// Returns `None` if any of `o1`, `o2`, ... `o5` are `None`.
    ///
    pub def map5(f: (t1, t2, t3, t4, t5) -> u \ ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4], o5: Option[t5]): Option[u] \ ef =
        ap(map4(f, o1, o2, o3, o4), o5)

    ///
    /// Applies the 6-ary function `f` to the values in `o1`, `o2`, ... `o6`.
    ///
    /// Returns `None` if any of `o1`, `o2`, ... `o6` are `None`.
    ///
    pub def map6(f: (t1, t2, t3, t4, t5, t6) -> u \ ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4], o5: Option[t5], o6: Option[t6]): Option[u] \ ef =
        ap(map5(f, o1, o2, o3, o4, o5), o6)

    ///
    /// Applies the 7-ary function `f` to the values in `o1`, `o2`, ... `o7`.
    ///
    /// Returns `None` if any of `o1`, `o2`, ... `o7` are `None`.
    ///
    pub def map7(f: (t1, t2, t3, t4, t5, t6, t7) -> u \ ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4], o5: Option[t5], o6: Option[t6], o7: Option[t7]): Option[u] \ ef=
        ap(map6(f, o1, o2, o3, o4, o5, o6), o7)

    ///
    /// Applies the 8-ary function `f` to the values in `o1`, `o2`, ... `o8`.
    ///
    /// Returns `None` if any of `o1`, `o2`, ... `o8` are `None`.
    ///
    pub def map8(f: (t1, t2, t3, t4, t5, t6, t7, t8) -> u \ ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4], o5: Option[t5], o6: Option[t6], o7: Option[t7], o8: Option[t8]): Option[u] \ ef =
        ap(map7(f, o1, o2, o3, o4, o5, o6, o7), o8)

    ///
    /// Applies the 9-ary function `f` to the values in `o1`, `o2`, ... `o9`.
    ///
    /// Returns `None` if any of `o1`, `o2`, ... `o9` are `None`.
    ///
    pub def map9(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9) -> u \ ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4], o5: Option[t5], o6: Option[t6], o7: Option[t7], o8: Option[t8], o9: Option[t9]): Option[u] \ ef =
        ap(map8(f, o1, o2, o3, o4, o5, o6, o7, o8), o9)

    ///
    /// Applies the 10-ary function `f` to the values in `o1`, `o2`, ... `o10`.
    ///
    /// Returns `None` if any of `o1`, `o2`, ... `o10` are `None`.
    ///
    pub def map10(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) -> u \ ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4], o5: Option[t5], o6: Option[t6], o7: Option[t7], o8: Option[t8], o9: Option[t9], o10: Option[t10]): Option[u] \ ef =
        ap(map9(f, o1, o2, o3, o4, o5, o6, o7, o8, o9), o10)

    ///
    /// Returns an iterator over `o` with 1 element or an empty iterator if `o` is `None`.
    ///
    pub def iterator(rc: Region[r], o: Option[a]): Iterator[a, r, r] \ r = match o {
        case None    => Iterator.empty(rc)
        case Some(x) => Iterator.singleton(rc, x)
    }

    ///
    /// Returns an iterator over `o` zipped with the indices of the elements.
    ///
    pub def enumerator(rc: Region[r], o: Option[a]): Iterator[(Int32, a), r, r] \ r =
        iterator(rc, o) |> Iterator.zipWithIndex

}
