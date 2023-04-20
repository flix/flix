/*
 * Copyright 2020 Magnus Madsen
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
/// The Validation type.
///
@MustUse
pub enum Validation[e, t] with Eq, Order, ToString, Sendable {
    case Success(t),
    case Failure(Nec[e])
}

instance Hash[Validation[e, t]] with Hash[e], Hash[t] {
    pub def hash(x: Validation[e, t]): Int32 = match x {
        case Validation.Success(v) => 5407 + 197 * Hash.hash(v)
        case Validation.Failure(v) => 5413 + 199 * Hash.hash(v)
    }
}

instance SemiGroup[Validation[e, t]] with SemiGroup[t] {
    pub def combine(x: Validation[e, t], y: Validation[e, t]): Validation[e, t] = match (x, y) {
        case (Validation.Success(x1), Validation.Success(y1))       => Validation.Success(SemiGroup.combine(x1, y1))
        case (Validation.Failure(es1), Validation.Failure(es2))     => Validation.Failure(Nec.append(es1, es2))
        case (Validation.Failure(_), _)                             => x
        case (_, Validation.Failure(_))                             => y
    }
}

instance Monoid[Validation[e, t]] with Monoid[t] {
    pub def empty(): Validation[e, t] = Validation.Success(Monoid.empty())
}

instance Functor[Validation[e]] {
    pub def map(f: a -> b \ ef, x: Validation[e, a]): Validation[e, b] \ ef = Validation.map(f, x)
}

instance Applicative[Validation[e]] {
    pub def point(a: a) : Validation[e, a] = Validation.Success(a)
    pub def ap(f: Validation[e, a -> b \ ef], x: Validation[e, a]): Validation[e, b] \ ef = Validation.ap(f, x)
}


mod Validation {

    ///
    /// Applies the function in `v1` to the value in `v2`.
    ///
    pub def ap(v1: Validation[e, t -> u \ ef], v2: Validation[e, t]): Validation[e, u] \ ef = match (v1, v2) {
        case (Success(f), Success(v)) => Success(f(v))
        case (Success(_), Failure(e)) => Failure(e)
        case (Failure(e), Success(_)) => Failure(e)
        case (Failure(x), Failure(y)) => Failure(Nec.append(x, y))
    }

    ///
    /// Chain two functions, returns the product of their results.
    ///
    pub def product(fa: Validation[e, t1], fb: Validation[e, t2]): Validation[e, (t1, t2)] =
        ap(map((a, b) -> (a, b), fa), fb)

    ///
    /// Chain three functions, returns the product of their results.
    ///
    pub def product3(fa: Validation[e, t1], fb: Validation[e, t2], fc: Validation[e, t3]): Validation[e, (t1, t2, t3)] =
        ap(ap(map((a, b, c) -> (a, b, c), fa), fb), fc)

    ///
    /// Chain four functions, returns the product of their results.
    ///
    pub def product4(fa: Validation[e, t1], fb: Validation[e, t2], fc: Validation[e, t3], fd: Validation[e, t4]): Validation[e, (t1, t2, t3, t4)] =
        ap(ap(ap(map((a, b, c, d) -> (a, b, c, d), fa), fb), fc), fd)

    ///
    /// Chain five functions, returns the product of their results.
    ///
    pub def product5(fa: Validation[e, t1], fb: Validation[e, t2], fc: Validation[e, t3], fd: Validation[e, t4], fe: Validation[e, t5]): Validation[e, (t1, t2, t3, t4, t5)] =
        ap(ap(ap(ap(map((a, b, c, d, e) -> (a, b, c, d, e), fa), fb), fc), fd), fe)

    ///
    /// Returns `t` if `v` is `Success(t).` Otherwise returns `d`.
    ///
    pub def getWithDefault(d: t, v: Validation[e, t]): t = match v {
        case Success(t) => t
        case Failure(_) => d
    }

    ///
    /// Returns `v` if it is `Success(v)`. Otherwise returns `default`.
    ///
    pub def withDefault(default: {default = Validation[e, t]}, v: Validation[e, t]): Validation[e, t] = match v {
        case Success(_) => v
        case Failure(_) => default.default
    }

    ///
    /// Returns `true` if and only if `v` is `Success(t)` and `f(t)` is true.
    ///
    /// Returns `false` if `v` is `Failure`.
    ///
    pub def exists(f: t -> Bool \ ef, v: Validation[e, t]): Bool \ ef = match v {
        case Success(t) => f(t)
        case Failure(_) => false
    }

    ///
    /// Returns `true` if `v` is `Success(t)` and `f(t)` is true or if `v` is `Failure`.
    ///
    pub def forAll(f: t -> Bool \ ef, v: Validation[e, t]): Bool \ ef = match v {
        case Success(t) => f(t)
        case Failure(_) => true
    }

    ///
    /// Returns `Success(f(v))` if `o` is `Success(v)`. Otherwise returns `v`.
    ///
    pub def map(f: t -> u \ ef, v: Validation[e, t]): Validation[e, u] \ ef = match v {
        case Success(t) => Success(f(t))
        case Failure(e) => Failure(e)
    }

    ///
    /// Returns `Success(v1 :: v2 :: ... :: vn)` if each of `l_i` is `Success(v_i)`.
    ///
    /// Otherwise returns `Failure(e1 :: ... :: en)` with all of the failures concatenated.
    ///
    pub def sequence(l: List[Validation[e, t]]): Validation[e, List[t]] =
        def loop(ll, k) = match ll {
            case Nil              => k(Nil)
            case Success(x) :: xs => loop(xs, ks -> k(x :: ks))
            case Failure(e) :: xs => collectFailuresWith(identity, xs, e)
        };
        loop(l, ks -> Success(ks))

    ///
    /// Returns `Success(v1 :: v2 :: ... v :: vn)` if each of `f(l_i)` is `Success(v_i)`.
    ///
    /// Otherwise returns `Failure(e1 :: ... :: en)` with all of the failures concatenated.
    ///
    pub def traverse(f: a -> Validation[e, b] \ ef, l: List[a]): Validation[e, List[b]] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => match f(x) {
                case Success(y) => loop(xs, ks -> k(y :: ks))
                case Failure(y) => collectFailuresWith(f, xs, y)
            }
        };
        loop(l, ks -> Success(ks))

    ///
    /// Returns `Success()` if each of `f(l_i)` is `Success(_)`.
    ///
    /// Otherwise returns `Failure(e1 :: ... :: en)` with all of the failures concatenated.
    ///
    /// This function is the "forgetful" version of `traverse`, use it when the you want the effect
    /// of applying `f` to each element but do not care about collecting the results.
    ///
    pub def traverseX(f: a -> Validation[e, b] \ ef, l: List[a]): Validation[e, Unit] \ ef = match l {
        case Nil     => Success()
        case x :: xs => match f(x) {
            case Success(_) => traverseX(f, xs)
            case Failure(e) => collectFailuresWith(f, xs, e)
        }
    }

    ///
    /// Helper function for `sequence`, `traverse` and `traverseX`. Collects a chain of failures.
    ///
    def collectFailuresWith(f: a -> Validation[e, b] \ ef, l: List[a], acc: Nec[e]): Validation[e, c] \ ef = match l {
        case Nil     => Failure(acc)
        case x :: xs => match f(x) {
            case Success(_) => collectFailuresWith(f, xs, acc)
            case Failure(e) => collectFailuresWith(f, xs, Nec.append(acc, e))
        }
    }

    ///
    /// Converts a Validation to an Option.
    ///
    /// Returns `Some(t)` if `v` is `Success(t)`.
    /// Returns `None` otherwise.
    ///
    pub def toOption(v: Validation[e, t]): Option[t] = match v {
        case Success(t) => Some(t)
        case Failure(_) => None
    }

    ///
    /// Converts a Validation to a Result.
    ///
    /// Returns `Ok(t)` if `v` is `Success(t)`.
    /// Returns `Err(e)` if `v` is `Failure(e)`.
    ///
    pub def toResult(v: Validation[e, t]): Result[Nec[e], t] = match v {
        case Success(t) => Ok(t)
        case Failure(e) => Err(e)
    }

    ///
    /// Converts a Validation to a List.
    ///
    /// Returns `t :: Nil` if `v` is `Success(v)`.
    /// Returns `Nil` if `v` is `Failure(e)`.
    ///
    pub def toList(v: Validation[e, t]): List[t] = match v {
        case Success(t) => t :: Nil
        case Failure(_) => Nil
    }

    ///
    /// Applies the binary function `f` to the values in `v1` and `v2`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if either or both of `v1` or `v2` are `Failure(xs1)`.
    ///
    pub def map2(f: (t1, t2) -> u \ ef, v1: Validation[e, t1], v2: Validation[e, t2]): Validation[e, u] \ ef =
        ap(map(f, v1), v2)

    ///
    /// Applies the ternary function `f` to the values in `v1`, `v2` and `v3`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2` and `v3` are `Failure(xs1)`.
    ///
    pub def map3(f: (t1, t2, t3) -> u \ ef, v1: Validation[e, t1], v2: Validation[e, t2], v3: Validation[e, t3]): Validation[e, u] \ ef =
        ap(map2(f, v1, v2), v3)

    ///
    /// Applies the 4-ary function `f` to the values in `v1`, `v2`, `v3` and `v4`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, `v3` and `v4` are `Failure(xs1)`.
    ///
    pub def map4(f: (t1, t2, t3, t4) -> u \ ef, v1: Validation[e, t1], v2: Validation[e, t2], v3: Validation[e, t3], v4: Validation[e, t4]): Validation[e, u] \ ef=
        ap(map3(f, v1, v2, v3), v4)

    ///
    /// Applies the 5-ary function `f` to the values in `v1`, `v2`, ... `v5`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v5` are `Failure(xs1)`.
    ///
    pub def map5(f: (t1, t2, t3, t4, t5) -> u \ ef, v1: Validation[e, t1], v2: Validation[e, t2], v3: Validation[e, t3], v4: Validation[e, t4], v5: Validation[e, t5]): Validation[e, u] \ ef =
        ap(map4(f, v1, v2, v3, v4), v5)

    ///
    /// Applies the 6-ary function `f` to the values in `v1`, `v2`, ... `v6`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v6` are `Failure(xs1)`.
    ///
    pub def map6(f: (t1, t2, t3, t4, t5, t6) -> u \ ef, v1: Validation[e, t1], v2: Validation[e, t2], v3: Validation[e, t3], v4: Validation[e, t4], v5: Validation[e, t5], v6: Validation[e, t6]): Validation[e, u] \ ef =
        ap(map5(f, v1, v2, v3, v4, v5), v6)

    ///
    /// Applies the 7-ary function `f` to the values in `v1`, `v2`, ... `v7`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v7` are `Failure(xs1)`.
    ///
    pub def map7(f: (t1, t2, t3, t4, t5, t6, t7) -> u \ ef, v1: Validation[e, t1], v2: Validation[e, t2], v3: Validation[e, t3], v4: Validation[e, t4], v5: Validation[e, t5], v6: Validation[e, t6], v7: Validation[e, t7]): Validation[e, u] \ ef=
        ap(map6(f, v1, v2, v3, v4, v5, v6), v7)

    ///
    /// Applies the 8-ary function `f` to the values in `v1`, `v2`, ... `v8`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v8` are `Failure(xs1)`.
    ///
    pub def map8(f: (t1, t2, t3, t4, t5, t6, t7, t8) -> u \ ef, v1: Validation[e, t1], v2: Validation[e, t2], v3: Validation[e, t3], v4: Validation[e, t4], v5: Validation[e, t5], v6: Validation[e, t6], v7: Validation[e, t7], v8: Validation[e, t8]): Validation[e, u] \ ef =
        ap(map7(f, v1, v2, v3, v4, v5, v6, v7), v8)

    ///
    /// Applies the 9-ary function `f` to the values in `v1`, `v2`, ... `v9`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v9` are `Failure(xs1)`.
    ///
    pub def map9(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9) -> u \ ef, v1: Validation[e, t1], v2: Validation[e, t2], v3: Validation[e, t3], v4: Validation[e, t4], v5: Validation[e, t5], v6: Validation[e, t6], v7: Validation[e, t7], v8: Validation[e, t8], v9: Validation[e, t9]): Validation[e, u] \ ef =
        ap(map8(f, v1, v2, v3, v4, v5, v6, v7, v8), v9)

    ///
    /// Applies the 10-ary function `f` to the values in `v1`, `v2`, ... `v10`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v10` are `Failure(xs1)`.
    ///
    pub def map10(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) -> u \ ef, v1: Validation[e, t1], v2: Validation[e, t2], v3: Validation[e, t3], v4: Validation[e, t4], v5: Validation[e, t5], v6: Validation[e, t6], v7: Validation[e, t7], v8: Validation[e, t8], v9: Validation[e, t9], v10: Validation[e, t10]): Validation[e, u] \ ef =
        ap(map9(f, v1, v2, v3, v4, v5, v6, v7, v8, v9), v10)

}
