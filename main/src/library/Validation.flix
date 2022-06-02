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
pub enum Validation[t, e] with Eq, Order, ToString {
    case Success(t),
    case Failure(Nec[e])
}

instance Hash[Validation[t, e]] with Hash[t], Hash[e] {
    pub def hash(x: Validation[t, e]): Int32 = match x {
        case Success(v) => 5407 + 197 * Hash.hash(v)
        case Failure(v) => 5413 + 199 * Hash.hash(v)
    }
}

instance SemiGroup[Validation[t, e]] with SemiGroup[t] {
    pub def combine(x: Validation[t, e], y: Validation[t, e]): Validation[t, e] = match (x, y) {
        case (Success(x1), Success(y1))     => Success(SemiGroup.combine(x1, y1))
        case (Failure(es1), Failure(es2))   => Failure(Nec.append(es1, es2))
        case (Failure(_), _)                => x
        case (_, Failure(_))                => y
    }
}

instance Monoid[Validation[t, e]] with Monoid[t] {
    pub def empty(): Validation[t, e] = Success(Monoid.empty())
}

namespace Validation {

    ///
    /// Applies the function in `v1` to the value in `v2`.
    ///
    @Time(1) @Space(1)
    pub def ap(v1: Validation[t -> u & ef, e], v2: Validation[t, e]): Validation[u, e] & ef = match (v1, v2) {
        case (Success(f), Success(v)) => Success(f(v))
        case (Success(_), Failure(e)) => Failure(e)
        case (Failure(e), Success(_)) => Failure(e)
        case (Failure(x), Failure(y)) => Failure(Nec.append(x, y))
    }

    ///
    /// Chain two functions, returns the product of their results.
    ///
    pub def product(fa: Validation[t1, e], fb: Validation[t2, e]): Validation[(t1, t2), e] =
        ap(map((a, b) -> (a, b), fa), fb)

    ///
    /// Chain three functions, returns the product of their results.
    ///
    pub def product3(fa: Validation[t1, e], fb: Validation[t2, e], fc: Validation[t3, e]): Validation[(t1, t2, t3), e] =
        ap(ap(map((a, b, c) -> (a, b, c), fa), fb), fc)

    ///
    /// Chain four functions, returns the product of their results.
    ///
    pub def product4(fa: Validation[t1, e], fb: Validation[t2, e], fc: Validation[t3, e], fd: Validation[t4, e]): Validation[(t1, t2, t3, t4), e] =
        ap(ap(ap(map((a, b, c, d) -> (a, b, c, d), fa), fb), fc), fd)

    ///
    /// Chain five functions, returns the product of their results.
    ///
    pub def product5(fa: Validation[t1, e], fb: Validation[t2, e], fc: Validation[t3, e], fd: Validation[t4, e], fe: Validation[t5, e]): Validation[(t1, t2, t3, t4, t5), e] =
        ap(ap(ap(ap(map((a, b, c, d, e) -> (a, b, c, d, e), fa), fb), fc), fd), fe)

    ///
    /// Returns `t` if `v` is `Success(t).` Otherwise returns `d`.
    ///
    @Time(1) @Space(1)
    pub def getWithDefault( d: t, v: Validation[t, e]): t = match v {
        case Success(t) => t
        case Failure(_) => d
    }

    ///
    /// Returns `v` if it is `Success(v)`. Otherwise returns `default`.
    ///
    @Time(1) @Space(1)
    pub def withDefault(default: {default :: Validation[t, e]}, v: Validation[t, e]): Validation[t, e] = match v {
        case Success(_) => v
        case Failure(_) => default.default
    }

    ///
    /// Returns `true` if and only if `v` is `Success(t)` and `f(t)` is true.
    ///
    /// Returns `false` if `v` is `Failure`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def exists(f: t -> Bool & ef, v: Validation[t, e]): Bool & ef = match v {
        case Success(t) => f(t)
        case Failure(_) => false
    }

    ///
    /// Returns `true` if `v` is `Success(t)` and `f(t)` is true or if `v` is `Failure`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def forall(f: t -> Bool & ef, v: Validation[t, e]): Bool & ef = match v {
        case Success(t) => f(t)
        case Failure(_) => true
    }

    ///
    /// Returns `Success(f(v))` if `o` is `Success(v)`. Otherwise returns `v`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def map(f: t -> u & ef, v: Validation[t, e]): Validation[u, e] & ef = match v {
        case Success(t) => Success(f(t))
        case Failure(e) => Failure(e)
    }

    ///
    /// Returns `Success(v1 :: v2 :: ... :: vn)` if each of `l_i` is `Success(v_i)`.
    ///
    /// Otherwise returns `Failure(e1 :: ... :: en)` with all of the failures concatenated.
    ///
    pub def sequence(l: List[Validation[t, e]]): Validation[List[t], e] =
        def loopFailure(ll, acc) = match ll {
            case Nil              => Failure(acc)
            case Success(_) :: xs => loopFailure(xs, acc)
            case Failure(e) :: xs => loopFailure(xs, Nec.append(acc, e))
        };
        def loop(ll, k) = match ll {
            case Nil              => k(Nil)
            case Success(x) :: xs => loop(xs, ks -> k(x :: ks))
            case Failure(e) :: xs => loopFailure(xs, e)
        };
        loop(l, ks -> Success(ks))

    ///
    /// Returns `Success(v1 :: v2 :: ... v :: vn)` if each of `f(l_i)` is `Success(v_i)`.
    ///
    /// Otherwise returns `Failure(e1 :: ... :: en)` with all of the failures concatenated.
    ///
    pub def traverse(f: a -> Validation[b, e] & ef, l: List[a]): Validation[List[b], e] & ef =
        def loopFailure(ll, acc) = match ll {
            case Nil     => Failure(acc)
            case x :: xs => match f(x) {
                case Success(_) => loopFailure(xs, acc)
                case Failure(y) => loopFailure(xs, Nec.append(acc, y))
            }
        };
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => match f(x) {
                case Success(y) => loop(xs, ks -> k(y :: ks))
                case Failure(y) => loopFailure(xs, y)
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
    pub def traverseX(f: a -> Validation[b, e] & ef, l: List[a]): Validation[Unit, e] & ef =
        def loopFailure(ll, acc) = match ll {
            case Nil     => Failure(acc)
            case x :: xs => match f(x) {
                case Success(_) => loopFailure(xs, acc)
                case Failure(e) => loopFailure(xs, Nec.append(acc, e))
            }
        };
        def loop(ll) = match ll {
            case Nil     => Success()
            case x :: xs => match f(x) {
                case Success(_) => loop(xs)
                case Failure(e) => loopFailure(xs, e)
            }
        };
        loop(l)

    ///
    /// Converts a Validation to an Option.
    ///
    /// Returns `Some(t)` if `v` is `Success(t)`.
    /// Returns `None` otherwise.
    ///
    @Time(1) @Space(1)
    pub def toOption(v: Validation[t, e]): Option[t] = match v {
        case Success(t) => Some(t)
        case Failure(_) => None
    }

    ///
    /// Converts a Validation to a Result.
    ///
    /// Returns `Ok(t)` if `v` is `Success(t)`.
    /// Returns `Err(e)` if `v` is `Failure(e)`.
    ///
    @Time(1) @Space(1)
    pub def toResult(v: Validation[t, e]): Result[t, Nec[e]] = match v {
        case Success(t) => Ok(t)
        case Failure(e) => Err(e)
    }

    ///
    /// Converts a Validation to a List.
    ///
    /// Returns `t :: Nil` if `v` is `Success(v)`.
    /// Returns `Nil` if `v` is `Failure(e)`.
    ///
    @Time(1) @Space(1)
    pub def toList(v: Validation[t, e]): List[t] = match v {
        case Success(t) => t :: Nil
        case Failure(_) => Nil
    }

    ///
    /// Applies the binary function `f` to the values in `v1` and `v2`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if either or both of `v1` or `v2` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift2(f: (t1, t2) -> u & ef, v1: Validation[t1, e], v2: Validation[t2, e]): Validation[u, e] & ef =
        ap(map(f, v1), v2)

    ///
    /// Applies the ternary function `f` to the values in `v1`, `v2` and `v3`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2` and `v3` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift3(f: (t1, t2, t3) -> u & ef, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e]): Validation[u, e] & ef =
        ap(lift2(f, v1, v2), v3)

    ///
    /// Applies the 4-ary function `f` to the values in `v1`, `v2`, `v3` and `v4`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, `v3` and `v4` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift4(f: (t1, t2, t3, t4) -> u & ef, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e]): Validation[u, e] & ef=
        ap(lift3(f, v1, v2, v3), v4)

    ///
    /// Applies the 5-ary function `f` to the values in `v1`, `v2`, ... `v5`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v5` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift5(f: (t1, t2, t3, t4, t5) -> u & ef, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e]): Validation[u, e] & ef =
        ap(lift4(f, v1, v2, v3, v4), v5)

    ///
    /// Applies the 6-ary function `f` to the values in `v1`, `v2`, ... `v6`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v6` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift6(f: (t1, t2, t3, t4, t5, t6) -> u & ef, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e], v6: Validation[t6, e]): Validation[u, e] & ef =
        ap(lift5(f, v1, v2, v3, v4, v5), v6)

    ///
    /// Applies the 7-ary function `f` to the values in `v1`, `v2`, ... `v7`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v7` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift7(f: (t1, t2, t3, t4, t5, t6, t7) -> u & ef, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e], v6: Validation[t6, e], v7: Validation[t7, e]): Validation[u, e] & ef=
        ap(lift6(f, v1, v2, v3, v4, v5, v6), v7)

    ///
    /// Applies the 8-ary function `f` to the values in `v1`, `v2`, ... `v8`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v8` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift8(f: (t1, t2, t3, t4, t5, t6, t7, t8) -> u & ef, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e], v6: Validation[t6, e], v7: Validation[t7, e], v8: Validation[t8, e]): Validation[u, e] & ef =
        ap(lift7(f, v1, v2, v3, v4, v5, v6, v7), v8)

    ///
    /// Applies the 9-ary function `f` to the values in `v1`, `v2`, ... `v9`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v9` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift9(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9) -> u & ef, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e], v6: Validation[t6, e], v7: Validation[t7, e], v8: Validation[t8, e], v9: Validation[t9, e]): Validation[u, e] & ef =
        ap(lift8(f, v1, v2, v3, v4, v5, v6, v7, v8), v9)

    ///
    /// Applies the 10-ary function `f` to the values in `v1`, `v2`, ... `v10`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v10` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift10(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) -> u & ef, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e], v6: Validation[t6, e], v7: Validation[t7, e], v8: Validation[t8, e], v9: Validation[t9, e], v10: Validation[t10, e]): Validation[u, e] & ef =
        ap(lift9(f, v1, v2, v3, v4, v5, v6, v7, v8, v9), v10)

}
