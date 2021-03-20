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
pub enum Validation[t, e] {
    case Success(t),
    case Failure(Nel[e])
}

instance ToString[Validation[t, e]] with [t : ToString, e : ToString] {
    pub def toString(x: Validation[t, e]): String = match x {
        case Success(t) => "Success(${t})"
        case Failure(e) => "Failure(${e})"
    }
}

instance Hash[Validation[t, e]] with [t : Hash, e : Hash] {
    pub def hash(x: Validation[t, e]): Int = match x {
        case Success(v) => 5407 + 197 * Hash.hash(v)
        case Failure(v) => 5413 + 199 * Hash.hash(v)
    }
}

instance Eq[Validation[t, e]] with[t : Eq, e : Eq] {

    pub def eq(v1: Validation[t, e], v2: Validation[t, e]): Bool = match (v1, v2) {
        case (Success(t1), Success(t2)) => t1 == t2
        case (Failure(e1), Failure(e2)) => e1 == e2
        case _ => false
    }

}

namespace Validation {

    ///
    /// Applies the function in `v1` to the value in `v2`.
    ///
    @Time(1) @Space(1)
    pub def ap(v1: Validation[t -> u & f, e], v2: Validation[t, e]): Validation[u, e] & f = match (v1, v2) {
        case (Success(f), Success(v)) => Success(f(v))
        case (Success(_), Failure(e)) => Failure(e)
        case (Failure(e), Success(_)) => Failure(e)
        case (Failure(x), Failure(y)) => Failure(Nel.append(x, y))
    }

    ///
    /// Returns `t` if `v` is `Success(t).` Otherwise returns `d`.
    ///
    @Time(1) @Space(1)
    pub def getWithDefault(v: Validation[t, e], d: t): t = match v {
        case Success(t) => t
        case Failure(_) => d
    }

    ///
    /// Returns `v1` if it is `Success(v)`. Otherwise returns `v2`.
    ///
    @Time(1) @Space(1)
    pub def withDefault(v1: Validation[t, e], v2: Validation[t, e]): Validation[t, e] = match v1 {
        case Success(_) => v1
        case Failure(_) => v2
    }

    ///
    /// Returns `true` if and only if `v` is `Success(t)` and `f(t)` is true.
    ///
    /// Returns `false` if `v` is `Failure`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def exists(f: t -> Bool, v: Validation[t, e]): Bool = match v {
        case Success(t) => f(t)
        case Failure(_) => false
    }

    ///
    /// Returns `true` if `v` is `Success(t)` and `f(t)` is true or if `v` is `Failure`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def forall(f: t -> Bool, v: Validation[t, e]): Bool = match v {
        case Success(t) => f(t)
        case Failure(_) => true
    }

    ///
    /// Returns `Success(f(v))` if `o` is `Success(v)`. Otherwise returns `v`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def map(f: t -> u & f, v: Validation[t, e]): Validation[u, e] & f = match v {
        case Success(t) => Success(f(t))
        case Failure(e) => Failure(e)
    }

    ///
    /// Returns `Success(v1 :: v2 :: ... :: vn)` if each of `xs_i` is `Success(v_i)`.
    ///
    /// Otherwise returns `Failure(e1 :: ... :: en)` with all of the failures concatenated.
    ///
    pub def sequence(xs: List[Validation[t, e]]): Validation[List[t], e] =
        sequenceHelperSuccess(xs, ys -> Success(ys))

    ///
    /// Helper function for `sequence`.
    ///
    /// Precondition: no Failure has been encountered.
    ///
    def sequenceHelperSuccess(xs: List[Validation[a, e]], sk: List[a] -> Validation[List[a], e]): Validation[List[a], e] = match xs {
        case Nil              => sk(Nil)
        case Success(x) :: vs => sequenceHelperSuccess(vs, xs -> sk(x :: xs))
        case Failure(e) :: vs => sequenceHelperFailure(vs, e)
    }

    ///
    /// Helper function for `sequence`.
    ///
    /// Precondition: at least one Failure has been encountered.
    ///
    def sequenceHelperFailure(xs: List[Validation[a, e]], ac: Nel[e]): Validation[List[a], e] = match xs {
        case Nil              => Failure(ac)
        case Success(_) :: vs => sequenceHelperFailure(vs, ac)
        case Failure(e) :: vs => sequenceHelperFailure(vs, Nel.append(ac, e))
    }


    ///
    /// Returns `Success(v1 :: v2 :: ... v :: vn)` if each of `f(xs_i)` is `Success(v_i)`.
    ///
    /// Otherwise returns `Failure(e1 :: ... :: en)` with all of the failures concatenated.
    ///
    pub def traverse(f: a -> Validation[b, e] & f, xs: List[a]): Validation[List[b], e] & f =
        traverseHelperSuccess(f, xs, ys -> Success(ys))

    ///
    /// Helper function for `traverse`.
    ///
    /// Precondition: no Failure has been encountered.
    ///
    def traverseHelperSuccess(f: a -> Validation[b, e] & f, xs: List[a], sk: List[b] -> Validation[List[b], e]): Validation[List[b], e] & f = match xs {
        case Nil        => sk(Nil)
        case v :: vs    => match f(v) {
            case Success(x) => traverseHelperSuccess(f, vs, xs -> sk(x :: xs))
            case Failure(x) => traverseHelperFailure(f, vs, x)
        }
    }

    ///
    /// Helper function for `traverse`.
    ///
    /// Precondition: at least one Failure has been encountered.
    ///
    def traverseHelperFailure(f: a -> Validation[b, e] & f, xs: List[a], ac: Nel[e]): Validation[List[b], e] & f = match xs {
        case Nil        => Failure(ac)
        case v :: vs    => match f(v) {
            case Success(_) => traverseHelperFailure(f, vs, ac)
            case Failure(x) => traverseHelperFailure(f, vs, Nel.append(ac, x))
        }
    }

    ///
    /// Returns `Success()` if each of `f(xs_i)` is `Success(_)`.
    ///
    /// Otherwise returns `Failure(e1 :: ... :: en)` with all of the failures concatenated.
    ///
    /// This function is the "forgetful" version of `traverse`, use it when the you want the effect
    /// of applying `f` to each element but do not care about collecting the results.
    ///
    pub def traverseX(f: a -> Validation[b, e] & f, xs: List[a]): Validation[Unit, e] & f =
        traverseXHelperSuccess(f, xs)

    ///
    /// Helper function for `traverseX`.
    ///
    /// Precondition: no Failure has been encountered.
    ///
    def traverseXHelperSuccess(f: a -> Validation[b, e] & f, xs: List[a]): Validation[Unit, e] & f = match xs {
        case Nil        => Success()
        case v :: vs    => match f(v) {
            case Success(_) => traverseXHelperSuccess(f, vs)
            case Failure(e) => traverseXHelperFailure(f, vs, e)
        }
    }

    ///
    /// Helper function for `traverseX`.
    ///
    /// Precondition: at least one Failure has been encountered.
    ///
    def traverseXHelperFailure(f: a -> Validation[b, e] & f, xs: List[a], ac: Nel[e]): Validation[Unit, e] & f = match xs {
        case Nil        => Failure(ac)
        case v :: vs    => match f(v) {
            case Success(_) => traverseXHelperFailure(f, vs, ac)
            case Failure(e) => traverseXHelperFailure(f, vs, Nel.append(ac, e))
        }
    }

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
    pub def toResult(v: Validation[t, e]): Result[t, Nel[e]] = match v {
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
    pub def lift2(f: (t1, t2) -> u & f, v1: Validation[t1, e], v2: Validation[t2, e]): Validation[u, e] & f =
        ap(map(f, v1), v2)

    ///
    /// Applies the ternary function `f` to the values in `v1`, `v2` and `v3`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2` and `v3` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift3(f: (t1, t2, t3) -> u & f, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e]): Validation[u, e] & f =
        ap(lift2(f, v1, v2), v3)

    ///
    /// Applies the 4-ary function `f` to the values in `v1`, `v2`, `v3` and `v4`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, `v3` and `v4` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift4(f: (t1, t2, t3, t4) -> u & f, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e]): Validation[u, e] & f=
        ap(lift3(f, v1, v2, v3), v4)

    ///
    /// Applies the 5-ary function `f` to the values in `v1`, `v2`, ... `v5`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v5` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift5(f: (t1, t2, t3, t4, t5) -> u & f, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e]): Validation[u, e] & f =
        ap(lift4(f, v1, v2, v3, v4), v5)

    ///
    /// Applies the 6-ary function `f` to the values in `v1`, `v2`, ... `v6`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v6` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift6(f: (t1, t2, t3, t4, t5, t6) -> u & f, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e], v6: Validation[t6, e]): Validation[u, e] & f =
        ap(lift5(f, v1, v2, v3, v4, v5), v6)

    ///
    /// Applies the 7-ary function `f` to the values in `v1`, `v2`, ... `v7`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v7` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift7(f: (t1, t2, t3, t4, t5, t6, t7) -> u & f, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e], v6: Validation[t6, e], v7: Validation[t7, e]): Validation[u, e] & f=
        ap(lift6(f, v1, v2, v3, v4, v5, v6), v7)

    ///
    /// Applies the 8-ary function `f` to the values in `v1`, `v2`, ... `v8`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v8` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift8(f: (t1, t2, t3, t4, t5, t6, t7, t8) -> u & f, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e], v6: Validation[t6, e], v7: Validation[t7, e], v8: Validation[t8, e]): Validation[u, e] & f =
        ap(lift7(f, v1, v2, v3, v4, v5, v6, v7), v8)

    ///
    /// Applies the 9-ary function `f` to the values in `v1`, `v2`, ... `v9`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v9` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift9(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9) -> u & f, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e], v6: Validation[t6, e], v7: Validation[t7, e], v8: Validation[t8, e], v9: Validation[t9, e]): Validation[u, e] & f =
        ap(lift8(f, v1, v2, v3, v4, v5, v6, v7, v8), v9)

    ///
    /// Applies the 10-ary function `f` to the values in `v1`, `v2`, ... `v10`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v10` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift10(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) -> u & f, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e], v6: Validation[t6, e], v7: Validation[t7, e], v8: Validation[t8, e], v9: Validation[t9, e], v10: Validation[t10, e]): Validation[u, e] & f =
        ap(lift9(f, v1, v2, v3, v4, v5, v6, v7, v8, v9), v10)


}

