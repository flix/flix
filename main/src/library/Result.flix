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

instance ToString[Result[t, e]] with [t : ToString, e : ToString] {
    def toString(r: Result[t, e]): String = match r {
        case Ok(t)  => "Ok(${t})"
        case Err(e) => "Err(${e})"
    }
}

instance Eq[Result[t, e]] with[t : Eq, e : Eq] {

    def eq(r1: Result[t, e], r2: Result[t, e]): Bool = match (r1, r2) {
        case (Ok(v1), Ok(v2)) => v1 == v2
        case (Err(w1), Err(w2)) => w1 == w2
        case _ => false
    }

    def neq(r1: Result[t, e], r2: Result[t, e]): Bool = match (r1, r2) {
        case (Ok(v1), Ok(v2)) => v1 != v2
        case (Err(w1), Err(w2)) => w1 != w2
        case _ => true
    }

}

namespace Result {

    ///
    /// Returns `true` iff `r` is `Ok(v)`.
    ///
    @Time(1) @Space(1)
    pub def isOk(r: Result[t, e]): Bool = match r {
        case Ok(_) => true
        case Err(_) => false
    }

    ///
    /// Returns `true` iff `r` is `Err(w)`.
    ///
    @Time(1) @Space(1)
    pub def isErr(r: Result[t, e]): Bool = match r {
        case Ok(_) => false
        case Err(_) => true
    }

    ///
    /// Returns `v` if `r` is `Ok(v)`. Otherwise returns `d`.
    ///
    @Time(1) @Space(1)
    pub def getWithDefault(r: Result[t, e], d: t): t = match r {
        case Ok(v) => v
        case Err(_) => d
    }

    ///
    /// Returns `Ok(v)` if `r1` is `Ok(v)`. Otherwise returns `r2`.
    ///
    @Time(1) @Space(1)
    pub def withDefault(r1: Result[t, e1], r2: Result[t, e2]): Result[t, e2] = match r1 {
        case Ok(v) => Ok(v)
        case Err(_) => r2
    }

    ///
    /// Returns `Ok(y)` if `r` is `Ok(x)`. Otherwise returns `r`.
    ///
    @Time(1) @Space(1)
    pub def replace[t : Eq, e](x: t, y: t, r: Result[t, e]): Result[t, e] = match r {
        case Ok(v) => Ok(if (v == x) y else v)
        case Err(_) => r
    }

    ///
    /// Returns `true` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `false`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f)) @Space(space(f))
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
    @Time(time(f)) @Space(space(f))
    pub def forall(f: t -> Bool, r: Result[t, e]): Bool = match r {
        case Ok(t)  => f(t)
        case Err(_) => true
    }

    ///
    /// Returns `Ok(f(v))` if `r` is `Ok(v)`. Returns `Err(w)` if `r` is `Err(w)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def map(f: t1 -> t2 & f, r: Result[t1, e]): Result[t2, e] & f = match r {
        case Ok(v) => Ok(f(v))
        case Err(w) => Err(w)
    }

    ///
    /// Returns `Err(f(e))` if `r` is `Err(e)`. Returns `Ok(v)` if `r` is `Ok(v)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def mapErr(f: e1 -> e2 & f, r: Result[t, e1]): Result[t, e2] & f = match r {
        case Ok(v) => Ok(v)
        case Err(w) => Err(f(w))
    }


    ///
    /// Returns `f(v)` if `r` is `Ok(v)`. Returns `Err(w)` if `r` is `Err(w)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def flatMap(f: t1 -> Result[t2, e] & f, r: Result[t1, e]): Result[t2, e] & f = match r {
        case Ok(v) => f(v)
        case Err(w) => Err(w)
    }

    ///
    /// Returns `1` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `0`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f)) @Space(space(f))
    pub def count(f: t -> Bool, r: Result[t, e]): Int32 = match r {
        case Ok(v) => if (f(v)) 1 else 0
        case Err(_) => 0
    }

    ///
    /// Returns `Some(v)` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `None`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f)) @Space(space(f))
    pub def find(f: t -> Bool, r: Result[t, e]): Option[t] = match r {
        case Ok(v) => if (f(v)) Some(v) else None
        case Err(_) => None
    }

    ///
    /// Returns `f(z, v)` if `r` is `Ok(v)`. Otherwise returns `z`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def foldLeft(f: (a, t) -> a & f, z: a, r: Result[t, e]): a & f = match r {
        case Ok(v) => f(z, v)
        case Err(_) => z
    }

    ///
    /// Returns `f(v, z)` if `r` is `Ok(v)`. Otherwise returns `z`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def foldRight(f: (t, a) -> a & f, r: Result[t, e], z: a): a & f = match r {
        case Ok(v) => f(v, z)
        case Err(_) => z
    }

    ///
    /// Returns `Ok(v1 :: v2 :: ... :: vn)` if each of `xs_i` is `Ok(v_i)`.
    /// Otherwise returns the first `Err` encountered.
    ///
    pub def sequence(xs: List[Result[a, e]]): Result[List[a], e] =
        sequenceHelper(xs, e -> Err(e), ys -> Ok(ys))

    ///
    /// Helper function for `sequence`.
    ///
    def sequenceHelper(xs: List[Result[a, e]], fk: e -> Result[List[a], e], sk: List[a] -> Result[List[a], e]): Result[List[a], e] = match xs {
        case Nil            => sk(Nil)
        case Err(e) :: _    => fk(e)
        case Ok(y) :: rs    => sequenceHelper(rs, fk, ys -> sk(y :: ys))
    }

    ///
    /// Returns `Some(v1 :: v2 :: ... v :: vn)` if each of `f(xs_i)` is `Ok(v_i)`.
    /// Otherwise returns the first `Err` encountered.
    ///
    pub def traverse(f: a -> Result[b, e] & f, xs: List[a]): Result[List[b], e] & f =
        traverseHelper(f, xs, e -> Err(e), ys -> Ok(ys))

    ///
    /// Helper function for `traverse`.
    ///
    def traverseHelper(f: a -> Result[b, e] & f, xs: List[a], fk: e -> Result[List[b], e], sk: List[b] -> Result[List[b], e]): Result[List[b], e] & f = match xs {
        case Nil            => sk(Nil)
        case x :: rs        => match f(x) {
            case Err(e) => fk(e)
            case Ok(y)  => traverseHelper(f, rs, fk, ys -> sk(y :: ys))
        }
    }

    ///
    /// Returns `Ok()` if each of `f(xs_i)` is `Ok(_)`. Otherwise returns `None`.
    ///
    /// This function is the "forgetful" version of `traverse`, use it when the you want the effect
    /// of applying `f` to each element but do not care about collecting the results.
    ///
    pub def traverseX(f: a -> Result[b, e] & f, xs: List[a]): Result[Unit, e] & f = match xs {
        case Nil            => Ok()
        case x :: rs        => match f(x) {
            case Err(e) => Err(e)
            case Ok(_)  => traverseX(f, rs)
        }
    }

    ///
    /// Returns the result of applying `f` to a start value `s` and the elements in `xs`
    /// going from left to right.
    ///
    /// If at any step applying `f` fails (i.e. it produces a `Err(e)` value) the traversal
    /// of `xs` is short-circuited and `Err(e)` is returned.
    ///
    /// If `f` is successfully applied to all elements in `xs` the result is of the form:
    /// `Ok(f(...f(f(s, x1), x2)..., xn))`.
    ///
    pub def foldLeftM(f: (b, a) -> Result[b, e] & f, s: b, xs: List[a]): Result[b, e] & f = match xs {
        case Nil => Ok(s)
        case x :: rs => match f(s, x) {
            case Ok(s1) => foldLeftM(f, s1, rs)
            case Err(e) => Err(e)
        }
    }

    ///
    /// Returns the result of applying `f` to a start value `s` and the elements in `xs`
    /// going from right to left.
    ///
    /// If at any step applying `f` fails (i.e. it produces a `Err(e)` value) the traversal
    /// of `xs` is short-circuited and `Err(e)` is returned.
    ///
    /// If `f` is successfully applied to all elements in `xs` the result is of the form:
    /// `Ok(f(x1, ...f(xn-1, f(xn, s))...))`.
    ///
    pub def foldRightM(f: (a, b) -> Result[b, e] & f, s: b, xs: List[a]): Result[b, e] & f =
        foldRightMHelper(f, s, xs, e1 -> Err(e1) as & f, s1 -> Ok(s1) as & f)

    ///
    /// Helper function for `foldRightM`.
    ///
    def foldRightMHelper(f: (a, b) -> Result[b, e] & f, s: b, xs: List[a], fk: e -> Result[b, e] & f, sk: b -> Result[b, e] & f): Result[b, e] & f = match xs {
        case Nil => sk(s)
        case x :: rs =>
            foldRightMHelper(f, s, rs, fk, s1 -> match f(x, s1) {
                    case Ok(s2) => sk(s2)
                    case Err(e) => fk(e)
                })
    }

    ///
    /// Returns a one-element list of the value `v` if `r` is `Ok(v)`. Otherwise returns the empty list.
    ///
    @Time(1) @Space(1)
    pub def toList(r: Result[t, e]): List[t] = match r {
        case Ok(v) => v :: Nil
        case Err(_) => Nil
    }

    ///
    /// Returns a one-element set of the value `v` if `r` is `Ok(v)`. Otherwise returns the empty set.
    ///
    @Time(1) @Space(1)
    pub def toSet(r: Result[t, e]): Set[t] = match r {
        case Ok(v)  => Set.singleton(v)
        case Err(_) => Set.empty()
    }

    ///
    /// Returns a singleton map with the mapping `k -> v` if `o` is `Ok((k, v))`. Otherwise returns the empty map.
    ///
    @Time(1) @Space(1)
    pub def toMap(r: Result[(k, v), e]): Map[k, v] = match r {
        case Ok((k, v)) => Map.singleton(k, v)
        case Err(_)     => Map.empty()
    }

    ///
    /// Returns `Some(v)` if `r` is `Ok(v)`. Otherwise returns `None`.
    ///
    @Time(1) @Space(1)
    pub def toOption(r: Result[t, e]): Option[t] = match r {
        case Ok(v)  => Some(v)
        case Err(_) => None
    }

    ///
    /// Applies `f` to `v` if `r` is `Ok(v)`. Otherwise does nothing.
    ///
    /// The function `f` must be impure.
    ///
    @Time(time(f)) @Space(space(f))
    pub def foreach(f: t ~> Unit, r: Result[t, e]): Unit & Impure = match r {
        case Ok(v)  => f(v)
        case Err(_) => ()
    }

    ///
    /// Applies the function in `r1` to the value in `r2`.
    ///
    @Time(foldLeft((_, f) -> time(f), 1, r1)) @Space(foldLeft((_, f) -> time(f), 1, r1))
    pub def ap(r1: Result[t -> u & f, e], r2: Result[t, e]): Result[u, e] & f = match r1 {
        case Ok(f)  => match r2 {
            case Ok(a)  => Ok(f(a))
            case Err(e) => Err(e)
        }
        case Err(e) => Err(e)
    }

    ///
    /// Applies the binary function `f` to the values in `r1` and `r2`.
    ///
    /// Returns the first `Err(e)` value if either of `r1` and `r2` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift2(f: (t1, t2) -> u & f, r1: Result[t1, e], r2: Result[t2, e]): Result[u, e] & f =
        ap(map(f, r1), r2)

    ///
    /// Applies the ternary function `f` to the values in `r1`, `r2` and `r3`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2` and `r3` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift3(f: (t1, t2, t3) -> u & f, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e]): Result[u, e] & f =
        ap(lift2(f, r1, r2), r3)

    ///
    /// Applies the 4-ary function `f` to the values in `r1`, `r2`, `r3` and `r4`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, `r3` and `r4` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift4(f: (t1, t2, t3, t4) -> u & f, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e]): Result[u, e] & f =
        ap(lift3(f, r1, r2, r3), r4)

    ///
    /// Applies the 5-ary function `f` to the values in `r1`, `r2`, ... `r5`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r5` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift5(f: (t1, t2, t3, t4, t5) -> u & f, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e], r5: Result[t5, e]): Result[u, e] & f =
        ap(lift4(f, r1, r2, r3, r4), r5)

    ///
    /// Applies the 6-ary function `f` to the values in `r1`, `r2`, ... `r6`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r6` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift6(f: (t1, t2, t3, t4, t5, t6) -> u & f, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e], r5: Result[t5, e], r6: Result[t6, e]): Result[u, e] & f =
        ap(lift5(f, r1, r2, r3, r4, r5), r6)

    ///
    /// Applies the 7-ary function `f` to the values in `r1`, `r2`, ... `r7`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r7` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift7(f: (t1, t2, t3, t4, t5, t6, t7) -> u & f, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e], r5: Result[t5, e], r6: Result[t6, e], r7: Result[t7, e]): Result[u, e] & f=
        ap(lift6(f, r1, r2, r3, r4, r5, r6), r7)

    ///
    /// Applies the 8-ary function `f` to the values in `r1`, `r2`, ... `r8`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r8` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift8(f: (t1, t2, t3, t4, t5, t6, t7, t8) -> u & f, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e], r5: Result[t5, e], r6: Result[t6, e], r7: Result[t7, e], r8: Result[t8, e]): Result[u, e] & f =
        ap(lift7(f, r1, r2, r3, r4, r5, r6, r7), r8)

    ///
    /// Applies the 9-ary function `f` to the values in `r1`, `r2`, ... `r9`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r9` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift9(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9) -> u & f, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e], r5: Result[t5, e], r6: Result[t6, e], r7: Result[t7, e], r8: Result[t8, e], r9: Result[t9, e]): Result[u, e] & f =
        ap(lift8(f, r1, r2, r3, r4, r5, r6, r7, r8), r9)

    ///
    /// Applies the 10-ary function `f` to the values in `r1`, `r2`, ... `r10`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r10` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift10(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) -> u & f, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e], r5: Result[t5, e], r6: Result[t6, e], r7: Result[t7, e], r8: Result[t8, e], r9: Result[t9, e], r10: Result[t10, e]): Result[u, e] & f =
        ap(lift9(f, r1, r2, r3, r4, r5, r6, r7, r8, r9), r10)

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