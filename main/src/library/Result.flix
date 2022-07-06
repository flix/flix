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
pub enum Result[t, e] with Eq, Order, ToString {
    case Ok(t),
    case Err(e)
}

instance Hash[Result[t, e]] with Hash[t], Hash[e] {
    pub def hash(r: Result[t, e]): Int32 = match r {
        case Ok(v)  => 5381 + 113 * Hash.hash(v)
        case Err(v) => 5351 + 97 * Hash.hash(v)
    }
}

namespace Result {

    ///
    /// Returns `true` iff `r` is `Ok(v)`.
    ///
    @Time(1) @Space(1)
    pub def isOk(r: Result[t, e]): Bool = match r {
        case Ok(_)  => true
        case Err(_) => false
    }

    ///
    /// Returns `true` iff `r` is `Err(w)`.
    ///
    @Time(1) @Space(1)
    pub def isErr(r: Result[t, e]): Bool = match r {
        case Ok(_)  => false
        case Err(_) => true
    }

    ///
    /// Returns `v` if `r` is `Ok(v)`. Otherwise returns `d`.
    ///
    @Time(1) @Space(1)
    pub def getWithDefault(d: t, r: Result[t, e]): t = match r {
        case Ok(v)  => v
        case Err(_) => d
    }

    ///
    /// Returns `Ok(v)` if `r` is `Ok(v)`. Otherwise returns `default`.
    ///
    @Time(1) @Space(1)
    pub def withDefault(default: {default :: Result[t, e2]}, r: Result[t, e1]): Result[t, e2] = match r {
        case Ok(v)  => Ok(v)
        case Err(_) => default.default
    }

    ///
    /// Returns `Ok(to)` if `r` is `Ok(from)`. Otherwise returns `r`.
    ///
    @Time(1) @Space(1)
    pub def replace(from: {from :: t}, to: {to :: t}, r: Result[t, e]): Result[t, e] with Eq[t] = match r {
        case Ok(v)  => Ok(if (v == from.from) to.to else v)
        case Err(_) => r
    }

    ///
    /// Returns `true` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `false`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def exists(f: t -> Bool & ef, r: Result[t, e]): Bool & ef = match r {
        case Ok(t)  => f(t)
        case Err(_) => false
    }

    ///
    /// Returns `true` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true` or if `r` is `Err(w)`.
    /// Otherwise returns `false`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def forall(f: t -> Bool & ef, r: Result[t, e]): Bool & ef = match r {
        case Ok(t)  => f(t)
        case Err(_) => true
    }

    ///
    /// Returns `Ok(f(v))` if `r` is `Ok(v)`. Returns `Err(w)` if `r` is `Err(w)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def map(f: t1 -> t2 & ef, r: Result[t1, e]): Result[t2, e] & ef = match r {
        case Ok(v)  => Ok(f(v))
        case Err(w) => Err(w)
    }

    ///
    /// Returns `Err(f(e))` if `r` is `Err(e)`. Returns `Ok(v)` if `r` is `Ok(v)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def mapErr(f: e1 -> e2 & ef, r: Result[t, e1]): Result[t, e2] & ef = match r {
        case Ok(v)  => Ok(v)
        case Err(w) => Err(f(w))
    }


    ///
    /// Returns `f(v)` if `r` is `Ok(v)`. Returns `Err(w)` if `r` is `Err(w)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def flatMap(f: t1 -> Result[t2, e] & ef, r: Result[t1, e]): Result[t2, e] & ef = match r {
        case Ok(v)  => f(v)
        case Err(w) => Err(w)
    }

    ///
    /// Returns `1` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `0`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def count(f: t -> Bool & ef, r: Result[t, e]): Int32 & ef = match r {
        case Ok(v)  => if (f(v)) 1 else 0
        case Err(_) => 0
    }

    ///
    /// Returns `v` if `r` is `Ok(v)` else `0`.
    ///
    pub def sum(r: Result[Int32, e]): Int32 =
        foldLeft((acc, x) -> acc + x, 0, r)

    ///
    /// Returns `f(v)` if `r` is `Ok(v)` else `0`.
    ///
    pub def sumWith(f: t -> Int32 & ef, r: Result[t, e]): Int32 & ef =
        foldLeft((acc, x) -> acc + f(x), 0, r)

    ///
    /// Returns `v` if `r` is `Ok(v)` else `0`.
    ///
    pub def product(r: Result[Int32, e]): Int32 =
        if (isErr(r))
            0
        else
            foldLeft((acc, x) -> acc * x, 1, r)

    ///
    /// Returns `f(v)` if `r` is `Ok(v)` else `0`.
    ///
    pub def productWith(f: t -> Int32 & ef, r: Result[t, e]): Int32 & ef =
        if (isErr(r))
            0
        else
            foldLeft((acc, x) -> acc * f(x), 1, r)

    ///
    /// Returns `Some(v)` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `None`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f)) @Space(space(f))
    pub def find(f: t -> Bool, r: Result[t, e]): Option[t] = match r {
        case Ok(v)  => if (f(v)) Some(v) else None
        case Err(_) => None
    }

    ///
    /// Returns `f(z, v)` if `r` is `Ok(v)`. Otherwise returns `z`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def foldLeft(f: (a, t) -> a & ef, z: a, r: Result[t, e]): a & ef = match r {
        case Ok(v)  => f(z, v)
        case Err(_) => z
    }

    ///
    /// Returns `f(v, z)` if `r` is `Ok(v)`. Otherwise returns `z`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def foldRight(f: (t, a) -> a & ef, z: a, r: Result[t, e]): a & ef = match r {
        case Ok(v)  => f(v, z)
        case Err(_) => z
    }

    ///
    /// Returns `f(v, z)` if `r` is `Ok(v)`. Otherwise returns `z`.
    ///
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    @Time(time(f)) @Space(space(f))
    pub def foldRightWithCont(f: (t, Unit -> a & ef) -> a & ef, z: a, r: Result[t, e]): a & ef = match r {
        case Ok(v)  => f(v, _ -> z as & ef)
        case Err(_) => z
    }

    ///
    /// Returns `Ok(v1 :: v2 :: ... :: vn)` if each of `l_i` is `Ok(v_i)`.
    /// Otherwise returns the first `Err` encountered.
    ///
    pub def sequence(l: List[Result[a, e]]): Result[List[a], e] =
        def loop(ll, k) = match ll {
            case Nil         => k(Nil)
            case Err(e) :: _ => Err(e)
            case Ok(x) :: xs => loop(xs, ks -> k(x :: ks))
        };
        loop(l, ks -> Ok(ks))

    ///
    /// Returns `Some(v1 :: v2 :: ... v :: vn)` if each of `f(l_i)` is `Ok(v_i)`.
    /// Otherwise returns the first `Err` encountered.
    ///
    pub def traverse(f: a -> Result[b, e] & ef, l: List[a]): Result[List[b], e] & ef =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => match f(x) {
                case Ok(y)  => loop(xs, ks -> k(y :: ks))
                case Err(e) => Err(e)
            }
        };
        loop(l, ks -> Ok(ks))

    ///
    /// Returns `Ok()` if each of `f(l_i)` is `Ok(_)`. Otherwise returns `None`.
    ///
    /// This function is the "forgetful" version of `traverse`, use it when the you want the effect
    /// of applying `f` to each element but do not care about collecting the results.
    ///
    pub def traverseX(f: a -> Result[b, e] & ef, l: List[a]): Result[Unit, e] & ef = match l {
        case Nil     => Ok()
        case x :: xs => match f(x) {
            case Ok(_)  => traverseX(f, xs)
            case Err(e) => Err(e)
        }
    }

    ///
    /// Returns the result of applying `f` to a start value `s` and the elements in `l`
    /// going from left to right.
    ///
    /// If at any step applying `f` fails (i.e. it produces a `Err(e)` value) the traversal
    /// of `l` is short-circuited and `Err(e)` is returned.
    ///
    /// If `f` is successfully applied to all elements in `l` the result is of the form:
    /// `Ok(f(...f(f(s, x1), x2)..., xn))`.
    ///
    pub def foldLeftM(f: (b, a) -> Result[b, e] & ef, s: b, l: List[a]): Result[b, e] & ef = match l {
        case Nil     => Ok(s)
        case x :: xs => match f(s, x) {
            case Ok(s1) => foldLeftM(f, s1, xs)
            case Err(e) => Err(e)
        }
    }

    ///
    /// Returns the result of applying `f` to a start value `s` and the elements in `l`
    /// going from right to left.
    ///
    /// If at any step applying `f` fails (i.e. it produces a `Err(e)` value) the traversal
    /// of `l` is short-circuited and `Err(e)` is returned.
    ///
    /// If `f` is successfully applied to all elements in `l` the result is of the form:
    /// `Ok(f(x1, ...f(xn-1, f(xn, s))...))`.
    ///
    pub def foldRightM(f: (a, b) -> Result[b, e] & ef, s: b, l: List[a]): Result[b, e] & ef =
        def loop(ll, k) = match ll {
            case Nil     => k(s)
            case x :: xs => loop(xs, s1 -> match f(x, s1) {
                case Ok(s2) => k(s2)
                case Err(e) => Err(e)
            })
        };
        loop(l, s1 -> Ok(s1) as & ef)

    ///
    /// Returns a one-element list of the value `v` if `r` is `Ok(v)`. Otherwise returns the empty list.
    ///
    @Time(1) @Space(1)
    pub def toList(r: Result[t, e]): List[t] = match r {
        case Ok(v)  => v :: Nil
        case Err(_) => Nil
    }

    ///
    /// Returns a one-element set of the value `v` if `r` is `Ok(v)`. Otherwise returns the empty set.
    ///
    @Time(1) @Space(1)
    pub def toSet(r: Result[t, e]): Set[t] with Order[t] = match r {
        case Ok(v)  => Set.singleton(v)
        case Err(_) => Set.empty()
    }

    ///
    /// Returns a singleton map with the mapping `k -> v` if `o` is `Ok((k, v))`. Otherwise returns the empty map.
    ///
    @Time(1) @Space(1)
    pub def toMap(r: Result[(k, v), e]): Map[k, v] with Order[k] = match r {
        case Ok((k, v)) => Map.singleton(k, v)
        case Err(_)     => Map.empty()
    }

    ///
    /// Returns a map with elements of `s` as keys and `f` applied as values.
    ///
    pub def toMapWith(f: a -> b, s: Result[a, a]): Map[a, b] with Order[a] =
        Result.foldRight((x, acc) -> Map.insert(x, f(x), acc), Map.empty(), s)

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
    pub def foreach(f: t -> Unit & ef, r: Result[t, e]): Unit & ef = match r {
        case Ok(v)  => f(v)
        case Err(_) => ()
    }

    ///
    /// Applies the function in `r1` to the value in `r2`.
    ///
    @Time(foldLeft((_, f) -> time(f), 1, r1)) @Space(foldLeft((_, f) -> time(f), 1, r1))
    pub def ap(r1: Result[t -> u & ef, e], r2: Result[t, e]): Result[u, e] & ef = match r1 {
        case Err(e) => Err(e)
        case Ok(f)  => match r2 {
            case Ok(a)  => Ok(f(a))
            case Err(e) => Err(e)
        }
    }

    ///
    /// Applies the binary function `f` to the values in `r1` and `r2`.
    ///
    /// Returns the first `Err(e)` value if either of `r1` and `r2` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift2(f: (t1, t2) -> u & ef, r1: Result[t1, e], r2: Result[t2, e]): Result[u, e] & ef =
        ap(map(f, r1), r2)

    ///
    /// Applies the ternary function `f` to the values in `r1`, `r2` and `r3`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2` and `r3` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift3(f: (t1, t2, t3) -> u & ef, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e]): Result[u, e] & ef =
        ap(lift2(f, r1, r2), r3)

    ///
    /// Applies the 4-ary function `f` to the values in `r1`, `r2`, `r3` and `r4`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, `r3` and `r4` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift4(f: (t1, t2, t3, t4) -> u & ef, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e]): Result[u, e] & ef =
        ap(lift3(f, r1, r2, r3), r4)

    ///
    /// Applies the 5-ary function `f` to the values in `r1`, `r2`, ... `r5`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r5` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift5(f: (t1, t2, t3, t4, t5) -> u & ef, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e], r5: Result[t5, e]): Result[u, e] & ef =
        ap(lift4(f, r1, r2, r3, r4), r5)

    ///
    /// Applies the 6-ary function `f` to the values in `r1`, `r2`, ... `r6`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r6` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift6(f: (t1, t2, t3, t4, t5, t6) -> u & ef, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e], r5: Result[t5, e], r6: Result[t6, e]): Result[u, e] & ef =
        ap(lift5(f, r1, r2, r3, r4, r5), r6)

    ///
    /// Applies the 7-ary function `f` to the values in `r1`, `r2`, ... `r7`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r7` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift7(f: (t1, t2, t3, t4, t5, t6, t7) -> u & ef, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e], r5: Result[t5, e], r6: Result[t6, e], r7: Result[t7, e]): Result[u, e] & ef=
        ap(lift6(f, r1, r2, r3, r4, r5, r6), r7)

    ///
    /// Applies the 8-ary function `f` to the values in `r1`, `r2`, ... `r8`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r8` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift8(f: (t1, t2, t3, t4, t5, t6, t7, t8) -> u & ef, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e], r5: Result[t5, e], r6: Result[t6, e], r7: Result[t7, e], r8: Result[t8, e]): Result[u, e] & ef =
        ap(lift7(f, r1, r2, r3, r4, r5, r6, r7), r8)

    ///
    /// Applies the 9-ary function `f` to the values in `r1`, `r2`, ... `r9`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r9` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift9(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9) -> u & ef, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e], r5: Result[t5, e], r6: Result[t6, e], r7: Result[t7, e], r8: Result[t8, e], r9: Result[t9, e]): Result[u, e] & ef =
        ap(lift8(f, r1, r2, r3, r4, r5, r6, r7, r8), r9)

    ///
    /// Applies the 10-ary function `f` to the values in `r1`, `r2`, ... `r10`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r10` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift10(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) -> u & ef, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e], r5: Result[t5, e], r6: Result[t6, e], r7: Result[t7, e], r8: Result[t8, e], r9: Result[t9, e], r10: Result[t10, e]): Result[u, e] & ef =
        ap(lift9(f, r1, r2, r3, r4, r5, r6, r7, r8, r9), r10)

    ///
    /// Returns an iterator over `r` with 1 element or an empty iterator if `r` is `Err`.
    ///
    pub def toIterator(reg: Region[reg], r: Result[t, e]): Iterator[t, reg] \ Write(reg) = match r {
        case Err(_) => new Iterator(reg)
        case Ok(x)  => Iterator.singleton(reg, x)
    }

    ///
    /// Returns an iterator over `r` zipped with the indices of the elements.
    ///
    pub def enumerator(reg: Region[reg], r: Result[t, e]): Iterator[(t, Int32), reg] \ Write(reg) =
        toIterator(reg, r) |> Iterator.zipWithIndex

    ///
    /// Returns `Ok(x)` if `f` was invoked without throwing an exception.
    ///
    /// If `f` throws a Java `RuntimeException`, `Err(e)` is returned
    /// where `e` is the error message.
    ///
    pub def try(f: Unit -> a & ef): Result[a, String] & ef =
        try {
            Ok(f())
        } catch {
            case e: ##java.lang.RuntimeException =>
                import java.lang.Throwable.getMessage(): String & Pure;
                Err(getMessage(e))
        }

}
