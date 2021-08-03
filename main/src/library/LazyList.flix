/*
 * Copyright 2021 Jakob Schneider Villumsen
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

pub enum LazyList[a] {
    case ENil,
    case ECons(a, LazyList[a]),
    case LCons(a, Lazy[LazyList[a]]),
    case LList(Lazy[LazyList[a]])
}

namespace LazyList {
    // TODO: Implement functions used in TestExamples

    ///
    /// Returns an empty LazyList.
    ///
    pub def empty(): LazyList[a] = LList(lazy ENil)

    ///
    /// Returns true if and only if `l` is the empty LazyList, i.e. `ENil`.
    ///
    /// Does not force the tail of `l`.
    ///
    pub def isEmpty(l: LazyList[a]): Bool = match l {
        case ENil      => true
        case LList(ys) => isEmpty(force ys)
        case _         => false
    }

    ///
    /// Returns a lazy list of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty lazy list if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): LazyList[Int32] =
        if (b >= e)
            empty()
        else
            LList(lazy LCons(b, lazy range(b + 1, e)))

    ///
    /// Returns `Some(x)` if `x` is the first element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Does not force the tail of `l`.
    ///
    pub def head(l: LazyList[a]): Option[a] = match l {
        case ENil        => None
        case ECons(x, _) => Some(x)
        case LCons(x, _) => Some(x)
        case LList(ys)   => head(force ys)
    }

    ///
    /// Returns the first `n` elements of `l`.
    ///
    /// Does not force the tail of `l`.
    ///
    pub def take(n: Int32, l: LazyList[a]): LazyList[a] =
        if (n <= 0)
            empty()
        else
            match l {
                case ENil                     => ENil
                case ECons(x, _)  if (n == 1) => LCons(x, lazy ENil)
                case ECons(x, ys)             => LCons(x, lazy take(n - 1, ys))
                case LCons(x, _)  if (n == 1) => LCons(x, lazy ENil)
                case LCons(x, ys)             => LCons(x, lazy take(n - 1, force ys))
                case LList(ys)                => LList(lazy take(n, force ys))
            }

    ///
    /// Returns the length of `l`.
    ///
    /// Forces the entire list `l`.
    ///
    pub def length(l: LazyList[a]): Int32 = lengthAcc(0, l)

    ///
    /// Helper function for `length`
    ///
    def lengthAcc(acc: Int32, l: LazyList[a]): Int32 = match l {
        case ENil         => acc
        case ECons(_, ys) => lengthAcc(acc + 1, ys)
        case LCons(_, ys) => lengthAcc(acc + 1, force ys)
        case LList(ys)    => lengthAcc(acc, force ys)
    }

    ///
    /// Returns the elements of `l` in a lazy list.
    ///
    // TODO: Move to List.
    pub def toLazy(l: List[a]): LazyList[a] = match l {
        case Nil     => ENil
        case x :: xs => LCons(x, lazy toLazy(xs))
    }

    ///
    /// Returns `l` as a `List`.
    ///
    /// Forces the entire list `l`.
    ///
    pub def toList(l: LazyList[a]): List[a] = toListAcc(l, Nil) |> List.reverse

    ///
    /// Helper function for `toList`.
    ///
    def toListAcc(l: LazyList[a], acc: List[a]): List[a] = match l {
        case ENil         => acc
        case ECons(x, ys) => toListAcc(ys, x :: acc)
        case LCons(x, ys) => toListAcc(force ys, x :: acc)
        case LList(ys)    => toListAcc(force ys, acc)
    }
}