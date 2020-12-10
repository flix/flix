/*
 * Copyright 2020 Esben Bjerre
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

namespace LazyList {

    ///
    /// An immutable lazy list.
    ///
    /// A lazy list is either the empty list represented by `Empty`, or
    /// an element `x` followed by a lazy list `xs`.
    /// Note that a lazy list is lazy on its tail but strict on its head.
    /// The empty list is always strict.
    ///
    pub enum LazyList[t] {
        case Empty,
        case LazyCons(t, Lazy[LazyList[t]])
    }

    ///
    /// Returns the empty LazyList.
    ///
    pub def empty(): LazyList[a] = Empty

    ///
    /// Returns an infinite LazyList of repeating `x`s.
    ///
    pub def repeat(x: a): LazyList[a] = LazyCons(x, lazy repeat(x))

    ///
    /// Returns an infinite sequence of integers starting from and including `n`.
    ///
    pub def from(n: Int32): LazyList[Int32] = LazyCons(n, lazy from(n + 1))

    ///
    /// Returns true if and only if `xs` is the empty LazyList, i.e. `Empty`.
    ///
    /// Does not force the tail of the list `xs`.
    ///
    pub def isEmpty(xs: LazyList[a]): Bool = match xs {
        case Empty => true
        case _ => false
    }

    ///
    /// Returns the length of `xs`.
    ///
    /// Forces the entire list `xs`.
    ///
    pub def length(xs: LazyList[a]): Int32 = lengthHelper(xs, 0)

    def lengthHelper(xs: LazyList[a], acc: Int32): Int32 = match xs {
        case Empty => acc
        case LazyCons(_, t) => lengthHelper(force t, acc + 1)
    }

    ///
    /// Returns `Some(x)` if `x` is the first element of `xs`.
    ///
    /// Returns `None` if `xs` is empty.
    ///
    /// Does not force the tail of the list `xs`.
    ///
    pub def head(xs: LazyList[a]): Option[a] = match xs {
        case Empty => None
        case LazyCons(h, _) => Some(h)
    }

    ///
    /// Returns `Some(x)` if `x` is the last element of `xs`.
    ///
    /// Returns `None` if `xs` is empty.
    ///
    /// Forces the entire list `xs`.
    ///
    pub def last(xs: LazyList[a]): Option[a] = match xs {
        case Empty => None
        case LazyCons(h, t) => {
            let tail = force t;
            if (isEmpty(tail)) Some(h) else last(tail)
        }
    }

    ///
    /// Returns `Some(x)` if `x` is the `n`th element of `xs`.
    ///
    /// Returns `None` if `xs` is empty or `n` >= `length(xs)`.
    ///
    /// Forces the first `n` elements of `xs`.
    ///
    pub def nth(n: Int32, xs: LazyList[a]): Option[a] = match xs {
        case Empty => None
        case LazyCons(h, t) => if (n == 0) Some(h) else nth(n - 1, force t)
    }

    ///
    /// Returns `ys` appended to `xs`.
    ///
    /// Forces the entire list `xs` (but does not force `ys`).
    ///
    pub def append(xs: LazyList[a], ys: LazyList[a]): LazyList[a] = match xs {
        case Empty => ys
        case LazyCons(h, t) => LazyCons(h, lazy append(force t, ys))
    }

    ///
    /// Returns the reverse of `xs`.
    ///
    /// Forces the entire list `xs`.
    ///
    pub def reverse(xs: LazyList[a]): LazyList[a] = reverseHelper(xs, Empty)

    def reverseHelper(xs: LazyList[a], acc: LazyList[a]): LazyList[a] = match xs {
        case Empty => acc
        case LazyCons(h, t) => reverseHelper(force t, LazyCons(h, lazy acc))
    }

    ///
    /// Returns the first `n` elements of `xs`.
    ///
    /// Forces the first `n` elements of `xs`.
    ///
    pub def take(n: Int32, xs: LazyList[a]): LazyList[a] = match xs {
        case Empty => Empty
        case LazyCons(h, t) =>
            if (n == 0) Empty
            else if (n == 1) LazyCons(h, lazy Empty)
            else LazyCons(h, lazy take(n - 1, force t))
    }

    ///
    /// Returns `xs` without the first `n` elements.
    ///
    /// Forces the first `n` elements of `xs`.
    ///
    pub def drop(n: Int32, xs: LazyList[a]): LazyList[a] = match xs {
        case Empty => xs
        case LazyCons(_, t) => if (n == 0) xs else drop(n - 1, force t)
    }

    ///
    /// Returns `x` without the first element.
    ///
    /// Forces the first element of `xs`.
    ///
    pub def tail(xs: LazyList[a]): LazyList[a] = drop(1, xs)

    ///
    /// Returns `xs` as a `List`.
    ///
    /// Forces the entire list `xs`.
    ///
    pub def toList(xs: LazyList[a]): List[a] =
        toListHelper(xs, Nil)

    pub def toListHelper(xs: LazyList[a], acc: List[a]): List[a] = match xs {
        case Empty => List.reverse(acc)
        case LazyCons(h, t) => toListHelper(force t, h :: acc)
    }

    ///
    /// Alias for `foldLeft`.
    ///
    /// Forces the entire list `xs`.
    ///
    pub def fold(f: (b, a) -> b, s: b, xs: LazyList[a]): b = foldLeft(f, s, xs)

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    /// Forces the entire list `xs`.
    ///
    pub def foldLeft(f: (b, a) -> b, s: b, xs: LazyList[a]): b = match xs {
        case Empty => s
        case LazyCons(h, t) => foldLeft(f, f(s, h), force t)
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    /// Forces the entire list `xs`.
    ///
    pub def foldRight(f: (a, Lazy[b]) -> b, s: b, xs: LazyList[a]): b = match xs {
        case Empty => s
        case LazyCons(h, t) => f(h, lazy foldRight(f, s, force t))
    }

    ///
    /// Returns `true` if and only if at least one element in `xs` satisfies the predicate `f`.
    ///
    /// Returns `false` if `xs` is empty.
    ///
    /// Forces the list `xs` until an element that satisfies the predicate `f` is found.
    ///
    pub def exists(f: a -> Bool, xs: LazyList[a]): Bool = match xs {
        case Empty => false
        case LazyCons(h, t) => f(h) or exists(f, force t)
    }

    ///
    /// Returns `true` if and only if all elements in `xs` satisfy the predicate `f`.
    ///
    /// Returns `true` if `xs` is empty.
    ///
    /// Forces the list `xs` until an element that does not satisfy the predicate `f` is found.
    ///
    pub def forall(f: a -> Bool, xs: LazyList[a]): Bool = match xs {
        case Empty => true
        case LazyCons(h, t) => f(h) and forall(f, force t)
    }

    ///
    /// Returns the result of applying `f` to every element in `xs`.
    ///
    /// Does not force the list `xs`.
    ///
    pub def map(f: a -> b, xs: LazyList[a]): LazyList[b] = match xs {
        case Empty => Empty
        case LazyCons(h, t) => LazyCons(f(h), lazy map(f, force t))
    }

    ///
    /// Returns a lazy list of every element in `xs` that satisfies the predicate `f`.
    ///
    /// Does not force the list `xs`.
    ///
    pub def filter(f: a -> Bool, xs: LazyList[a]): LazyList[a] = match xs {
        case Empty => Empty
        case LazyCons(h, t) => if (f(h)) LazyCons(h, lazy filter(f, force t)) else filter(f, force t)
    }

    ///
    /// Build a lazy list by applying `f` to the seed value `s`.
    ///
    /// `f` should return `Some(a, s1)` to signal a new list element `a` and a new seed value `s1`.
    /// `f` should return `None` to signal the end of building the list.
    ///
    pub def unfold(f: b -> Option[(a, b)], s: b): LazyList[a] = match f(s) {
        case None => Empty
        case Some((h, t)) => LazyCons(h, lazy unfold(f, t))
    }

    ///
    /// Returns a list where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `xs` and `b` is the element at index `i` in `ys`.
    ///
    /// If either `xs` or `ys` becomes depleted, then no further elements are added to the resulting list.
    ///
    /// Does not force the lists `xs` and `ys`.
    ///
    pub def zipWith(f: (a, b) -> c, xs: LazyList[a], ys: LazyList[b]): LazyList[c] = match (xs, ys) {
        case (LazyCons(h1, t1), LazyCons(h2, t2)) => LazyCons(f(h1, h2), lazy zipWith(f, force t1, force t2))
        case _ => Empty
    }

    ///
    /// Returns a list where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `xs` and `b` is the element at index `i` in `ys`.
    ///
    /// If either `xs` or `ys` becomes depleted, then no further elements are added to the resulting list.
    ///
    /// Does not force the lists `xs` and `ys`.
    ///
    pub def zip(xs: LazyList[a], ys: LazyList[b]): LazyList[(a, b)] = zipWith((x, y) -> (x, y), xs, ys)

}