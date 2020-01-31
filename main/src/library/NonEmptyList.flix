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
/// The NonEmptyList type.
///

enum Nel[a] {
    case Nel(a, List[a])
}

namespace NonEmptyList {

    ///
    /// Returns the first element of `xs`.
    ///
    pub def head(xs: Nel[a]): a = match xs {
        case Nel(h, _) => h
    }

    ///
    /// Returns the last element of `xs`.
    ///
    pub def last(xs: Nel[a]): a = match xs {
        case Nel(h, l) => Option.getWithDefault(List.last(l), h)
    }

    ///
    /// Returns all elements in `xs` without the last element.
    ///
    // TODO: init.

    ///
    /// Returns all elements in `xs` without the first element.
    ///
    pub def tail(xs: Nel[a]): List[a] = match xs {
        case Nel(_, l) => l
    }

    ///
    /// Returns the length of `xs`.
    ///
    pub def length(xs: Nel[a]): Int = match xs {
        case Nel(_, l) => 1 + List.length(l)
    }

    ///
    /// Returns `ys` appended to `xs`.
    ///
    pub def append(xs: Nel[a], ys: Nel[a]): Nel[a] = match (xs, ys) {
        case (Nel(h1, l1), Nel(h2, l2)) => Nel(h1, l1 ::: (h2 :: l2))
    }

    ///
    /// Returns `true` if and only if `xs` contains the element `a`.
    ///
    pub def memberOf(a: a, xs: Nel[a]): Bool = match xs {
        case Nel(h, l) => h == a || List.memberOf(a, l)
    }

    // TODO: indexOf

    ///
    /// Alias for `findLeft`.
    ///
    pub def find(f: a -> Bool, xs: Nel[a]): Option[a] = findLeft(f, xs)

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from left to right.
    ///
    pub def findLeft(f: a -> Bool, xs: Nel[a]): Option[a] = match xs {
        case Nel(h, l) => if (f(h)) Some(h) else List.findLeft(f, l)
    }

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from right to left.
    ///
    pub def findRight(f: a -> Bool, xs: Nel[a]): Option[a] = match xs {
        case Nel(h, l) => match List.findRight(f, l) {
            case None    => if (f(h)) Some(h) else None
            case Some(x) => Some(x)
        }
    }

    // TODO: scan, scanLeft, scanRight

    ///
    /// Returns the result of applying `f` to every element in `xs`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    pub def map(f: a -> b, xs: Nel[a]): Nel[b] = match xs {
        case Nel(h, l) => Nel(f(h), List.map(f, l))
    }

    // TODO: mapWithIndex

    // TODO: flatMap

    // TODO: reverse

    // TODO: rotateLeft

    // TODO: rotateRight

    // TODO: Update

    ///
    /// Returns `xs` with every occurrence of `x` replaced by `y`.
    ///
    pub def replace(x: a, y: a, xs: Nel[a]): Nel[a] = map(e -> if (e == x) y else e, xs)

    // TODO: patch

    // TODO: permutations

    // TODO: removeIndex

    // TODO: subsequences

    // TODO: intersperse

    // TODO: intercalate

    // TODO: transpose

    // TODO: isPrefixOf

    // TODO: isInfixOf

    // TODO: isSuffixOf

    ///
    /// Alias for `foldLeft`.
    ///
    pub def fold(f: (b, a) -> b, s: b, xs: Nel[a]): b = foldLeft(f, s, xs)

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b, s: b, xs: Nel[a]): b = match xs {
        case Nel(h, l) => List.foldLeft(f, f(h, s), l)
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b, s: b, xs: Nel[a]): b = match xs {
        case Nel(h, l) => f(h, List.foldLeft(f, s, l))
    }

    ///
    /// Alias for `reduceLeft`.
    ///
    pub def reduce(f: (a, a) -> a, xs: Nel[a]): a = reduceLeft(f, xs)

    ///
    /// Applies `f` to all elements in `xs` going from left to right until a single value `v` is obtained.
    ///
    /// That is, the result is of the form: `f(...f(f(x1, x2), x3)..., xn)`
    ///
    pub def reduceLeft(f: (a, a) -> a, xs: Nel[a]): a = match xs {
        case Nel(h, l) => List.foldLeft(f, h, l)
    }

    ///
    /// Applies `f` to all elements in `xs` going from right to left until a single value `v` is obtained.
    ///
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    ///
    pub def reduceRight(f: (a, a) -> a, xs: Nel[a]): a = match xs {
        case Nel(h, l) => List.foldRight(f, h, l)
    }

    ///
    /// Returns the number of elements in `xs` that satisfy the predicate `f`.
    ///
    pub def count(f: a -> Bool, xs: Nel[a]): Int32 = match xs {
        case Nel(h, l) => (if (f(h)) 1 else 0) + List.count(f, l)
    }

    // TODO: Flatten



}

