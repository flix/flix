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




    pub def map(f: a -> b, xs: Nel[a]): Nel[b] = match xs {
        case Nel(h, l) => Nel(f(h), List.map(f, l))
    }

}

