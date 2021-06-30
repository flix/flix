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

pub enum Nel[a] {
    case Nel(a, List[a])
}

instance Boxable[Nel[a]] with Order[a], ToString[a] {
    pub def box(x: Nel[a]): Boxed = {
        let value = x as ##java.lang.Object;
        let compare = (o1, o2) -> Order.compare(o1 as Nel[a], o2 as Nel[a]);
        let toString = o -> ToString.toString(o as Nel[a]);
        BoxedObject(value, compare, toString)
    }
    pub def unbox(x: Boxed): Nel[a] = match x {
        case BoxedObject(v, _, _) => v as Nel[a]
        case _                    => ?bug
    }
}

instance ToString[Nel[a]] with ToString[a] {
    pub def toString(l: Nel[a]): String = match l {
        case Nel(x, xs) => "Nel(${x}, ${xs})"
    }
}

instance Hash[Nel[a]] with Hash[a] {
    pub def hash(l: Nel[a]): Int = match l {
        case Nel(x, xs) => 39119 + Hash.hash(xs) + Hash.hash(x)
    }
}

instance Eq[Nel[a]] with Eq[a] {
    pub def eq(xs: Nel[a], ys: Nel[a]): Bool = match (xs, ys) {
        case (Nel(x, l1), Nel(y, l2)) => x == y and l1 == l2
    }
}

instance Order[Nel[a]] with Order[a] {
    ///
    /// Compares `xs` and `ys` lexicographically.
    ///
    pub def compare(xs: Nel[a], ys: Nel[a]): Comparison = match (xs, ys) {
        case (Nel(x, l1), Nel(y, l2)) =>
            let cmp = x <=> y;
            if (cmp == EqualTo) l1 <=> l2 else cmp
    }
}

instance Functor[Nel] {
    pub def map(f: a -> b & e, xs: Nel[a]): Nel[b] & e = Nel.map(f, xs)
}

instance Foldable[Nel] {
    pub def foldLeft(f: (b, a) -> b & e, s: b, xs: Nel[a]): b & e = Nel.foldLeft(f, s, xs)
    pub def foldRight(f: (a, b) -> b & e, s: b, xs: Nel[a]): b & e = Nel.foldRight(f, s, xs)
}

namespace Nel {

    ///
    /// Returns a new non-empty list containing the single element `x`.
    ///
    @Time(1) @Space(1)
    pub def singleton(x: a): Nel[a] = Nel(x, Nil)

    ///
    /// Returns the non-empty list `xs` prefixed with the new element `x`.
    ///
    @Time(1) @Space(1)
    pub def cons(x: a, xs: Nel[a]): Nel[a] = match xs {
        case Nel(y, ys) => Nel(x, y :: ys)
    }

    ///
    /// Returns the first element of `xs`.
    ///
    @Time(1) @Space(1)
    pub def head(xs: Nel[a]): a = match xs {
        case Nel(h, _) => h
    }

    ///
    /// Returns the last element of `xs`.
    ///
    @Time(length(xs)) @Space(1)
    pub def last(xs: Nel[a]): a = match xs {
        case Nel(h, l) => Option.getWithDefault(List.last(l), h)
    }

    ///
    /// Returns all elements in `xs` without the last element.
    ///
    @Time(length(xs)) @Space(length(xs))
    pub def init(xs: Nel[a]): List[a] = match xs {
        case Nel(_, Nil) => Nil
        case Nel(h, l)   => match List.reverse(l) {
            case Nil     => h :: Nil
            case _ :: xs => h :: List.reverse(xs)
        }
    }

    ///
    /// Returns all elements in `xs` without the first element.
    ///
    @Time(1) @Space(1)
    pub def tail(xs: Nel[a]): List[a] = match xs {
        case Nel(_, l) => l
    }

    ///
    /// Returns the length of `xs`.
    ///
    @Time(length(xs)) @Space(1)
    pub def length(xs: Nel[a]): Int = match xs {
        case Nel(_, l) => 1 + List.length(l)
    }

    ///
    /// Returns `ys` appended to `xs`.
    ///
    @Time(length(xs)) @Space(length(xs))
    pub def append(xs: Nel[a], ys: Nel[a]): Nel[a] = match (xs, ys) {
        case (Nel(h1, l1), Nel(h2, l2)) => Nel(h1, l1 ::: (h2 :: l2))
    }

    ///
    /// Returns `true` if and only if `xs` contains the element `a`.
    ///
    @Time(length(xs)) @Space(1)
    pub def memberOf(a: a, xs: Nel[a]): Bool with Eq[a] = match xs {
        case Nel(h, l) => h == a or List.memberOf(a, l)
    }

    ///
    /// Finds the smallest element of `xs` according to the `Order` on `a`.
    ///
    pub def minimum(xs: Nel[a]): a with Order[a] =
        reduceLeft(Order.min, xs)

    ///
    /// Finds the smallest element of `xs` according to the given comparator `cmp`.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, xs: Nel[a]): a =
        reduceLeft(Order.minBy(cmp), xs)

    ///
    /// Finds the largest element of `xs` according to the `Order` on `a`.
    ///
    pub def maximum(xs: Nel[a]): a with Order[a] =
        reduceLeft(Order.max, xs)

    ///
    /// Finds the largest element of `xs` according to the given comparator `cmp`.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, xs: Nel[a]): a =
        reduceLeft(Order.maxBy(cmp), xs)

    ///
    /// Alias for `findLeft`.
    ///
    @Time(time(f) * length(xs)) @Space(space(f))
    pub def find(f: a -> Bool, xs: Nel[a]): Option[a] = findLeft(f, xs)

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from left to right.
    ///
    @Time(time(f) * length(xs)) @Space(space(f))
    pub def findLeft(f: a -> Bool, xs: Nel[a]): Option[a] = match xs {
        case Nel(h, l) => if (f(h)) Some(h) else List.findLeft(f, l)
    }

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from right to left.
    ///
    @Time(time(f) * length(xs)) @Space(space(f))
    pub def findRight(f: a -> Bool, xs: Nel[a]): Option[a] = match xs {
        case Nel(h, l) => match List.findRight(f, l) {
            case None    => if (f(h)) Some(h) else None
            case Some(x) => Some(x)
        }
    }

    ///
    /// Returns the result of applying `f` to every element in `xs`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    @Time(time(f) * length(xs)) @Space(space(f) * length(xs))
    pub def map(f: a -> b & e, xs: Nel[a]): Nel[b] & e = match xs {
        case Nel(h, l) => Nel(f(h), List.map(f, l))
    }

    ///
    /// Returns the result of applying `f` to every element in `xs` along with that element's index.
    ///
    /// That is, the result is of the form: `f(x1, 0) :: f(x2, 1) :: ...`.
    ///
    @Time(time(f) * length(xs)) @Space(space(f) * length(xs))
    pub def mapWithIndex(f: (a, Int32) -> b & e, xs: Nel[a]): Nel[b] & e = match xs {
        case Nel(h, l) => Nel(f(h, 0), mapWithIndexHelper(f, l, 1))
    }

    ///
    /// Helper function for `mapWithIndex`.
    ///
    def mapWithIndexHelper(f: (a, Int32) -> b & e, xs: List[a], i: Int32): List[b] & e = match xs {
        case Nil     => Nil
        case x :: rs => f(x, i) :: mapWithIndexHelper(f, rs, i + 1)
    }

    ///
    /// Returns the result of applying `f` to every element in `xs` and concatenating the results.
    ///
    @Time(time(f) * length(xs)) @Space(time(f) * length(xs))
    pub def flatMap(f: a -> Nel[b] & e, xs: Nel[a]): Nel[b] & e = match xs {
        case Nel(h, l) => match f(h) {
            case Nel(h1, l1) => Nel(h1, l1 ::: List.flatMap(x -> toList(f(x)), l))
        }
    }

    ///
    /// Returns the reverse of `xs`.
    ///
    @Time(length(xs)) @Space(length(xs))
    pub def reverse(xs: Nel[a]): Nel[a] = match xs {
        case Nel(h, l) => match List.reverse(h :: l) {
            case y :: ys => Nel(y, ys)
            case _       => unreachable!()
        }
    }

    ///
    /// Returns `xs` with every occurrence of `x` replaced by `y`.
    ///
    @Time(length(xs)) @Space(length(xs))
    pub def replace(x: a, y: a, xs: Nel[a]): Nel[a] with Eq[a] = map(e -> if (e == x) y else e, xs)

    ///
    /// Returns all permutations of `xs` in lexicographical order by element indices in `xs`.
    ///
    /// That is, `xs` is the first permutation and `reverse(xs)` is the last permutation.
    ///
    @Time(Int32.factorial(length(xs))) @Space(Int32.factorial(length(xs)))
    pub def permutations(xs: Nel[a]): Nel[List[a]] = match xs {
        case Nel(h, l) => match List.permutations(h :: l) {
            case y :: ys => Nel(y, ys)
            case Nil => unreachable!()
        }
    }

    ///
    /// Returns all subsequences of `xs` in lexicographical order by element indices in `xs`.
    ///
    /// That is, `xs` is the first subsequence and `Nil` is the last subsequence.
    ///
    @Time(length(xs) * length(xs)) @Space(length(xs) * length(xs))
    pub def subsequences(xs: Nel[a]): Nel[List[a]] = match xs {
        case Nel(h, l) => match List.subsequences(h :: l) {
            case y :: ys => Nel(y, ys)
            case Nil => unreachable!()
        }
    }

    ///
    /// Returns `xs` with `a` inserted between every two adjacent elements.
    ///
    @Time(length(xs)) @Space(length(xs))
    pub def intersperse(a: a, xs: Nel[a]): Nel[a] = match xs {
        case Nel(h, Nil) => Nel(h, Nil)
        case Nel(h, l)   => Nel(h, a :: List.intersperse(a, l))
    }

    ///
    /// Returns the result of applying `combine` to all the elements in `xs`, using `empty` as the initial value.
    ///
    pub def fold(xs: Nel[a]): a with Monoid[a] = Foldable.fold(xs)

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & e, s: b, xs: Nel[a]): b & e = match xs {
        case Nel(h, l) => List.foldLeft(f, f(s, h), l)
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b & e, s: b, xs: Nel[a]): b & e = match xs {
        case Nel(h, l) => f(h, List.foldRight(f, s, l))
    }

    ///
    /// Applies `combine` to all elements in `xs` until a single value is obtained.
    ///
    pub def reduce(xs: Nel[a]): a with Semigroup[a] = match xs {
        case Nel(h, l) => Foldable.foldLeft((acc, x) -> Semigroup.combine(acc, x), h, l)
    }

    ///
    /// Applies `f` to all elements in `xs` going from left to right until a single value `v` is obtained.
    ///
    /// That is, the result is of the form: `f(...f(f(x1, x2), x3)..., xn)`
    ///
    pub def reduceLeft(f: (a, a) -> a & e, xs: Nel[a]): a & e = match xs {
        case Nel(h, l) => List.foldLeft(f, h, l)
    }

    ///
    /// Applies `f` to all elements in `xs` going from right to left until a single value `v` is obtained.
    ///
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    ///
    pub def reduceRight(f: (a, a) -> a & e, xs: Nel[a]): a & e = match xs {
        case Nel(h, l) => match List.reduceRight(f, h :: l) {
            case None    => unreachable!()
            case Some(v) => v
        }
    }

    ///
    /// Returns the number of elements in `xs` that satisfy the predicate `f`.
    ///
    @Time(time(f) * length(xs)) @Space(space(f) * length(xs))
    pub def count(f: a -> Bool, xs: Nel[a]): Int32 = match xs {
        case Nel(h, l) => (if (f(h)) 1 else 0) + List.count(f, l)
    }

    ///
    /// Returns the concatenation of the elements in `xs`.
    ///
    @Time(length(xs)) @Space(length(xs))
    pub def flatten(xs: Nel[Nel[a]]): Nel[a] = match xs {
        case Nel(Nel(h, ys), zs) => Nel(h, ys ::: List.flatMap(toList, zs))
    }

    ///
    /// Returns `true` if and only if at least one element in `xs` satisfies the predicate `f`.
    ///
    @Time(time(f) * length(xs)) @Space(space(f))
    pub def exists(f: a -> Bool, xs: Nel[a]): Bool = match xs {
        case Nel(h, l) => f(h) or List.exists(f, l)
    }

    ///
    /// Returns `true` if and only if all elements in `xs` satisfy the predicate `f`.
    ///
    @Time(time(f) * length(xs)) @Space(space(f))
    pub def forall(f: a -> Bool, xs: Nel[a]): Bool = match xs {
        case Nel(h, l) => f(h) and List.forall(f, l)
    }

    ///
    /// Returns a list of every element in `xs` that satisfies the predicate `f`.
    ///
    @Time(time(f) * length(xs)) @Space(space(f) * length(xs))
    pub def filter(f: a -> Bool, xs: Nel[a]): List[a] = match xs {
        case Nel(h, l) =>
            if (f(h))
                h :: List.filter(f, l)
            else
                List.filter(f, l)
    }

    ///
    /// Returns a non-empty list where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `xs` and `b` is the element at index `i` in `ys`.
    ///
    /// If either `xs` or `ys` becomes depleted, then no further elements are added to the resulting list.
    ///
    @Time(Int32.min(length(xs), length(ys))) @Space(Int32.min(length(xs), length(ys)))
    pub def zip(xs: Nel[a], ys: Nel[b]): Nel[(a,b)] = match (xs, ys) {
        case (Nel(h1, l1), Nel(h2, l2)) => Nel((h1, h2), List.zip(l1, l2))
    }

    ///
    /// Returns a non-empty list where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `xs` and `b` is the element at index `i` in `ys`.
    ///
    /// If either `xs` or `ys` becomes depleted, then no further elements are added to the resulting list.
    ///
    @Time(time(f) * Int32.min(length(xs), length(ys))) @Space(space(f) * Int32.min(length(xs), length(ys)))
    pub def zipWith(f: (a, b) -> c, xs: Nel[a], ys: Nel[b]): Nel[c] = match (xs, ys) {
        case (Nel(h1, l1), Nel(h2, l2)) => Nel(f(h1, h2), List.zipWith(f, l1, l2))
    }

    ///
    /// Returns a pair of non-empty lists, the first containing all first components in `xs`
    /// and the second containing all second components in `xs`.
    ///
    @Time(length(xs)) @Space(length(xs))
    pub def unzip(xs: Nel[(a, b)]): (Nel[a], Nel[b]) = match xs {
        case Nel((a, b), l) =>
            let (l1, l2) = List.unzip(l);
            (Nel(a, l1), Nel(b, l2))
    }

    ///
    /// Returns `xs` as a normal list.
    ///
    @Time(1) @Space(1)
    pub def toList(xs: Nel[a]): List[a] = match xs {
        case Nel(h, l) => h :: l
    }

    ///
    /// Applies `f` to every element of `xs`.
    ///
    /// The function `f` must be impure.
    ///
    @Time(time(f)) @Space(space(f))
    pub def foreach(f: a -> Unit, xs: Nel[a]): Unit & ef = match xs {
        case Nel(h, l) => f(h); List.foreach(f, l)
    }

    ///
    /// Sort the non-empty list `xs` so that elements are ordered from low to high according
    /// to their `Order` instance.
    ///
    /// The sort is stable, i.e., equal elements appear in the same order as in the input `xs`.
    ///
    /// The sort implementation is a mergesort.
    ///
    pub def sort(xs: Nel[a]): Nel[a] with Order[a] = match xs {
        case Nel(_, Nil) => xs
        case Nel(h, l) => match List.sort(h :: l) {
            case y :: ys => Nel(y, ys)
            case _       => unreachable!()
        }
    }

    ///
    /// Sort the non-empty list `xs` so that elements are ordered from low to high according
    /// to the `Order` instance for the values obtained by applying `f` to each element.
    ///
    /// The sort is stable, i.e., equal elements appear in the same order as in the input `xs`.
    ///
    /// The sort implementation is a mergesort.
    ///
    pub def sortBy(f: a -> b, xs: Nel[a]): Nel[a] with Order[b] = match xs {
        case Nel(_, Nil) => xs
        case Nel(h, l) => match List.sortBy(f, h :: l) {
            case y :: ys => Nel(y, ys)
            case _       => unreachable!()
        }
    }

    ///
    /// Sort the non-empty list `xs` so that elements are ordered from low to high according
    /// to the comparison function `cmp`.
    ///
    /// The sort is stable, i.e., equal elements appear in the same order as in the input `xs`.
    ///
    /// The sort implementation is a mergesort.
    ///
    pub def sortWith(cmp: (a,a) -> Comparison, xs: Nel[a]): Nel[a] = match xs {
        case Nel(_, Nil) => xs
        case Nel(h, l) => match List.sortWith(cmp, h :: l) {
            case y :: ys => Nel(y, ys)
            case _       => unreachable!()
        }
    }
}

