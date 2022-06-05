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

instance Boxable[Nel[a]] with Boxable[a]

instance ToString[Nel[a]] with ToString[a] {
    pub def toString(l: Nel[a]): String = match l {
        case Nel(x, xs) => "Nel(${x}, ${xs})"
    }
}

instance Hash[Nel[a]] with Hash[a] {
    pub def hash(l: Nel[a]): Int32 = match l {
        case Nel(x, xs) => 39119 + Hash.hash(xs) + Hash.hash(x)
    }
}

instance Eq[Nel[a]] with Eq[a] {
    pub def eq(l1: Nel[a], l2: Nel[a]): Bool = match (l1, l2) {
        case (Nel(x, xs), Nel(y, ys)) => x == y and xs == ys
    }
}

instance Order[Nel[a]] with Order[a] {
    ///
    /// Compares `l1` and `l2` lexicographically.
    ///
    pub def compare(l1: Nel[a], l2: Nel[a]): Comparison = match (l1, l2) {
        case (Nel(x, xs), Nel(y, ys)) =>
            let cmp = x <=> y;
            if (cmp == EqualTo) xs <=> ys else cmp
    }
}

instance Functor[Nel] {
    pub def map(f: a -> b & ef, l: Nel[a]): Nel[b] & ef = Nel.map(f, l)
}

instance Applicative[Nel] {
    pub def point(x: a): Nel[a] = Nel.singleton(x)
    pub def ap(f: Nel[a -> b & ef], x: Nel[a]): Nel[b] & ef = Nel.ap(f, x)
}

instance Monad[Nel] {
    pub def flatMap(f: a -> Nel[b] & ef, x : Nel[a]) : Nel[b] & ef = Nel.flatMap(f, x)
}

instance Foldable[Nel] {
    pub def foldLeft(f: (b, a) -> b & ef, s: b, l: Nel[a]): b & ef = Nel.foldLeft(f, s, l)
    pub def foldRight(f: (a, b) -> b & ef, s: b, l: Nel[a]): b & ef = Nel.foldRight(f, s, l)
    pub def foldRightWithCont(f: (a, Unit -> b & ef) -> b & ef, s: b, l: Nel[a]): b & ef = Nel.foldRightWithCont(f, s, l)
}

instance Traversable[Nel] {
    pub def traverse(f: a -> m[b] & ef, t: Nel[a]): m[Nel[b]] & ef with Applicative[m] = Nel.traverse(f, t)
    pub override def sequence(t: Nel[m[a]]): m[Nel[a]] with Applicative[m] = Nel.sequence(t)
}

instance Iterable[Nel] {
    pub def iterator(t: Nel[a]): Iterator[a] & Impure =
        Nel.toIterator(t)
}
instance Reducible[Nel] {
    pub def reduceLeftTo(f: (b, a) -> b & ef1, g: a -> b & ef2, l: Nel[a]): b & (ef1 and ef2) = Nel.reduceLeftTo(f, g, l)
    pub def reduceRightTo(f: (a, b) -> b & ef1, g: a -> b & ef2, l: Nel[a]): b & (ef1 and ef2) = Nel.reduceRightTo(f, g, l)
    override pub def head(l: Nel[a]): a = Nel.head(l)
    override pub def last(l: Nel[a]): a = Nel.last(l)
    override pub def init(l: Nel[a]): List[a] = Nel.init(l)
    override pub def tail(l: Nel[a]): List[a] = Nel.tail(l)
    override pub def exists(f: a -> Bool & ef, l: Nel[a]): Bool & ef = Nel.exists(f, l)
    override pub def forall(f: a -> Bool & ef, l: Nel[a]): Bool & ef = Nel.forall(f, l)
    override pub def find(f: a -> Bool & ef, l: Nel[a]): Option[a] & ef = Nel.find(f, l)
    override pub def findLeft(f: a -> Bool & ef, l: Nel[a]): Option[a] & ef = Nel.findLeft(f, l)
    override pub def findRight(f: a -> Bool & ef, l: Nel[a]): Option[a] & ef = Nel.findRight(f, l)
    override pub def memberOf(a: a, l: Nel[a]): Bool with Eq[a] = Nel.memberOf(a, l)
    override pub def dropWhile(f: a -> Bool & ef, l: Nel[a]): List[a] & ef = Nel.dropWhile(f, l)
    override pub def takeWhile(f: a -> Bool & ef, l: Nel[a]): List[a] & ef = Nel.takeWhile(f, l)
    override pub def toArray(l: Nel[a], r: Region[r]): ScopedArray[a, r] \ Write(r) = Nel.toArray(l, r)
    override pub def toIterator(l: Nel[a]): Iterator[a] & Impure = Nel.toIterator(l)
    override pub def toList(l: Nel[a]): List[a] = Nel.toList(l)
}

instance SemiGroup[Nel[a]] {
    pub def combine(x: Nel[a], y: Nel[a]): Nel[a] = Nel.append(x, y)
}

namespace Nel {

    ///
    /// Returns a new non-empty list containing the single element `x`.
    ///
    @Time(1) @Space(1)
    pub def singleton(x: a): Nel[a] = Nel(x, Nil)

    ///
    /// Returns the non-empty list `l` prefixed with the new element `x`.
    ///
    @Time(1) @Space(1)
    pub def cons(x: a, l: Nel[a]): Nel[a] = match l {
        case Nel(y, ys) => Nel(x, y :: ys)
    }

    ///
    /// Returns the first element of `l`.
    ///
    @Time(1) @Space(1)
    pub def head(l: Nel[a]): a = match l {
        case Nel(x, _) => x
    }

    ///
    /// Returns the last element of `l`.
    ///
    @Time(length(l)) @Space(1)
    pub def last(l: Nel[a]): a = match l {
        case Nel(x, xs) => Option.getWithDefault(x, List.last(xs))
    }

    ///
    /// Returns all elements in `l` without the last element.
    ///
    @Time(length(l)) @Space(length(l))
    pub def init(l: Nel[a]): List[a] = match l {
        case Nel(_, Nil) => Nil
        case Nel(x, xs)   => match List.reverse(xs) {
            case Nil     => x :: Nil
            case _ :: ys => x :: List.reverse(ys)
        }
    }

    ///
    /// Returns all elements in `l` without the first element.
    ///
    @Time(1) @Space(1)
    pub def tail(l: Nel[a]): List[a] = match l {
        case Nel(_, xs) => xs
    }

    ///
    /// Returns the length of `l`.
    ///
    @Time(length(l)) @Space(1)
    pub def length(l: Nel[a]): Int32 = match l {
        case Nel(_, xs) => 1 + List.length(xs)
    }

    ///
    /// Returns `l2` appended to `l1`.
    ///
    @Time(length(l1)) @Space(length(l1))
    pub def append(l1: Nel[a], l2: Nel[a]): Nel[a] = match (l1, l2) {
        case (Nel(x, xs), Nel(y, ys)) => Nel(x, xs ::: (y :: ys))
    }

    ///
    /// Returns `true` if and only if `l` contains the element `a`.
    ///
    @Time(length(l)) @Space(1)
    pub def memberOf(a: a, l: Nel[a]): Bool with Eq[a] = match l {
        case Nel(x, xs) => if (x == a) true else List.memberOf(a, xs)
    }

    ///
    /// Finds the smallest element of `l` according to the `Order` on `a`.
    ///
    pub def minimum(l: Nel[a]): a with Order[a] =
        reduceLeft(Order.min, l)

    ///
    /// Finds the smallest element of `l` according to the given comparator `cmp`.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, l: Nel[a]): a =
        reduceLeft(Order.minBy(cmp), l)

    ///
    /// Finds the largest element of `l` according to the `Order` on `a`.
    ///
    pub def maximum(l: Nel[a]): a with Order[a] =
        reduceLeft(Order.max, l)

    ///
    /// Finds the largest element of `l` according to the given comparator `cmp`.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, l: Nel[a]): a =
        reduceLeft(Order.maxBy(cmp), l)

    ///
    /// Alias for `findLeft`.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def find(f: a -> Bool & ef, l: Nel[a]): Option[a] & ef = findLeft(f, l)

    ///
    /// Optionally returns the first element of `l` that satisfies the predicate `f` when searching from left to right.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def findLeft(f: a -> Bool & ef, l: Nel[a]): Option[a] & ef = match l {
        case Nel(x, xs) => if (f(x)) Some(x) else List.findLeft(f, xs)
    }

    ///
    /// Optionally returns the first element of `l` that satisfies the predicate `f` when searching from right to left.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def findRight(f: a -> Bool & ef, l: Nel[a]): Option[a] & ef = match l {
        case Nel(x, xs) => match List.findRight(f, xs) {
            case None    => if (f(x)) Some(x) else None
            case Some(y) => Some(y)
        }
    }

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def map(f: a -> b & ef, l: Nel[a]): Nel[b] & ef = match l {
        case Nel(x, xs) => Nel(f(x), List.map(f, xs))
    }

    ///
    /// Returns the result of applying `f` to every element in `l` along with that element's index.
    ///
    /// That is, the result is of the form: `f(x1, 0) :: f(x2, 1) :: ...`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def mapWithIndex(f: (a, Int32) -> b & ef, l: Nel[a]): Nel[b] & ef =
        def loop(i, ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs =>
                let y = f(x, i);
                loop(i + 1, xs, ks -> k(y :: ks))
        };
        match l {
            case Nel(x, xs) => Nel(f(x, 0), loop(1, xs, identity))
        }

    ///
    /// Apply every function from `f` to every argument from `l` and return a non-empty list with all results.
    /// For `f = f1, f2, ...` and `x = x1, x2, ...` the results appear in the order
    /// `f1(x1), f1(x2), ..., f2(x1), f2(x2), ...`.
    ///
    pub def ap(f: Nel[a -> b & ef], l: Nel[a]) : Nel[b] & ef =
        // Note - loop has been worker-wrapper transformed to represent the list of
        // functions as head (f1) and tail (fs) so we never have an empty list to deal with.
        def loop(f1, fs, k) = match fs {
            case Nil      => k(map(f1, l))
            case f2 :: rs =>
                let ks1 = map(f1, l);
                loop(f2, rs, ks2 ->
                  k(append(ks1, ks2)))
        };
        match f {
            case Nel(f1, Nil) => map(f1, l)
            case Nel(f1, rs)  => loop(f1, rs, ks -> ks as & ef)
        }

    ///
    /// Returns the result of applying `f` to every element in `l` and concatenating the results.
    ///
    @Time(time(f) * length(l)) @Space(time(f) * length(l))
    pub def flatMap(f: a -> Nel[b] & ef, l: Nel[a]): Nel[b] & ef = match l {
        case Nel(x, xs) => match f(x) {
            case Nel(y, ys) => Nel(y, ys ::: List.flatMap(z -> toList(f(z)), xs))
        }
    }

    ///
    /// Returns the reverse of `l`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def reverse(l: Nel[a]): Nel[a] = match l {
        case Nel(x, xs) => match List.reverse(x :: xs) {
            case y :: ys => Nel(y, ys)
            case _       => unreachable!()
        }
    }

    ///
    /// Returns `l` with every occurrence of `from` replaced by `to`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def replace(from: {from :: a}, to: {to :: a}, l: Nel[a]): Nel[a] with Eq[a] =
        map(e -> if (e == from.from) to.to else e, l)

    ///
    /// Returns all permutations of `l` in lexicographical order by element indices in `l`.
    ///
    /// That is, `l` is the first permutation and `reverse(l)` is the last permutation.
    ///
    @Time(Int32.factorial(length(l))) @Space(Int32.factorial(length(l)))
    pub def permutations(l: Nel[a]): Nel[List[a]] = match l {
        case Nel(x, xs) => match List.permutations(x :: xs) {
            case y :: ys => Nel(y, ys)
            case Nil => unreachable!()
        }
    }

    ///
    /// Returns all subsequences of `l` in lexicographical order by element indices in `l`.
    ///
    /// That is, `l` is the first subsequence and `Nil` is the last subsequence.
    ///
    @Time(length(l) * length(l)) @Space(length(l) * length(l))
    pub def subsequences(l: Nel[a]): Nel[List[a]] = match l {
        case Nel(x, xs) => match List.subsequences(x :: xs) {
            case y :: ys => Nel(y, ys)
            case Nil => unreachable!()
        }
    }

    ///
    /// Returns `l` with `a` inserted between every two adjacent elements.
    ///
    @Time(length(l)) @Space(length(l))
    pub def intersperse(a: a, l: Nel[a]): Nel[a] = match l {
        case Nel(x, Nil)  => Nel(x, Nil)
        case Nel(x, xs)   => Nel(x, a :: List.intersperse(a, xs))
    }

    ///
    /// Returns the result of applying `combine` to all the elements in `l`, using `empty` as the initial value.
    ///
    pub def fold(l: Nel[a]): a with Monoid[a] = Foldable.fold(l)

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, s: b, l: Nel[a]): b & ef = match l {
        case Nel(x, xs) => List.foldLeft(f, f(s, x), xs)
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b & ef, s: b, l: Nel[a]): b & ef = match l {
        case Nel(x, xs) => f(x, List.foldRight(f, s, xs))
    }

    ///
    /// Applies `f` to a start value `z` and all elements in `l` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b & ef) -> b & ef, z: b, l: Nel[a]): b & ef = match l {
        case Nel(x, xs) => f(x, _ -> List.foldRightWithCont(f, z, xs))
    }

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b & ef, l: Nel[a]): b & ef with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), l)

    ///
    /// Left-associative reduction of a structure.
    /// Applies `g` to the initial element of `l` and combines it
    /// with the remainder of `l` using `f` going from left to right.
    ///
    pub def reduceLeftTo(f: (b, a) -> b & ef1, g: a -> b & ef2, l: Nel[a]): b & (ef1 and ef2) = match l {
        case Nel(x, xs) => List.foldLeft(f, g(x), xs)
    }

    ///
    /// Right-associative reduction of a structure.
    /// Applies `g` to the initial element of `l` and combines it
    /// with the remainder of `l` using `f` going from right to left.
    ///
    pub def reduceRightTo(f: (a, b) -> b & ef1, g: a -> b & ef2, l: Nel[a]): b & (ef1 and ef2) =
        def loop(ll, k) = match ll {
            case x :: Nil => k(g(x))
            case x :: xs  => loop(xs, ks -> k(f(x, ks)))
            case _        => unreachable!()
        };
        let Nel(x, xs) = l;
        loop(x :: xs, ks -> ks as & (ef1 and ef2))

    ///
    /// Applies `combine` to all elements in `l` until a single value is obtained.
    ///
    pub def reduce(l: Nel[a]): a with SemiGroup[a] = match l {
        case Nel(x, xs) => Foldable.foldLeft((acc, y) -> SemiGroup.combine(acc, y), x, xs)
    }

    ///
    /// Applies `f` to all elements in `l` going from left to right until a single value `v` is obtained.
    ///
    /// That is, the result is of the form: `f(...f(f(x1, x2), x3)..., xn)`
    ///
    pub def reduceLeft(f: (a, a) -> a & ef, l: Nel[a]): a & ef = match l {
        case Nel(x, xs) => List.foldLeft(f, x, xs)
    }

    ///
    /// Applies `f` to all elements in `l` going from right to left until a single value `v` is obtained.
    ///
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    ///
    pub def reduceRight(f: (a, a) -> a & ef, l: Nel[a]): a & ef = match l {
        case Nel(x, xs) => match List.reduceRight(f, x :: xs) {
            case None    => unreachable!()
            case Some(v) => v
        }
    }

    ///
    /// Returns the number of elements in `l` that satisfy the predicate `f`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def count(f: a -> Bool & ef, l: Nel[a]): Int32 & ef = match l {
        case Nel(x, xs) => (if (f(x)) 1 else 0) + List.count(f, xs)
    }

    ///
    /// Returns the sum of all elements in the list `l`.
    ///
    pub def sum(l: Nel[Int32]): Int32 =
        Foldable.sum(l)

    ///
    /// Returns the sum of all elements in the list `l` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 & ef, l: Nel[a]): Int32 & ef =
        Foldable.sumWith(f, l)

    ///
    /// Returns the product of all elements in the list `l`.
    ///
    pub def product(l: Nel[Int32]): Int32 =
        Foldable.product(l)

    ///
    /// Returns the product of all elements in the list `l` according to the function `f`.
    ///
    pub def productWith(f: a -> Int32 & ef, l: Nel[a]): Int32 & ef =
        Foldable.productWith(f, l)

    ///
    /// Returns the concatenation of the elements in `l`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def flatten(l: Nel[Nel[a]]): Nel[a] = match l {
        case Nel(Nel(y, ys), xs) => Nel(y, ys ::: List.flatMap(toList, xs))
    }

    ///
    /// Returns `true` if and only if at least one element in `l` satisfies the predicate `f`.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def exists(f: a -> Bool & ef, l: Nel[a]): Bool & ef = match l {
        case Nel(x, xs) => if (f(x)) true else List.exists(f, xs)
    }

    ///
    /// Returns `true` if and only if all elements in `l` satisfy the predicate `f`.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def forall(f: a -> Bool & ef, l: Nel[a]): Bool & ef = match l {
        case Nel(x, xs) => if (f(x)) List.forall(f, xs) else false
    }

    ///
    /// Returns a list of every element in `l` that satisfies the predicate `f`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def filter(f: a -> Bool, l: Nel[a]): List[a] = match l {
        case Nel(x, xs) =>
            if (f(x))
                x :: List.filter(f, xs)
            else
                List.filter(f, xs)
    }

    ///
    /// Returns a non-empty list where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` becomes depleted, then no further elements are added to the resulting list.
    ///
    @Time(Int32.min(length(l1), length(l2))) @Space(Int32.min(length(l1), length(l2)))
    pub def zip(l1: Nel[a], l2: Nel[b]): Nel[(a,b)] = match (l1, l2) {
        case (Nel(x, xs), Nel(y, ys)) => Nel((x, y), List.zip(xs, ys))
    }

    ///
    /// Returns a non-empty list where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` becomes depleted, then no further elements are added to the resulting list.
    ///
    @Time(time(f) * Int32.min(length(l1), length(l2))) @Space(space(f) * Int32.min(length(l1), length(l2)))
    pub def zipWith(f: (a, b) -> c, l1: Nel[a], l2: Nel[b]): Nel[c] = match (l1, l2) {
        case (Nel(x, xs), Nel(y, ys)) => Nel(f(x, y), List.zipWith(f, xs, ys))
    }

    ///
    /// Returns a pair of non-empty lists, the first containing all first components in `l`
    /// and the second containing all second components in `l`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def unzip(l: Nel[(a, b)]): (Nel[a], Nel[b]) = match l {
        case Nel((a, b), xs) =>
            let (l1, l2) = List.unzip(xs);
            (Nel(a, l1), Nel(b, l2))
    }

    ///
    /// Returns a new non-empty list where each element `e` is mapped to `(e, i)`
    /// where `i` is the index of `e`.
    ///
    pub def zipWithIndex(l: Nel[a]): Nel[(a, Int32)] =
        def loop(ll, k, i) = match ll {
            case (x :: xs) => loop(xs, ks -> (k((x, i) :: ks)), i + 1)
            case Nil       => k(Nil)
        };
        match l {
            case Nel(x, xs) => Nel((x, 0), loop(xs, identity, 1))
        }

    ///
    /// Returns `l` as a normal list.
    ///
    @Time(1) @Space(1)
    pub def toList(l: Nel[a]): List[a] = match l {
        case Nel(x, xs) => x :: xs
    }

    ///
    /// Returns `l` as an array.
    ///
    pub def toArray(l: Nel[a], r: Region[r]): ScopedArray[a, r] \ Write(r) =
        (l |> toList |> List.toArray)(r)

    ///
    /// Returns `l` as a MutDeque.
    ///
    pub def toMutDeque(l: Nel[a], r: Region[r]): MutDeque[a, r] \ Write(r) =
        let d = new MutDeque(r);
        foreach(x -> MutDeque.pushBack(x, d), l);
        d

    ///
    /// Applies `f` to every element of `l`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def foreach(f: a -> Unit & ef, l: Nel[a]): Unit & ef = match l {
        case Nel(x, xs) => f(x); List.foreach(f, xs)
    }

    ///
    /// Sort the non-empty list `l` so that elements are ordered from low to high according
    /// to their `Order` instance.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `l`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sort(l: Nel[a]): Nel[a] with Order[a] = region r {
        let list = (toArray(l, r) |> Array.sort |> Array.toList);
        match list {
            case x :: xs => Nel(x, xs)
            case _       => unreachable!()
        }
    }

    ///
    /// Sort the non-empty list `l` so that elements are ordered from low to high according
    /// to the `Order` instance for the values obtained by applying `f` to each element.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `l`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortBy(f: a -> b, l: Nel[a]): Nel[a] with Order[b] = region r {
        let list = (toArray(l, r) |> Array.sortBy(f) |> Array.toList);
        match list {
            case x :: xs => Nel(x, xs)
            case _       => unreachable!()
        }
    }

    ///
    /// Sort the non-empty list `l` so that elements are ordered from low to high according
    /// to the comparison function `cmp`.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `l`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWith(cmp: (a,a) -> Comparison, l: Nel[a]): Nel[a] = region r {
        let list = (toArray(l, r) |> Array.sortWith(cmp) |> Array.toList);
        match list {
            case x :: xs => Nel(x, xs)
            case _       => unreachable!()
        }
    }

    ///
    /// Returns an iterator over `l`.
    ///
    pub def toIterator(l: Nel[a]): Iterator[a] & Impure =
       let Nel(x, xs) = l;
       List.toIterator(x :: xs)

    ///
    /// Returns the result of applying the applicative mapping function `f` to all the elements of the
    /// non-empty list `l`.
    ///
    pub def sequence(l: Nel[m[a]]): m[Nel[a]] with Applicative[m] =
        match l {
            case Nel(x, xs) => (((y, ys) -> Nel(y, ys)) `Functor.map` x) `Applicative.ap` Traversable.sequence(xs)
        }

    ///
    /// Returns the result of running all the actions in the non-empty list `l`.
    ///
    pub def traverse(f: a -> m[b] & ef, l: Nel[a]): m[Nel[b]] & ef with Applicative[m] =
        match l {
            case Nel(x, xs) => (((y, ys) -> Nel(y, ys)) `Functor.map` f(x)) `Applicative.ap` Traversable.traverse(f, xs)
        }

    ///
    /// Returns a map with elements of `l` as keys and `f` applied as values.
    ///
    /// If `l` contains multiple mappings with the same key, `toMapWith` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMapWith(f: a -> b, l: Nel[a]): Map[a, b] with Order[a] =
        Nel.foldRight((x, acc) -> Map.insert(x, f(x), acc), Map.empty(), l)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `l` with `sep` inserted between each element.
    ///
    pub def join(sep: String, l: Nel[a]): String with ToString[a] =
        Foldable.join(sep, l)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `l` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String & ef, sep: String, l: Nel[a]): String & ef =
        Foldable.joinWith(f, sep, l)

    ///
    /// Returns `l` without the longest prefix that satisfies the predicate `f`.
    ///
    pub def dropWhile(f: a -> Bool & ef, l: Nel[a]): List[a] & ef =
        let Nel(x, xs) = l;
        List.dropWhile(f, x :: xs)

    ///
    /// Returns the longest prefix of `l` that satisfies the predicate `f`.
    ///
    pub def takeWhile(f: a -> Bool & ef, l: Nel[a]): List[a] & ef =
        let Nel(x, xs) = l;
        List.takeWhile(f, x :: xs)

}

