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

instance Eq[LazyList[a]] with Eq[a] {
    pub def eq(l1: LazyList[a], l2: LazyList[a]): Bool = match (l1, l2) {
        case (ENil, ENil)                 => true
        case (ECons(x, xs), ECons(y, ys)) => if (x != y) false else xs == ys
        case (ECons(x, xs), LCons(y, ys)) => if (x != y) false else xs == force ys
        case (LCons(x, xs), ECons(y, ys)) => if (x != y) false else (force xs) == ys
        case (LCons(x, xs), LCons(y, ys)) => if (x != y) false else (force xs) == force ys
        case (LList(xs), LList(ys))       => (force xs) == force ys
        case (l, LList(ys))               => l == force ys
        case (LList(xs), l)               => (force xs) == l
        case _                            => false
    }
}
    
instance Foldable[LazyList] {
    pub def foldLeft( f: (b, a) -> b & ef, s: b, l: LazyList[a]): b & ef = LazyList.foldLeft( f, s, l)
    pub def foldRight(f: (a, b) -> b & ef, s: b, l: LazyList[a]): b & ef = LazyList.foldRight(f, s, l)
}

instance Functor[LazyList] {
    pub def map(f: a -> b & ef, l: LazyList[a]): LazyList[b] & ef = LazyList.map(f, l)
}

instance ToString[LazyList[a]] with ToString[a] {
    pub def toString(l: LazyList[a]): String = LazyList.toString(l)
}

instance Order[LazyList[a]] with Order[a] {

    ///
    /// Compares `l1` and `l2` lexicographically.
    ///
    pub def compare(l1: LazyList[a], l2: LazyList[a]): Comparison = match (l1, l2) {
        case (ENil        ,         ENil) => EqualTo
        case (_           ,         ENil) => GreaterThan
        case (ENil        ,            _) => LessThan
        case (LList(xs)   ,    LList(ys)) => (force xs) <=> (force ys)
        case (_           ,    LList(ys)) =>         l1 <=> (force ys)
        case (LList(xs)   ,            _) => (force xs) <=>        l2
        case (ECons(x, xs), ECons(y, ys)) =>
            let cmp = x <=> y;
            if (cmp == EqualTo)         xs <=> ys         else cmp
        case (ECons(x, xs), LCons(y, ys)) =>
            let cmp = x <=> y;
            if (cmp == EqualTo)         xs <=> (force ys) else cmp
        case (LCons(x, xs), ECons(y, ys)) =>
            let cmp = x <=> y;
            if (cmp == EqualTo) (force xs) <=>        ys  else cmp
        case (LCons(x, xs), LCons(y, ys)) =>
            let cmp = x <=> y;
            if (cmp == EqualTo) (force xs) <=> (force ys) else cmp
    }
}


namespace LazyList {

    ///
    /// Converts the list `l` into a String.
    ///
    /// Forces the entire list `l`.
    ///
    pub def toString(l: LazyList[a]): String with ToString[a] =
        toStringHelper(l) as & Pure

    ///
    /// Helper function for `toString`.
    ///
    def toStringHelper(l: LazyList[a]): String & Impure with ToString[a] =
        let sb = StringBuilder.new();
        map(x -> StringBuilder.appendString!(sb, "${x} :: "), l);
        StringBuilder.appendString!(sb, "ENil");
        StringBuilder.toString(sb)

    ///
    /// Returns an empty LazyList.
    ///
    pub def empty(): LazyList[a] = ENil

    ///
    /// Returns true if and only if `l` is the empty LazyList, i.e. `ENil`.
    ///
    /// Does not force the tail of `l`.
    ///
    pub def isEmpty(l: LazyList[a]): Bool = match l {
        case ENil      => true
        case LList(xs) => isEmpty(force xs)
        case _         => false
    }

    ///
    /// Returns a lazy list of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty lazy list if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): LazyList[Int32] =
        LList(lazy rangeHelper(b, e))

    ///
    /// Helper function for `range`.
    ///
    def rangeHelper(b: Int32, e:Int32): LazyList[Int32] =
        if (b >= e)
            ENil
        else
            LCons(b, lazy rangeHelper(b + 1, e))

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
        case LList(xs)   => head(force xs)
    }

    ///
    /// Returns `l` without the first element.
    ///
    pub def tail(l: LazyList[a]): LazyList[a] = match l {
        case ENil         => ENil
        case ECons(_, xs) => xs
        case LCons(_, xs) => LList(                xs)
        case LList(xs)    => LList(lazy tail(force xs))
    }

    ///
    /// Returns the first `n` elements of `l`.
    ///
    /// Does not force the tail of `l`.
    ///
    pub def take(n: Int32, l: LazyList[a]): LazyList[a] =
        LList(lazy takeHelper(n, l))

    ///
    /// Helper function for `take`.
    ///
    def takeHelper(n: Int32, l: LazyList[a]): LazyList[a] =
        if (n <= 0)
            ENil
        else
            match l {
                case ENil         => ENil
                case ECons(x, xs) => LCons(x, lazy takeHelper(n - 1,       xs))
                case LCons(x, xs) => LCons(x, lazy takeHelper(n - 1, force xs))
                case LList(xs)    => LList(   lazy takeHelper(n,     force xs))
            }

    ///
    /// Returns the length of `l`.
    ///
    /// Forces the entire list `l`.
    ///
    pub def length(l: LazyList[a]): Int32 =
        lengthAcc(l, 0)

    ///
    /// Helper function for `length`
    ///
    def lengthAcc(l: LazyList[a], acc: Int32): Int32 = match l {
        case ENil         => acc
        case ECons(_, xs) => lengthAcc(      xs, acc + 1)
        case LCons(_, xs) => lengthAcc(force xs, acc + 1)
        case LList(xs)    => lengthAcc(force xs, acc)
    }

    ///
    /// Reverses the list `l`.
    ///
    pub def reverse(l: LazyList[a]): LazyList[a] =
        reverseAcc(l, ENil)

    ///
    /// Helper function for `reverse`.
    ///
    def reverseAcc(l: LazyList[a], acc: LazyList[a]): LazyList[a] = match l {
        case ENil         => acc
        case ECons(x, xs) => LList(lazy reverseAcc(      xs, ECons(x, acc)))
        case LCons(x, xs) => LList(lazy reverseAcc(force xs, ECons(x, acc)))
        case LList(xs)    => LList(lazy reverseAcc(force xs,          acc))
    }

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    pub def map(f: a -> b & ef, l: LazyList[a]): LazyList[b] & ef =
        if (reify ef)
            mapL(f as a -> b & Pure,   l)
        else
            mapE(f as a -> b & Impure, l) as & ef

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    def mapL(f: a -> b, l: LazyList[a]): LazyList[b] = match l {
        case ENil         => ENil
        case ECons(x, xs) => LList(lazy LCons(f(x), lazy mapL1(f,       xs)))
        case LCons(x, xs) => LList(lazy LCons(f(x), lazy mapL1(f, force xs)))
        case LList(xs)    => LList(                 lazy mapL1(f, force xs))
    }

    // TODO: DOC
    def mapL1(f: a -> b, l: LazyList[a]): LazyList[b] = match l {
        case ENil         => ENil
        case ECons(x, xs) => LCons(f(x), lazy mapL1(f,       xs))
        case LCons(x, xs) => LCons(f(x), lazy mapL1(f, force xs))
        case LList(xs)    => LList(      lazy mapL1(f, force xs))
    }

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def mapE(f: a -> b & Impure, l: LazyList[a]): LazyList[b] & Impure =
        mapEAcc(f, l, ENil) |> reverse

    ///
    /// Helper function for `mapE`.
    ///
    def mapEAcc(f: a -> b & Impure, l: LazyList[a], acc: LazyList[b]): LazyList[b] & Impure = match l {
        case ENil         => acc
        case ECons(x, xs) => mapEAcc(f,       xs, ECons(f(x), acc))
        case LCons(x, xs) => mapEAcc(f, force xs, ECons(f(x), acc))
        case LList(xs)    => mapEAcc(f, force xs,             acc)
    }

    ///
    /// Returns a lazy list of every element in `l` that satisfies the predicate `f`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    pub def filter(f: a -> Bool & ef, l: LazyList[a]): LazyList[a] & ef =
        if (reify ef)
            filterL(f as a -> Bool & Pure,   l)
        else
            filterE(f as a -> Bool & Impure, l) as & ef

    ///
    /// Returns a lazy list of every element in `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    def filterL(f: a -> Bool, l: LazyList[a]): LazyList[a] = match l {
        case ENil                 => ENil
        case ECons(x, xs) if f(x) => LCons(x, lazy filterL(f,       xs))
        case LCons(x, xs) if f(x) => LCons(x, lazy filterL(f, force xs))
        case ECons(_, xs)         => LList(   lazy filterL(f,       xs))
        case LCons(_, xs)         => LList(   lazy filterL(f, force xs))
        case LList(xs)            => LList(   lazy filterL(f, force xs))
    }

    ///
    /// Returns a lazy list of every element in `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def filterE(f: a -> Bool & Impure, l: LazyList[a]): LazyList[a] & Impure =
        filterEAcc(f, l, ENil) |> reverse

    ///
    /// Helper function for `filterE`
    ///
    def filterEAcc(f: a -> Bool & Impure, l: LazyList[a], acc: LazyList[a]): LazyList[a] & Impure = match l {
        case ENil         => acc
        case ECons(x, xs) => if (f(x)) filterEAcc(f,       xs, ECons(x, acc)) else filterEAcc(f,       xs, acc)
        case LCons(x, xs) => if (f(x)) filterEAcc(f, force xs, ECons(x, acc)) else filterEAcc(f, force xs, acc)
        case LList(xs)    => filterEAcc(f, force xs, acc)
    }

    ///
    /// Returns `l2` appended to `l1`.
    ///
    pub def append(l1: LazyList[a], l2: LazyList[a]): LazyList[a] = match l1 {
        case ENil         => l2
        case ECons(x, xs) => LCons(x, lazy append(      xs, l2))
        case LCons(x, xs) => LCons(x, lazy append(force xs, l2))
        case LList(xs)    => LList(   lazy append(force xs, l2))
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, s: b, l: LazyList[a]): b & ef = match l {
        case ENil         => s
        case ECons(x, xs) => foldLeft(f, f(s, x),       xs)
        case LCons(x, xs) => foldLeft(f, f(s, x), force xs)
        case LList(xs)    => foldLeft(f, s,       force xs)
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b & ef, s: b, l: LazyList[a]): b & ef =
        foldRightAcc(f, s, l, s1 -> s1 as & ef)

    ///
    /// Helper function for `foldRight`.
    ///
    def foldRightAcc(f: (a, b) -> b & ef, s: b, l: LazyList[a], k: b -> b & ef): b & ef = match l {
        case ENil         => k(s)
        case ECons(x, xs) => foldRightAcc(f, s,       xs, s1 -> k(f(x, s1)))
        case LCons(x, xs) => foldRightAcc(f, s, force xs, s1 -> k(f(x, s1)))
        case LList(xs)    => foldRightAcc(f, s, force xs, k)
    }

    ///
    /// Returns `l` as an `Array`.
    ///
    /// Forces the entire list `l`.
    ///
    pub def toArray(l: LazyList[a]): Array[a] & Impure =
        let a = Array.new(default, length(l));
        let f = (i, y) -> { a[i] = y; i + 1 };
        foldLeft(f, 0, l);
        a

    ///
    /// Returns `l` as an `Iterator`.
    ///
    /// Does not force any elements of the list.
    ///
    pub def toIter(l: LazyList[a]): Iterator[a] & Impure =
        let cursor = ref l;
        let done = () -> match head(deref cursor) {
            case None    => true
            case Some(_) => false
        };
        let next = () -> match head(deref cursor) {
            case None    => bug!("Empty iterator")
            case Some(x) =>
                cursor := tail(deref cursor);
                x
        };
        Iterator(done, next)

    ///
    /// Returns `l` as a `List`.
    ///
    /// Forces the entire list `l`.
    ///
    pub def toList(l: LazyList[a]): List[a] =
        toListAcc(l, Nil) |> List.reverse

    ///
    /// Helper function for `toList`.
    ///
    def toListAcc(l: LazyList[a], acc: List[a]): List[a] = match l {
        case ENil         => acc
        case ECons(x, ys) => toListAcc(      ys, x :: acc)
        case LCons(x, ys) => toListAcc(force ys, x :: acc)
        case LList(ys)    => toListAcc(force ys,      acc)
    }

    ///
    /// Returns the association list `l` as a map.
    ///
    /// If `l` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    /// Forces the entire list `l`.
    ///
    pub def toMap(l: LazyList[(a, b)]): Map[a, b] with Order[a] =
        toMapAcc(l, Map.empty())

    ///
    /// Helper functions for `toMap`.
    ///
    def toMapAcc(l: LazyList[(a, b)], acc: Map[a, b]): Map[a, b] with Order[a] = match l {
        case ENil              => acc
        case ECons((k, v), xs) => toMapAcc(      xs, Map.insert(k, v, acc))
        case LCons((k, v), xs) => toMapAcc(force xs, Map.insert(k, v, acc))
        case LList(xs)         => toMapAcc(force xs, acc)
    }

    ///
    /// Returns `l` as a `Set`.
    ///
    /// Forces the entire list `l`.
    ///
    pub def toSet(l: LazyList[a]): Set[a] with Order[a] =
        toSetAcc(l, Set.empty())

    ///
    /// Helper functions for `toSet`.
    ///
    def toSetAcc(l: LazyList[a], acc: Set[a]): Set[a] with Order[a] = match l {
        case ENil         => acc
        case ECons(x, xs) => toSetAcc(      xs, Set.insert(x, acc))
        case LCons(x, xs) => toSetAcc(force xs, Set.insert(x, acc))
        case LList(xs)    => toSetAcc(force xs, acc)
    }

    ///
    /// Returns the number of elements in `l` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def count(f: a -> Bool, l: LazyList[a]): Int32 =
        foldLeft((i, x) -> if (f(x)) i + 1 else i, 0, l)

    ///
    /// Returns `l` without the first `n` elements.
    ///
    /// Returns `ENil` if `n > length(l)`.
    /// Returns `l` if `n < 1`.
    ///
    pub def drop(n: Int32, l: LazyList[a]): LazyList[a] =
        LList(lazy dropHelper(n, l))

    ///
    /// Helper function for `drop`.
    ///
    def dropHelper(n: Int32, l: LazyList[a]): LazyList[a] =
        if (n < 1)
            l
        else
            match l {
                case ENil         => l
                case ECons(_, xs) => LList(lazy dropHelper(n - 1,       xs))
                case LCons(_, xs) => LList(lazy dropHelper(n - 1, force xs))
                case LList(xs)    => LList(lazy dropHelper(n,     force xs))
            }

    ///
    /// Returns the concatenation of the elements in `l`.
    ///
    pub def flatten(l: LazyList[LazyList[a]]): LazyList[a] = match l {
        case ENil          => ENil
        case ECons(xs, ys) => LList(lazy append(xs, flatten(      ys)))
        case LCons(xs, ys) => LList(lazy append(xs, flatten(force ys)))
        case LList(xs)     => LList(lazy flatten(force xs))
    }

    ///
    /// Returns `true` if and only if at least one element in `l` satisfies the predicate `f`.
    ///
    /// Returns `false` if `l` is empty.
    ///
    /// The function `f` must be pure.
    ///
    pub def exists(f: a -> Bool & ef, l: LazyList[a]): Bool & ef = match l {
        case ENil         => false
        case ECons(x, xs) => f(x) or exists(f, xs)
        case LCons(x, xs) => f(x) or exists(f, force xs)
        case LList(xs)    => exists(f, force xs)
    }

    ///
    /// Returns `true` if and only if all elements in `l` satisfy the predicate `f`.
    ///
    /// Returns `true` if `l` is empty.
    ///
    /// The function `f` must be pure.
    ///
    pub def forall(f: a -> Bool & ef, l: LazyList[a]): Bool & ef = match l {
        case ENil         => true
        case ECons(x, xs) => f(x) and forall(f, xs)
        case LCons(x, xs) => f(x) and forall(f, force xs)
        case LList(xs)    => forall(f, force xs)
    }

    ///
    /// Returns an infinite LazyList of repeating `x`s.
    ///
    pub def repeat(x: a): LazyList[a] =
        LList(lazy repeatHelper(x))

    ///
    /// Helper function for `repeat`.
    ///
    def repeatHelper(x: a): LazyList[a] =
        LCons(x, lazy repeatHelper(x))

    ///
    /// Returns an infinite sequence of integers starting from and including `n`.
    ///
    pub def from(n: Int32): LazyList[Int32] =
        LList(lazy fromHelper(n))

    ///
    /// Helper function for `from`.
    ///
    def fromHelper(n: Int32): LazyList[Int32] =
        LCons(n, lazy fromHelper(n + 1))

    ///
    /// Returns `Some(x)` if `x` is the last element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Forces the entire list `l`.
    ///
    pub def last(l: LazyList[a]): Option[a] = match l {
        case ENil         => None
        case ECons(x, xs) => if (isEmpty(      xs)) Some(x) else last(      xs)
        case LCons(x, xs) => if (isEmpty(force xs)) Some(x) else last(force xs)
        case LList(xs)    => last(force xs)
    }

    ///
    /// Returns a list where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` becomes depleted, then no further elements are added to the resulting list.
    ///
    pub def zip(l1: LazyList[a], l2: LazyList[b]): LazyList[(a, b)] = match (l1, l2) {
        case (ENil, _)                    => ENil
        case (_, ENil)                    => ENil
        case (ECons(x, xs), ECons(y, ys)) => LList(lazy LCons((x, y), lazy zip(      xs,       ys)))
        case (ECons(x, xs), LCons(y, ys)) => LList(lazy LCons((x, y), lazy zip(      xs, force ys)))
        case (LCons(x, xs), ECons(y, ys)) => LList(lazy LCons((x, y), lazy zip(force xs,       ys)))
        case (LCons(x, xs), LCons(y, ys)) => LList(lazy LCons((x, y), lazy zip(force xs, force ys)))
        case (LList(xs), LList(ys))       => LList(                   lazy zip(force xs, force ys))
        case (xs, LList(ys))              => LList(                   lazy zip(      xs, force ys))
        case (LList(xs), ys)              => LList(                   lazy zip(force xs,       ys))
    }

    ///
    /// Returns a list where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` becomes depleted, then no further elements are added to the resulting list.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    pub def zipWith(f: (a, b) -> c & ef, l1: LazyList[a], l2: LazyList[b]): LazyList[c] & ef =
        map((x) -> f(fst(x), snd(x)), zip(l1, l2))

    ///
    /// Applies `f` to all elements in `l` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def reduceLeft(f: (a, a) -> a & ef, l: LazyList[a]): Option[a] & ef = match l {
        case ENil         => None
        case ECons(x, xs) => Some(foldLeft(f, x,       xs))
        case LCons(x, xs) => Some(foldLeft(f, x, force xs))
        case LList(xs)    => reduceLeft(f, force xs)
    }

    ///
    /// Applies `f` to all elements in `l` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def reduceRight(f: (a, a) -> a & ef, l: LazyList[a]): Option[a] & ef = match l {
        case ENil      => None
        case LList(xs) => reduceRight(f, force xs)
        case _         => Some(reduceRightHelper(f, l, ks -> ks as & ef))
    }

    ///
    /// Helper function for `reduceRight`.
    /// `l` must be non-empty.
    ///
    def reduceRightHelper(f: (a, a) -> a & ef, l: LazyList[a], k: a -> a & ef): a & ef = match l {
        case ECons(x, xs) => if (isEmpty(      xs)) k(x) else reduceRightHelper(f,       xs, ks -> k(f(x, ks)))
        case LCons(x, xs) => if (isEmpty(force xs)) k(x) else reduceRightHelper(f, force xs, ks -> k(f(x, ks)))
        case LList(xs)    => reduceRightHelper(f, force xs, k)
        case _            => bug!("Unreachable code!")
    }

    ///
    /// Optionally finds the smallest element of `l` according to the `Order` on `a`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def minimum(l: LazyList[a]): Option[a] with Order[a] =
        reduceLeft(Order.min, l)

    ///
    /// Optionally finds the smallest element of `l` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, l: LazyList[a]): Option[a] =
        reduceLeft(Order.minBy(cmp), l)

    ///
    /// Optionally finds the largest element of `l` according to the `Order` on `a`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def maximum(l: LazyList[a]): Option[a] with Order[a] =
        reduceLeft(Order.max, l)

    ///
    /// Optionally finds the largest element of `l` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, l: LazyList[a]): Option[a] =
        reduceLeft(Order.maxBy(cmp), l)

    ///
    /// Returns the result of applying `f` to every element in `l` and concatenating the results.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    pub def flatMap(f: a -> LazyList[b] & ef, l: LazyList[a]): LazyList[b] & ef =
        if (reify ef)
            flatMapL(f as a -> LazyList[b] & Pure,   l)
        else
            flatMapE(f as a -> LazyList[b] & Impure, l) as & ef

    ///
    /// Returns the result of applying `f` to every element in `l` and concatenating the results.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    def flatMapL(f: a -> LazyList[b], l: LazyList[a]): LazyList[b] = match l {
        case ENil         => ENil
        case ECons(x, xs) => LList(lazy append(f(x), LList(lazy flatMapL(f,       xs))))
        case LCons(x, xs) => LList(lazy append(f(x), LList(lazy flatMapL(f, force xs))))
        case LList(xs)    => LList(lazy flatMapL(f, force xs))
    }

    ///
    /// Returns the result of applying `f` to every element in `l` and concatenating the results.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def flatMapE(f: a -> LazyList[b] & Impure, l: LazyList[a]): LazyList[b] & Impure =
        flatMapEHelper(f, l, ks -> ks)

    ///
    /// Helper function for `flatMapE`.
    ///
    def flatMapEHelper(f: a -> LazyList[b] & Impure, l: LazyList[a], k: LazyList[b] -> LazyList[b]): LazyList[b] & Impure = match l {
        case ENil         => k(ENil)
        case ECons(x, xs) =>
            let xs1 = f(x);
            flatMapEHelper(f,       xs, ks -> k(append(xs1, ks)))
        case LCons(x, xs) =>
            let xs1 = f(x);
            flatMapEHelper(f, force xs, ks -> k(append(xs1, ks)))
        case LList(xs)    => flatMapEHelper(f, force xs, k)
    }

    ///
    /// Returns `true` if and only if `l` contains the element `a`.
    ///
    /// Forces elements until `a` is found.
    ///
    pub def memberOf(a: a, l: LazyList[a]): Bool with Eq[a] = match l {
        case ENil         => false
        case ECons(x, xs) => x == a or memberOf(a,       xs)
        case LCons(x, xs) => x == a or memberOf(a, force xs)
        case LList(xs)    =>           memberOf(a, force xs)
    }

    ///
    /// Returns `l` with every occurrence of `x` replaced by `y`.
    ///
    pub def replace(x: a, y: a, l: LazyList[a]): LazyList[a] with Eq[a] =
        map(e -> if (x == e) y else e, l)

    ///
    /// Optionally returns the first element of `l` that satisfies the predicate `f` when searching from left to right.
    ///
    pub def findLeft(f: a -> Bool & ef, l: LazyList[a]): Option[a] & ef = match l {
        case ENil         => None
        case ECons(x, xs) => if (f(x)) Some(x) else findLeft(f,       xs)
        case LCons(x, xs) => if (f(x)) Some(x) else findLeft(f, force xs)
        case LList(xs)    => findLeft(f, force xs)
    }

    ///
    /// Optionally returns the first element of `l` that satisfies the predicate `f` when searching from right to left.
    ///
    pub def findRight(f: a -> Bool & ef, l: LazyList[a]): Option[a] & ef =
        reverse(l) |> findLeft(f)

    ///
    /// Returns the result of applying `f` to every element in `l` along with that element's index.
    ///
    /// That is, the result is of the form: `f(x1, 0) :: f(x2, 1) :: ...`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    pub def mapWithIndex(f: (a, Int32) -> b & ef, l: LazyList[a]): LazyList[b] & ef =
        if (reify ef)
            mapWithIndexL(f as (a, Int32) -> b & Pure,   l)
        else
            mapWithIndexE(f as (a, Int32) -> b & Impure, l) as & ef

    ///
    /// Returns the result of applying `f` to every element in `l` along with the element's index.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    def mapWithIndexL(f: (a, Int32) -> b, l: LazyList[a]): LazyList[b] =
        mapWithIndexLAcc(f, l, 0)

    ///
    /// Helper function for `mapWithIndexL`.
    ///
    def mapWithIndexLAcc(f: (a, Int32) -> b, l: LazyList[a], i: Int32): LazyList[b] = match l {
        case ENil         => ENil
        case ECons(x, xs) => LList(lazy LCons(f(x, i), lazy mapWithIndexLAcc(f,       xs, i + 1)))
        case LCons(x, xs) => LList(lazy LCons(f(x, i), lazy mapWithIndexLAcc(f, force xs, i + 1)))
        case LList(xs)    => LList(                    lazy mapWithIndexLAcc(f, force xs, i    ))
    }

    ///
    /// Returns the result of applying `f` to every element in `l` along with the element's index.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def mapWithIndexE(f: (a, Int32) -> b & Impure, l: LazyList[a]): LazyList[b] & Impure =
        mapWithIndexEAcc(f, l, 0, ks -> ks)

    ///
    /// Helper function for `mapWithIndexE`.
    ///
    def mapWithIndexEAcc(f: (a, Int32) -> b & Impure, l: LazyList[a], i: Int32, k: LazyList[b] -> LazyList[b]): LazyList[b] & Impure = match l {
        case ENil         => k(ENil)
        case ECons(x, xs) =>
            let x1 = f(x, i);
            mapWithIndexEAcc(f,       xs, i + 1, ks -> k(ECons(x1, ks)))
        case LCons(x, xs) =>
            let x1 = f(x, i);
            mapWithIndexEAcc(f, force xs, i + 1, ks -> k(ECons(x1, ks)))
        case LList(xs)    => mapWithIndexEAcc(f, force xs, i, k)
    }

    ///
    /// Returns the concatenation of the elements in `l2` with the elements of `l1` inserted between every two adjacent elements.
    ///
    /// That is, returns `l2.1 :: l1.1 ... l1.n :: l2.2 :: ... :: l2.n-1 :: l1.1 :: ... :: l1.n :: l2.n :: ENil`.
    ///
    /// Forces the tail of `l2` unless it is an unevaluated LazyList.
    ///
    pub def intercalate(l1: LazyList[a], l2: LazyList[LazyList[a]]): LazyList[a] = match l2 {
        case ENil           => ENil
        case ECons(x, xs)   => if (isEmpty(      xs)) x else LList(lazy append(append(x, l1), intercalate(l1,       xs)))
        case LCons(x, xs)   => if (isEmpty(force xs)) x else LList(lazy append(append(x, l1), intercalate(l1, force xs)))
        case LList(xs)      => LList(lazy intercalate(l1, force xs))
    }

    ///
    /// Returns `xs` with `a` inserted between every two adjacent elements.
    ///
    /// Forces the tail of `l` unless it is an unevaluated LazyList.
    ///
    pub def intersperse(x: a, l: LazyList[a]): LazyList[a] = match l {
        case ENil           => ENil
        case ECons(x1, xs)  => if (isEmpty(      xs)) l else LCons(x1, lazy LCons(x, lazy intersperse(x,       xs)))
        case LCons(x1, xs)  => if (isEmpty(force xs)) l else LCons(x1, lazy LCons(x, lazy intersperse(x, force xs)))
        case LList(xs)      => LList(lazy intersperse(x, force xs))
    }

    ///
    /// Returns the longest prefix of `l` that satisfies the predicate `f`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    pub def takeWhile(f: a -> Bool & ef, l: LazyList[a]): LazyList[a] & ef =
        if (reify ef)
            takeWhileL(f as a -> Bool & Pure,   l)
        else
            takeWhileE(f as a -> Bool & Impure, l) as & ef

    ///
    /// Returns the longest prefix of `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    def takeWhileL(f: a -> Bool, l: LazyList[a]): LazyList[a] =
        LList(lazy takeWhileLHelper(f, l))

    ///
    /// Helper function for `takeWhileL`.
    ///
    def takeWhileLHelper(f: a -> Bool, l: LazyList[a]): LazyList[a] = match l {
        case ENil         => ENil
        case ECons(x, xs) => if (f(x)) LCons(x, lazy takeWhileL(f,       xs)) else ENil
        case LCons(x, xs) => if (f(x)) LCons(x, lazy takeWhileL(f, force xs)) else ENil
        case LList(xs)    => LList(lazy takeWhileL(f, force xs))
    }

    ///
    /// Returns the longest prefix of `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def takeWhileE(f: a -> Bool & Impure, l: LazyList[a]): LazyList[a] & Impure =
        takeWhileEAcc(f, l, ks -> ks)

    ///
    /// Helper function for `takeWhileE`.
    ///
    def takeWhileEAcc(f: a -> Bool & Impure, l: LazyList[a], k: LazyList[a] -> LazyList[a]): LazyList[a] & Impure = match l {
        case ENil         => k(ENil)
        case ECons(x, xs) => if (f(x)) takeWhileEAcc(f,       xs, ks -> k(ECons(x, ks))) else k(ENil)
        case LCons(x, xs) => if (f(x)) takeWhileEAcc(f, force xs, ks -> k(ECons(x, ks))) else k(ENil)
        case LList(xs)    => takeWhileEAcc(f, force xs, k)
    }

    ///
    /// Returns `l` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    pub def dropWhile(f: a -> Bool & ef, l: LazyList[a]): LazyList[a] & ef =
        if (reify ef)
            dropWhileL(f as a -> Bool & Pure,   l)
        else
            dropWhileE(f as a -> Bool & Impure, l) as & ef

    ///
    /// Returns `l` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    def dropWhileL(f: a -> Bool, l: LazyList[a]): LazyList[a] =
        LList(lazy dropWhileLHelper(f, l))

    ///
    /// Helper function for `dropWhileL`.
    ///
    def dropWhileLHelper(f: a -> Bool, l: LazyList[a]): LazyList[a] = match l {
        case ENil         => ENil
        case ECons(x, xs) => if (f(x)) LList(lazy dropWhileL(f,       xs)) else l
        case LCons(x, xs) => if (f(x)) LList(lazy dropWhileL(f, force xs)) else l
        case LList(xs)    => LList(lazy dropWhileL(f, force xs))
    }

    ///
    /// Returns `l` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def dropWhileE(f: a -> Bool & Impure, l: LazyList[a]): LazyList[a] & Impure = match l {
        case ENil         => ENil
        case ECons(x, xs) => if (f(x)) dropWhileE(f,       xs) else l
        case LCons(x, xs) => if (f(x)) dropWhileE(f, force xs) else l
        case LList(xs)    => dropWhileE(f, force xs)
    }

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `l`.
    ///
    /// Returns `None` if every element `f(x)` of `l` is `None`.
    ///
    pub def findMap(f: a -> Option[b] & ef, l: LazyList[a]): Option[b] & ef = match l {
        case ENil         => None
        case ECons(x, xs) => match f(x) {
            case None    => findMap(f, xs)
            case Some(v) => Some(v)
        }
        case LCons(x, xs) => match f(x) {
            case None    => findMap(f, force xs)
            case Some(v) => Some(v)
        }
        case LList(xs)    => findMap(f, force xs)
    }

    ///
    /// Collects the results of applying the partial function `f` to every element in `l`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    pub def filterMap(f: a -> Option[b] & ef, l: LazyList[a]): LazyList[b] & ef =
        if (reify ef)
            filterMapL(f as a -> Option[b] & Pure,   l)
        else
            filterMapE(f as a -> Option[b] & Impure, l) as & ef

    ///
    /// Collects the results of applying the partial function `f` to every element in `l`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    def filterMapL(f: a -> Option[b], l: LazyList[a]): LazyList[b] = match l {
        case ENil         => ENil
        case ECons(x, xs) =>
            LList(lazy match f(x) {
                case None    => filterMapL(f, xs)
                case Some(v) => LCons(v, lazy filterMapL(f, xs))
            })
        case LCons(x, xs) =>
            LList(lazy match f(x) {
                case None    => filterMapL(f, force xs)
                case Some(v) => LCons(v, lazy filterMapL(f, force xs))
            })
        case LList(xs)    => LList(lazy filterMapL(f, force xs))
    }

    ///
    /// Collects the results of applying the partial function `f` to every element in `l`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def filterMapE(f: a -> Option[b] & Impure, l: LazyList[a]): LazyList[b] & Impure =
        filterMapEAcc(f, l, ks -> ks)

    ///
    /// Helper function for `filterMapE`.
    ///
    def filterMapEAcc(f: a -> Option[b] & Impure, l: LazyList[a], k: LazyList[b] -> LazyList[b]): LazyList[b] & Impure = match l {
        case ENil         => k(ENil)
        case ECons(x, xs) => match f(x) {
            case None    => filterMapEAcc(f, xs, k)
            case Some(v) => filterMapEAcc(f, xs, ks -> k(ECons(v, ks)))
        }
        case LCons(x, xs) => match f(x) {
            case None    => filterMapEAcc(f, force xs, k)
            case Some(v) => filterMapEAcc(f, force xs, ks -> k(ECons(v, ks)))
        }
        case LList(xs)    => filterMapEAcc(f, force xs, k)
    }

    ///
    /// Returns a pair of lists `(l1, l2)`.
    ///
    /// `l1` contains all elements of `l` that satisfy the predicate `f`.
    /// `l2` contains all elements of `l` that DO NOT satisfy the predicate `f`.
    ///
    pub def partition(f: a -> Bool & ef, l: LazyList[a]): (LazyList[a], LazyList[a]) & ef =
        partitionAcc(f, l, (ks, ls) -> (ks, ls))

    ///
    /// Helper function for `partition`.
    ///
    def partitionAcc(f: a -> Bool & ef, l: LazyList[a], k: (LazyList[a], LazyList[a]) -> (LazyList[a], LazyList[a])): (LazyList[a], LazyList[a]) & ef = match l {
        case ENil         => k(ENil, ENil)
        case ECons(x, xs) => if (f(x)) partitionAcc(f,       xs, (ks, ls) -> k(ECons(x, ks), ls)) else partitionAcc(f,       xs, (ks, ls) -> k(ks, ECons(x, ls)))
        case LCons(x, xs) => if (f(x)) partitionAcc(f, force xs, (ks, ls) -> k(ECons(x, ks), ls)) else partitionAcc(f, force xs, (ks, ls) -> k(ks, ECons(x, ls)))
        case LList(xs)    => partitionAcc(f, force xs, k)
    }

    ///
    /// `l1` is the longest prefix of `l` that satisfies the predicate `f`.
    /// `l2` is the remainder of `l`.
    ///
    /// Forces elements in the list until the first element (inclusive) that does not satisfy the predicate `f`.
    ///
    pub def span(f: a -> Bool & ef, l: LazyList[a]): (LazyList[a], LazyList[a]) & ef =
        spanAcc(f, l, (ks, ls) -> (ks, ls))

    ///
    /// Helper function for `span`.
    ///
    def spanAcc(f: a -> Bool & ef, l: LazyList[a], k: (LazyList[a], LazyList[a]) -> (LazyList[a], LazyList[a])): (LazyList[a], LazyList[a]) & ef = match l {
        case ENil         => k(ENil, ENil)
        case ECons(x, xs) =>
            if (f(x))
                spanAcc(f, xs, (ks, ls) -> k(ECons(x, ks), ls))
            else
                k(ENil, l)
        case LCons(x, xs) =>
            if (f(x))
                spanAcc(f, force xs, (ks, ls) -> k(ECons(x, ks), ls))
            else
                k(ENil, l)
        case LList(xs)    => spanAcc(f, force xs, k)
    }

    ///
    /// Returns the sum of all elements in the list `l`.
    ///
    pub def sum(l: LazyList[Int32]): Int32 =
        sumAcc(l, 0)

    ///
    /// Helper function for `sum`.
    ///
    def sumAcc(l: LazyList[Int32], acc: Int32): Int32 = match l {
        case ENil         => acc
        case ECons(x, xs) => sumAcc(      xs, x + acc)
        case LCons(x, xs) => sumAcc(force xs, x + acc)
        case LList(xs)    => sumAcc(force xs,     acc)
    }

}