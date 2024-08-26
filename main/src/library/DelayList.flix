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

pub enum DelayList[a] with Sendable {
    case ENil,
    case ECons(a, DelayList[a]),
    case LCons(a, Lazy[DelayList[a]]),
    case LList(Lazy[DelayList[a]])
}

instance Eq[DelayList[a]] with Eq[a] {
    pub def eq(l1: DelayList[a], l2: DelayList[a]): Bool = match (l1, l2) {
        case (DelayList.ENil, DelayList.ENil)                   => true
        case (DelayList.ECons(x, xs), DelayList.ECons(y, ys))   => if (x != y) false else xs == ys
        case (DelayList.ECons(x, xs), DelayList.LCons(y, ys))   => if (x != y) false else xs == force ys
        case (DelayList.LCons(x, xs), DelayList.ECons(y, ys))   => if (x != y) false else (force xs) == ys
        case (DelayList.LCons(x, xs), DelayList.LCons(y, ys))   => if (x != y) false else (force xs) == force ys
        case (DelayList.LList(xs), DelayList.LList(ys))         => (force xs) == force ys
        case (l, DelayList.LList(ys))                           => l == force ys
        case (DelayList.LList(xs), l)                           => (force xs) == l
        case _                                                  => false
    }
}

instance Order[DelayList[a]] with Order[a] {

    ///
    /// Compares `l1` and `l2` lexicographically.
    ///
    pub def compare(l1: DelayList[a], l2: DelayList[a]): Comparison = match (l1, l2) {
        case (DelayList.ENil ,         DelayList.ENil) => Comparison.EqualTo
        case (_           ,         DelayList.ENil) => Comparison.GreaterThan
        case (DelayList.ENil        ,            _) => Comparison.LessThan
        case (DelayList.LList(xs)   ,    DelayList.LList(ys)) => (force xs) <=> (force ys)
        case (_           ,    DelayList.LList(ys)) =>         l1 <=> (force ys)
        case (DelayList.LList(xs)   ,            _) => (force xs) <=>        l2
        case (DelayList.ECons(x, xs), DelayList.ECons(y, ys)) =>
            let cmp = x <=> y;
            if (cmp == Comparison.EqualTo) xs <=> ys else cmp
        case (DelayList.ECons(x, xs), DelayList.LCons(y, ys)) =>
            let cmp = x <=> y;
            if (cmp == Comparison.EqualTo) xs <=> (force ys) else cmp
        case (DelayList.LCons(x, xs), DelayList.ECons(y, ys)) =>
            let cmp = x <=> y;
            if (cmp == Comparison.EqualTo) (force xs) <=> ys  else cmp
        case (DelayList.LCons(x, xs), DelayList.LCons(y, ys)) =>
            let cmp = x <=> y;
            if (cmp == Comparison.EqualTo) (force xs) <=> (force ys) else cmp
    }
}

instance ToString[DelayList[a]] with ToString[a] {
    pub def toString(l: DelayList[a]): String = DelayList.toString(l)
}


instance Foldable[DelayList] {
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, l: DelayList[a]): b \ ef = DelayList.foldLeft(f, s, l)
    pub def foldRight(f: (a, b) -> b \ ef, s: b, l: DelayList[a]): b \ ef = DelayList.foldRight(f, s, l)
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, s: b, l: DelayList[a]): b \ ef = DelayList.foldRightWithCont(f, s, l)
    pub redef toDelayList(l: DelayList[a]): DelayList[a] = l
}

instance UnorderedFoldable[DelayList] {
    pub def foldMap(f: a -> b \ ef, l: DelayList[a]): b \ ef with CommutativeMonoid[b] = DelayList.foldMap(f, l)
    pub redef isEmpty(l: DelayList[a]): Bool = DelayList.isEmpty(l)
    pub redef exists(f: a -> Bool \ ef, l: DelayList[a]): Bool \ ef = DelayList.exists(f, l)
    pub redef forAll(f: a -> Bool \ ef, l: DelayList[a]): Bool \ ef = DelayList.forAll(f, l)
    pub redef memberOf(x: a, l: DelayList[a]): Bool with Eq[a] = DelayList.memberOf(x, l)
}

instance Functor[DelayList] {
    pub def map(f: a -> b \ ef, l: DelayList[a]): DelayList[b] \ ef = DelayList.map(f, l)
}

instance Applicative[DelayList] {
    pub def point(x: a): DelayList[a] = DelayList.singleton(x)
    pub def ap(f: DelayList[a -> b \ ef], l: DelayList[a]): DelayList[b] \ ef = DelayList.ap(f, l)
}

instance Monad[DelayList] {
    pub def flatMap(f: a -> DelayList[b] \ ef, l: DelayList[a]): DelayList[b] \ ef = DelayList.flatMap(f, l)
}

instance MonadZero[DelayList] {
    pub def empty(): DelayList[a] = DelayList.empty()
}

instance Traversable[DelayList] {
    pub def traverse(f: a -> m[b] \ ef, l: DelayList[a]): m[DelayList[b]] \ ef with Applicative[m] = DelayList.traverse(f, l)
    pub redef sequence(l: DelayList[m[a]]): m[DelayList[a]] with Applicative[m] = DelayList.sequence(l)
}

instance Filterable[DelayList] {
    pub def filterMap(f: a -> Option[b] \ ef, x: DelayList[a]): DelayList[b] \ ef = DelayList.filterMap(f, x)
    pub redef filter(f: a -> Bool \ ef, x: DelayList[a]): DelayList[a] \ ef = DelayList.filter(f, x)
}

instance Witherable[DelayList]

instance SemiGroup[DelayList[a]] {
    pub def combine(l1: DelayList[a], l2: DelayList[a]): DelayList[a] = DelayList.append(l1, l2)
}

instance Monoid[DelayList[a]] {
    pub def empty(): DelayList[a] = DelayList.ENil
}

instance Iterable[DelayList[a]] {
    type Elm = a
    pub def iterator(rc: Region[r], l: DelayList[a]): Iterator[a, r, r] \ r = DelayList.iterator(rc, l)
}

mod DelayList {

    ///
    /// Returns a string representation of `l`.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def toString(l: DelayList[a]): String with ToString[a] = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString!("DelayList(", sb);
        forEachWithIndex((i, x) -> {
            if (i == 0)
                StringBuilder.appendString!("${x}", sb)
            else
                StringBuilder.appendString!(", ${x}", sb)
            }, l);
        StringBuilder.appendString!(")", sb);
        StringBuilder.toString(sb)
    }

    ///
    /// Returns an empty DelayList.
    ///
    @Experimental
    pub def empty(): DelayList[a] = ENil

    ///
    /// Returns true if and only if `l` is the empty DelayList, i.e. `ENil`.
    ///
    /// Does not force the tail of `l`.
    ///
    @Experimental
    pub def isEmpty(l: DelayList[a]): Bool = match l {
        case ENil      => true
        case LList(xs) => isEmpty(force xs)
        case _         => false
    }

    ///
    /// Returns true if and only if `l` is a non-empty DelayList.
    ///
    /// Does not force the tail of `l`.
    ///
    @Experimental
    pub def nonEmpty(l: DelayList[a]): Bool = not isEmpty(l)

    ///
    /// Returns `Some(x)` if `x` is the first element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Does not force the tail of `l`.
    ///
    @Experimental
    pub def head(l: DelayList[a]): Option[a] = match l {
        case ENil        => None
        case ECons(x, _) => Some(x)
        case LCons(x, _) => Some(x)
        case LList(xs)   => head(force xs)
    }

    ///
    /// Returns `Some(x)` if `x` is the last element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def last(l: DelayList[a]): Option[a] = match l {
        case ENil         => None
        case ECons(x, xs) => if (isEmpty(      xs)) Some(x) else last(      xs)
        case LCons(x, xs) => if (isEmpty(force xs)) Some(x) else last(force xs)
        case LList(xs)    => last(force xs)
    }

    ///
    /// Returns `l` without the first element.
    ///
    /// Does not force the tail of `l`.
    ///
    @Experimental @Lazy
    pub def tail(l: DelayList[a]): DelayList[a] = match l {
        case ENil         => ENil
        case ECons(_, xs) => xs
        case LCons(_, xs) => LList(xs)
        case LList(xs)    => LList(lazy tail(force xs))
    }

    ///
    /// Returns the number of elements in `l`.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def length(l: DelayList[a]): Int32 =
        def loop(ll, acc) = match ll {
            case ENil         => acc
            case ECons(_, xs) => loop(      xs, acc + 1)
            case LCons(_, xs) => loop(force xs, acc + 1)
            case LList(xs)    => loop(force xs, acc)
        };
        loop(l, 0)

    ///
    /// Returns `l2` appended to `l1`.
    ///
    /// Does not force the tail of `l1`.
    ///
    @Experimental @Lazy
    pub def append(l1: DelayList[a], l2: DelayList[a]): DelayList[a] = match l1 {
        case ENil         => l2
        case ECons(x, xs) => LCons(x, lazy append(      xs, l2))
        case LCons(x, xs) => LCons(x, lazy append(force xs, l2))
        case LList(xs)    => LList(   lazy append(force xs, l2))
    }

    ///
    /// Returns the number of elements in `l` that satisfy the predicate `f`.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def count(f: a -> Bool \ ef, l: DelayList[a]): Int32 \ ef =
        foldLeft((i, x) -> if (f(x)) i + 1 else i, 0, l)

    ///
    /// Returns the sum of all elements in the DelayList `l`.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def sum(l: DelayList[Int32]): Int32 =
        Foldable.sum(l)

    ///
    /// Returns the sum of all elements in the DelayList `l` according to the function `f`.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def sumWith(f: a -> Int32 \ ef, l: DelayList[a]): Int32 \ ef =
        Foldable.sumWith(f ,l)

    ///
    /// Returns the concatenation of the elements in `l`.
    ///
    /// Does not force the tail of `l`.
    ///
    @Experimental @Lazy
    pub def flatten(l: DelayList[DelayList[a]]): DelayList[a] = match l {
        case ENil         => ENil
        case ECons(x, xs) => append(x, LList(lazy flatten(      xs)))
        case LCons(x, xs) => append(x, LList(lazy flatten(force xs)))
        case LList(xs)    => LList(lazy flatten(force xs))
    }

    ///
    /// Returns `true` if and only if at least one element in `l` satisfies the predicate `f`.
    ///
    /// Returns `false` if `l` is empty.
    ///
    /// Forces elements of `l` until the predicate `f` is satisfied.
    ///
    @Experimental
    pub def exists(f: a -> Bool \ ef, l: DelayList[a]): Bool \ ef = match l {
        case ENil         => false
        case ECons(x, xs) => if (f(x)) true else exists(f,       xs)
        case LCons(x, xs) => if (f(x)) true else exists(f, force xs)
        case LList(xs)    => exists(f, force xs)
    }

    ///
    /// Returns `true` if and only if all elements in `l` satisfy the predicate `f`.
    ///
    /// Returns `true` if `l` is empty.
    ///
    /// Forces elements in `l` until the first element that does not satisfy the predicate `f` (inclusive).
    ///
    @Experimental
    pub def forAll(f: a -> Bool \ ef, l: DelayList[a]): Bool \ ef = match l {
        case ENil         => true
        case ECons(x, xs) => if (f(x)) forAll(f,       xs) else false
        case LCons(x, xs) => if (f(x)) forAll(f, force xs) else false
        case LList(xs)    =>           forAll(f, force xs)
    }

    ///
    /// Returns `true` if and only if `l` contains the element `x`.
    ///
    /// Forces elements until `x` is found.
    ///
    @Experimental
    pub def memberOf(x: a, l: DelayList[a]): Bool with Eq[a] = match l {
        case ENil          => false
        case ECons(x1, xs) => if (x1 == x) true else memberOf(x,       xs)
        case LCons(x1, xs) => if (x1 == x) true else memberOf(x, force xs)
        case LList(xs)     =>                        memberOf(x, force xs)
    }

    ///
    /// Optionally finds the smallest element of `l` according to the `Order` on `a`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def minimum(l: DelayList[a]): Option[a] with Order[a] =
        reduceLeft(Order.min, l)

    ///
    /// Optionally finds the smallest element of `l` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def minimumBy(cmp: (a, a) -> Comparison, l: DelayList[a]): Option[a] =
        reduceLeft(Order.minBy(cmp), l)

    ///
    /// Optionally finds the largest element of `l` according to the `Order` on `a`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def maximum(l: DelayList[a]): Option[a] with Order[a] =
        reduceLeft(Order.max, l)

    ///
    /// Optionally finds the largest element of `l` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def maximumBy(cmp: (a, a) -> Comparison, l: DelayList[a]): Option[a] =
        reduceLeft(Order.maxBy(cmp), l)

    ///
    /// Returns a `DelayList` of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty `DelayList` if `b >= e`.
    ///
    @Experimental @Lazy
    pub def range(b: Int32, e: Int32): DelayList[Int32] =
        def loop(i) = {
            if (i >= e)
                ENil
            else
                LCons(i, lazy loop(i + 1))
        };
        LList(lazy loop(b))

    ///
    /// Returns an infinite DelayList of repeating `x`s.
    ///
    @Experimental @Lazy
    pub def repeat(x: a): DelayList[a] =
        LCons(x, lazy repeat(x))

    ///
    /// Returns an infinite sequence of integers starting from and including `n`.
    ///
    @Experimental @Lazy
    pub def startFrom(n: Int32): DelayList[Int32] =
        def loop(i) = LCons(i, lazy loop(i + 1));
        LList(lazy loop(n))

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    @Experimental @LazyWhenPure
    pub def map(f: a -> b \ ef, l: DelayList[a]): DelayList[b] \ ef =
        match purityOf(f) {
            case Purity.Pure(g)   => mapL(g, l)
            case Purity.Impure(g) => mapE(g, l)
        }

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    @Lazy
    def mapL(f: a -> b, l: DelayList[a]): DelayList[b] = match l {
        case ENil         => ENil
        case ECons(x, xs) => LCons(f(x), lazy mapL(f,       xs))
        case LCons(x, xs) => LCons(f(x), lazy mapL(f, force xs))
        case LList(xs)    => LList(      lazy mapL(f, force xs))
    }

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def mapE(f: a -> b \ ef, l: DelayList[a]): DelayList[b] \ ef =
        def loop(ll, k) = match ll {
            case ENil => k(ENil)
            case ECons(x, xs) =>
                let x1 = f(x);
                loop(xs, ks -> k(ECons(x1, ks)))
            case LCons(x, xs) =>
                let x1 = f(x);
                loop(force xs, ks -> k(ECons(x1, ks)))
            case LList(xs) =>
                loop(force xs, k)
        };
        loop(l, identity)

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
    @Experimental @LazyWhenPure
    pub def mapWithIndex(f: (Int32, a) -> b \ ef, l: DelayList[a]): DelayList[b] \ ef =
        match purityOf2(f) {
            case Purity2.Pure(g)   => mapWithIndexL(g, l)
            case Purity2.Impure(g) => mapWithIndexE(g, l)
        }

    ///
    /// Returns the result of applying `f` to every element in `l` along with the element's index.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    @Lazy
    def mapWithIndexL(f: (Int32, a) -> b, l: DelayList[a]): DelayList[b] =
        def loop(ll, i) = match ll {
            case ENil         => ENil
            case ECons(x, xs) => LCons(f(i, x), lazy loop(      xs, i + 1))
            case LCons(x, xs) => LCons(f(i, x), lazy loop(force xs, i + 1))
            case LList(xs)    => LList(         lazy loop(force xs, i    ))
        };
        LList(lazy loop(l, 0))

    ///
    /// Returns the result of applying `f` to every element in `l` along with the element's index.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def mapWithIndexE(f: (Int32, a) -> b \ ef, l: DelayList[a]): DelayList[b] \ ef =
        def loop(ll, i, k) = match ll {
            case ENil => k(ENil)
            case ECons(x, xs) =>
                let x1 = f(i, x);
                loop(xs, i + 1, ks -> k(ECons(x1, ks)))
            case LCons(x, xs) =>
                let x1 = f(i, x);
                loop(force xs, i + 1, ks -> k(ECons(x1, ks)))
            case LList(xs) => loop(force xs, i, k)
        };
        loop(l, 0, identity)

    ///
    /// Returns the result of applying `f` to every element in `l` and concatenating the results.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    @Experimental @LazyWhenPure
    pub def flatMap(f: a -> DelayList[b] \ ef, l: DelayList[a]): DelayList[b] \ ef =
        match purityOf(f) {
            case Purity.Pure(g)   => flatMapL(g, l)
            case Purity.Impure(g) => flatMapE(g, l)
        }

    ///
    /// Returns the result of applying `f` to every element in `l` and concatenating the results.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    @Lazy
    def flatMapL(f: a -> DelayList[b], l: DelayList[a]): DelayList[b] = match l {
        case ENil         => ENil
        case ECons(x, xs) => append(f(x), LList(lazy flatMapL(f,       xs)))
        case LCons(x, xs) => append(f(x), LList(lazy flatMapL(f, force xs)))
        case LList(xs)    =>              LList(lazy flatMapL(f, force xs))
    }

    ///
    /// Returns the result of applying `f` to every element in `l` and concatenating the results.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def flatMapE(f: a -> DelayList[b] \ ef, l: DelayList[a]): DelayList[b] \ ef =
        def loop(ll, k) = match ll {
            case ENil => k(ENil)
            case ECons(x, xs) =>
                let xs1 = f(x);
                loop(xs, ks -> k(append(xs1, ks)))
            case LCons(x, xs) =>
                let xs1 = f(x);
                loop(force xs, ks -> k(append(xs1, ks)))
            case LList(xs) => loop(force xs, k)
        };
        loop(l, identity)

    ///
    /// Return the singleton list with element `x`.
    ///
    @Experimental
    pub def singleton(x: a): DelayList[a] = ECons(x, ENil)

    ///
    /// Apply every function from `f` to every argument from `l` and return a list with all results.
    /// For `f = f1, f2, ...` and `l = x1, x2, ...` the results appear in the order
    /// `f1(x1), f1(x2), ..., f2(x1), f2(x2), ...`.
    ///
    /// Whether the i-th function in `f` (`fi`) is applied eagerly or lazily depends on its purity:
    ///
    /// - If `fi` is pure then it is applied lazily (i.e. the tail of `l` is not forced).
    /// - If `fi` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    /// Note that this implies that ALL functions in `f` must be pure to avoid forcing `l`.
    ///
    @Experimental @LazyWhenPure
    pub def ap(f: DelayList[a -> b \ ef], l: DelayList[a]): DelayList[b] \ ef =
        flatMap(g -> map(g, l), f)

    ///
    /// Reverses the list `l`.
    ///
    /// Does not force the tail of `l`.
    ///
    @Experimental @Lazy
    pub def reverse(l: DelayList[a]): DelayList[a] =
        def loop(ll, acc) = match ll {
            case ENil         => acc
            case ECons(x, xs) => loop(      xs, ECons(x, acc))
            case LCons(x, xs) => loop(force xs, ECons(x, acc))
            case LList(xs)    => loop(force xs,          acc)
        };
        LList(lazy loop(l, ENil))

    ///
    /// Returns `l` with every occurrence of `src` replaced by `dst`.
    ///
    /// Does not force the tail of `l`.
    ///
    @Experimental @Lazy
    pub def replace(src: {src = a}, dst: {dst = a}, l: DelayList[a]): DelayList[a] with Eq[a] =
        map(e -> if (src#src == e) dst#dst else e, l)

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, l: DelayList[a]): b \ ef = match l {
        case ENil         => s
        case ECons(x, xs) => foldLeft(f, f(s, x),       xs)
        case LCons(x, xs) => foldLeft(f, f(s, x), force xs)
        case LList(xs)    => foldLeft(f,   s,     force xs)
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def foldRight(f: (a, b) -> b \ ef, s: b, l: DelayList[a]): b \ ef =
        def loop(ll, k) = match ll {
            case ENil         => k(s)
            case ECons(x, xs) => loop(      xs, ks -> k(f(x, ks)))
            case LCons(x, xs) => loop(force xs, ks -> k(f(x, ks)))
            case LList(xs)    => loop(force xs, k)
        };
        loop(l, eidentity)

    ///
    /// Applies `f` to a start value `z` and all elements in `l` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    ///
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    /// Calling the continuation forces the list `l`.
    ///
    @Experimental
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, z: b, l: DelayList[a]): b \ ef =
        def loop(ll) = match ll {
            case ENil         => z
            case ECons(x, xs) => f(x, _ -> loop(xs))
            case LCons(x, xs) => f(x, _ -> loop(force xs))
            case LList(xs)    =>           loop(force xs)
        };
        loop(l)

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, l: DelayList[a]): b \ ef with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), l)

    ///
    /// Applies `f` to every element of `l`.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def forEach(f: a -> Unit \ ef, l: DelayList[a]): Unit \ ef = match l {
        case ENil         => ()
        case ECons(x, xs) => f(x); forEach(f,       xs)
        case LCons(x, xs) => f(x); forEach(f, force xs)
        case LList(xs)    =>       forEach(f, force xs)
    }

    ///
    /// Applies `f` to every element of `l` along with that element's index.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def forEachWithIndex(f: (Int32, a) -> Unit \ ef, l: DelayList[a]): Unit \ ef = region rc {
        let ix = Ref.fresh(rc, 0);
        let f1 = x -> { let i = Ref.get(ix); f(i, x); Ref.put(i + 1, ix) };
        forEach(f1, l)
    }

    ///
    /// Applies `f` to all elements in `l` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def reduceLeft(f: (a, a) -> a \ ef, l: DelayList[a]): Option[a] \ ef = match l {
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
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def reduceRight(f: (a, a) -> a \ ef, l: DelayList[a]): Option[a] \ ef =
        def loop(ll, k) = match ll {
            case ECons(x, xs) => if (isEmpty(      xs)) k(x) else loop(      xs, ks -> k(f(x, ks)))
            case LCons(x, xs) => if (isEmpty(force xs)) k(x) else loop(force xs, ks -> k(f(x, ks)))
            case LList(xs)    => loop(force xs, k)
            case _            => unreachable!()
        };
        if (isEmpty(l)) None else Some(loop(l, eidentity))

    ///
    /// Returns a `DelayList` with every element in `l` that satisfies the predicate `f`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    @Experimental @LazyWhenPure
    pub def filter(f: a -> Bool \ ef, l: DelayList[a]): DelayList[a] \ ef =
        match purityOf(f) {
            case Purity.Pure(g)   => filterL(g, l)
            case Purity.Impure(g) => filterE(g, l)
        }

    ///
    /// Returns a `DelayList` with every element in `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    @Lazy
    def filterL(f: a -> Bool, l: DelayList[a]): DelayList[a] = match l {
        case ENil         => ENil
        case ECons(x, xs) => if (f(x)) LCons(x, lazy filterL(f,       xs)) else LList(lazy filterL(f,       xs))
        case LCons(x, xs) => if (f(x)) LCons(x, lazy filterL(f, force xs)) else LList(lazy filterL(f, force xs))
        case LList(xs)    =>              LList(lazy filterL(f, force xs))
    }

    ///
    /// Returns a `DelayList` with every element in `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def filterE(f: a -> Bool \ ef, l: DelayList[a]): DelayList[a] \ ef =
        def loop(ll, k) = match ll {
            case ENil         => k(ENil)
            case ECons(x, xs) => if (f(x)) loop(      xs, ks -> k(ECons(x, ks))) else loop(      xs, k)
            case LCons(x, xs) => if (f(x)) loop(force xs, ks -> k(ECons(x, ks))) else loop(force xs, k)
            case LList(xs)    => loop(force xs, k)
        };
        loop(l, identity)

    ///
    /// Collects the results of applying the partial function `f` to every element in `l`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    @Experimental @LazyWhenPure
    pub def filterMap(f: a -> Option[b] \ ef, l: DelayList[a]): DelayList[b] \ ef =
        match purityOf(f) {
            case Purity.Pure(g)   => filterMapL(g, l)
            case Purity.Impure(g) => filterMapE(g, l)
        }

    ///
    /// Helper function for `filterMap`.
    ///
    /// Collects the results of applying the partial function `f` to every element in `l`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    @Lazy
    def filterMapL(f: a -> Option[b], l: DelayList[a]): DelayList[b] =
        def loop(ll) = match ll {
            case ENil         => ENil
            case ECons(x, xs) =>
                match f(x) {
                    case None    => loop(xs)
                    case Some(v) => LCons(v, lazy loop(xs))
                }
            case LCons(x, xs) => // Same as above except `xs` is forced.
                match f(x) {
                    case None    => loop(force xs)
                    case Some(v) => LCons(v, lazy loop(force xs))
                }
            case LList(xs) => LList(lazy loop(force xs))
        };
        LList(lazy loop(l))

    ///
    /// Helper function for `filterMap`.
    ///
    /// Collects the results of applying the partial function `f` to every element in `l`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def filterMapE(f: a -> Option[b] \ ef, l: DelayList[a]): DelayList[b] \ ef =
        def loop(ll, k) = match ll {
            case ENil         => k(ENil)
            case ECons(x, xs) => match f(x) {
                case None    => loop(xs, k)
                case Some(v) => loop(xs, ks -> k(ECons(v, ks)))
            }
            case LCons(x, xs) => match f(x) { // Same as above except `xs` is forced.
                case None    => loop(force xs, k)
                case Some(v) => loop(force xs, ks -> k(ECons(v, ks)))
            }
            case LList(xs) => loop(force xs, k)
        };
        loop(l, identity)

    ///
    /// Optionally returns the first element of `l` that satisfies the predicate `f` when searching from left to right.
    ///
    /// Forces elements of `l` until the predicate `f` is satisfied.
    ///
    @Experimental
    pub def findLeft(f: a -> Bool \ ef, l: DelayList[a]): Option[a] \ ef = match l {
        case ENil         => None
        case ECons(x, xs) => if (f(x)) Some(x) else findLeft(f,       xs)
        case LCons(x, xs) => if (f(x)) Some(x) else findLeft(f, force xs)
        case LList(xs)    =>                        findLeft(f, force xs)
    }

    ///
    /// Optionally returns the first element of `l` that satisfies the predicate `f` when searching from right to left.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def findRight(f: a -> Bool \ ef, l: DelayList[a]): Option[a] \ ef =
        def loop(ll, k) = match ll {
            case ENil         => k()
            case ECons(x, xs) => loop(      xs, () -> if (f(x)) Some(x) else k())
            case LCons(x, xs) => loop(force xs, () -> if (f(x)) Some(x) else k())
            case LList(xs)    => loop(force xs, k)
        };
        loop(l, constant(None))

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `l`.
    ///
    /// Returns `None` if every element `f(x)` of `l` is `None`.
    ///
    /// Forces elements of `l` until `f(x)` returns `Some(v)`.
    ///
    @Experimental
    pub def findMap(f: a -> Option[b] \ ef, l: DelayList[a]): Option[b] \ ef = match l {
        case ENil         => None
        case ECons(x, xs) => match f(x) {
            case None    => findMap(f, xs)
            case Some(v) => Some(v)
        }
        case LCons(x, xs) => match f(x) { // Same as above except `xs` is forced.
            case None    => findMap(f, force xs)
            case Some(v) => Some(v)
        }
        case LList(xs) => findMap(f, force xs)
    }

    ///
    /// Returns `l` with `x` inserted between every two adjacent elements.
    ///
    /// Does not force the tail of `l`.
    ///
    @Experimental @Lazy
    pub def intersperse(x: a, l: DelayList[a]): DelayList[a] = match l {
        case ENil           => ENil
        case ECons(x1, xs)  => if (isEmpty(      xs)) l else LCons(x1, lazy LCons(x, lazy intersperse(x,       xs)))
        case LCons(x1, xs)  => if (isEmpty(force xs)) l else LCons(x1, lazy LCons(x, lazy intersperse(x, force xs)))
        case LList(xs)      => LList(lazy intersperse(x, force xs))
    }

    ///
    /// Returns the concatenation of the elements in `l2` with the elements
    /// of `l1` inserted between every two adjacent elements of `l2`.
    ///
    /// That is, returns `l2.1 :: l1.1 ... l1.n :: l2.2 :: ... :: l2.n-1 :: l1.1 :: ... :: l1.n :: l2.n :: ENil`.
    ///
    /// Does not force the tail of `l2`.
    ///
    @Experimental @Lazy
    pub def intercalate(l1: DelayList[a], l2: DelayList[DelayList[a]]): DelayList[a] = match l2 {
        case ENil           => ENil
        case ECons(x, xs)   => if (isEmpty(      xs)) x else append(append(x, l1), intercalate(l1,       xs))
        case LCons(x, xs)   => if (isEmpty(force xs)) x else append(append(x, l1), intercalate(l1, force xs))
        case LList(xs)      =>                                          LList(lazy intercalate(l1, force xs))
    }

    ///
    /// Returns a pair of lists `(l1, l2)` where:
    /// - `l1` contains all elements of `l` that satisfy the predicate `f`.
    /// - `l2` contains all elements of `l` that DO NOT satisfy the predicate `f`.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def partition(f: a -> Bool \ ef, l: DelayList[a]): (DelayList[a], DelayList[a]) \ ef =
        def loop(ll, k) = match ll {
            case ENil         => k((ENil, ENil))
            case ECons(x, xs) =>
                if (f(x))
                    loop(xs, match (ks, ls) -> k((ECons(x, ks), ls)))
                else
                    loop(xs, match (ks, ls) -> k((ks, ECons(x, ls))))
            case LCons(x, xs) => // Same as above except `xs` is forced.
                if (f(x))
                    loop(force xs, match (ks, ls) -> k((ECons(x, ks), ls)))
                else
                    loop(force xs, match (ks, ls) -> k((ks, ECons(x, ls))))
            case LList(xs) => loop(force xs, k)
        };
        loop(l, identity)

    ///
    /// Returns a pair of lists `(l1, l2)` where:
    /// - `l1` is the longest prefix of `l` that satisfies the predicate `f`.
    /// - `l2` is the remainder of `l`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    @Experimental @LazyWhenPure
    pub def span(f: a -> Bool \ ef, l: DelayList[a]): (DelayList[a], DelayList[a]) \ ef =
        match purityOf(f) {
            case Purity.Pure(g)   => spanL(g, l)
            case Purity.Impure(g) => spanE(g, l)
        }

    ///
    /// Helper function for `span`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    @Lazy
    def spanL(f: a -> Bool, l: DelayList[a]): (DelayList[a], DelayList[a]) = match l {
        case ENil         => (ENil, ENil)
        case ECons(x, xs) =>
            if (f(x))
                let t = lazy spanL(f, xs);
                (LCons(x, lazy fst(force t)), LList(lazy snd(force t)))
            else
                (ENil, l)
        case LCons(x, xs) => // Same as above except `xs` is forced.
            if (f(x))
                let t = lazy spanL(f, force xs);
                (LCons(x, lazy fst(force t)), LList(lazy snd(force t)))
            else
                (ENil, l)
        case LList(xs) => spanL(f, force xs)
    }

    ///
    /// Helper function for `span`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def spanE(f: a -> Bool \ ef, l: DelayList[a]): (DelayList[a], DelayList[a]) \ ef =
        def loop(ll, k) = match ll {
            case ENil         => k((ENil, ENil))
            case ECons(x, xs) =>
                if (f(x))
                    loop(xs, match (ks, ls) -> k((ECons(x, ks), ls)))
                else
                    k((ENil, l))
            case LCons(x, xs) => // Same as above except `xs` is forced.
                if (f(x))
                    loop(force xs, match (ks, ls) -> k((ECons(x, ks), ls)))
                else
                    k((ENil, l))
            case LList(xs) => loop(force xs, k)
        };
        loop(l, identity)

    ///
    /// Returns `l` without the first `n` elements.
    ///
    /// Returns `ENil` if `n > length(l)`.
    /// Returns `l` if `n < 1`.
    ///
    /// Does not force the tail of `l`.
    ///
    @Experimental @Lazy
    pub def drop(n: Int32, l: DelayList[a]): DelayList[a] =
        def loop(i, ll) = { // Inner function used here to allow for early termination
            if (i < 1)
                ll
            else
                match ll {
                    case ENil         => ll
                    case ECons(_, xs) => loop(i - 1,       xs)
                    case LCons(_, xs) => loop(i - 1, force xs)
                    case LList(xs)    => loop(i,     force xs)
                }
        };
        LList(lazy loop(n, l))

    ///
    /// Returns `l` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the tail is forced until the first element that satisfies `f`).
    ///
    @Experimental @LazyWhenPure
    pub def dropWhile(f: a -> Bool \ ef, l: DelayList[a]): DelayList[a] \ ef =
        match purityOf(f) {
            case Purity.Pure(g)   => dropWhileL(g, l)
            case Purity.Impure(g) => dropWhileE(g, l)
        }

    ///
    /// Helper function for `dropWhile`.
    ///
    /// Returns `l` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    @Lazy
    def dropWhileL(f: a -> Bool, l: DelayList[a]): DelayList[a] =
        def loop(ll) = match ll { // Inner function used here to allow for early termination
            case ENil         => ENil
            case ECons(x, xs) => if (f(x)) loop(      xs) else ll
            case LCons(x, xs) => if (f(x)) loop(force xs) else ll
            case LList(xs)    => loop(force xs)
        };
        LList(lazy loop(l))

    ///
    /// Helper function for `dropWhile`.
    ///
    /// Returns `l` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Applies `f` eagerly (i.e. the tail is forced until the first element that satisfies `f`).
    ///
    def dropWhileE(f: a -> Bool \ ef, l: DelayList[a]): DelayList[a] \ ef = match l {
        case ENil         => ENil
        case ECons(x, xs) => if (f(x)) dropWhileE(f,       xs) else l
        case LCons(x, xs) => if (f(x)) dropWhileE(f, force xs) else l
        case LList(xs)    =>           dropWhileE(f, force xs)
    }

    ///
    /// Returns the first `n` elements of `l`.
    ///
    /// Does not force the tail of `l`.
    ///
    @Experimental @Lazy
    pub def take(n: Int32, l: DelayList[a]): DelayList[a] =
        def loop(i, ll) = { // Inner function used here to allow for early termination
            if (i <= 0)
                ENil
            else
                match ll {
                    case ENil         => ENil
                    case ECons(x, xs) => LCons(x, lazy loop(i - 1,       xs))
                    case LCons(x, xs) => LCons(x, lazy loop(i - 1, force xs))
                    case LList(xs)    => loop(i, force xs)
                }
        };
        LList(lazy loop(n, l))

    ///
    /// Returns the longest prefix of `l` that satisfies the predicate `f`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the tail is forced until the first element that satisfies `f`).
    ///
    @Experimental @LazyWhenPure
    pub def takeWhile(f: a -> Bool \ ef, l: DelayList[a]): DelayList[a] \ ef =
        match purityOf(f) {
            case Purity.Pure(g)   => takeWhileL(g, l)
            case Purity.Impure(g) => takeWhileE(g, l)
        }

    ///
    /// Helper function for `takeWhile`.
    ///
    /// Returns the longest prefix of `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    @Lazy
    def takeWhileL(f: a -> Bool, l: DelayList[a]): DelayList[a] =
        def loop(ll) = match ll { // Inner function used here to allow for early termination
            case ENil         => ENil
            case ECons(x, xs) => if (f(x)) LCons(x, lazy loop(      xs)) else ENil
            case LCons(x, xs) => if (f(x)) LCons(x, lazy loop(force xs)) else ENil
            case LList(xs)    => loop(force xs)
        };
        LList(lazy loop(l))

    ///
    /// Helper function for `takeWhile`.
    ///
    /// Returns the longest prefix of `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` eagerly (i.e. the tail is forced until the first element that satisfies `f`).
    ///
    def takeWhileE(f: a -> Bool \ ef, l: DelayList[a]): DelayList[a] \ ef =
        def loop(ll, k) = match ll {
            case ENil         => k(ENil)
            case ECons(x, xs) => if (f(x)) loop(      xs, ks -> k(ECons(x, ks))) else k(ENil)
            case LCons(x, xs) => if (f(x)) loop(force xs, ks -> k(ECons(x, ks))) else k(ENil)
            case LList(xs)    => loop(force xs, k)
        };
        loop(l, identity)

    ///
    /// Returns a list where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` is depleted, then no further elements are added to the resulting list.
    ///
    /// Does not force the tail of either `l1` or `l2`.
    ///
    @Experimental @Lazy
    pub def zip(l1: DelayList[a], l2: DelayList[b]): DelayList[(a, b)] =
        def loop(ll1, ll2) = match (ll1, ll2) { // Inner function used here to allow for early termination
            case (ENil, _) => ENil
            case (_, ENil) => ENil
            case (ECons(x, xs), ECons(y, ys)) => LCons((x, y), lazy loop(      xs,       ys))
            case (ECons(x, xs), LCons(y, ys)) => LCons((x, y), lazy loop(      xs, force ys))
            case (LCons(x, xs), ECons(y, ys)) => LCons((x, y), lazy loop(force xs,       ys))
            case (LCons(x, xs), LCons(y, ys)) => LCons((x, y), lazy loop(force xs, force ys))
            case (LList(xs), LList(ys))       => LList(        lazy loop(force xs, force ys))
            case (xs, LList(ys))              => LList(        lazy loop(      xs, force ys))
            case (LList(xs), ys)              => LList(        lazy loop(force xs,       ys))
        };
        match (l1, l2) {
            case (ENil, _) => ENil
            case (_, ENil) => ENil
            case _         => LList(lazy loop(l1, l2))
        }

    ///
    /// Returns a list where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` is depleted, then no further elements are added to the resulting list.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tails are not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. both lists `l1` and `l2` are forced).
    ///
    @Experimental @LazyWhenPure
    pub def zipWith(f: (a, b) -> c \ ef, l1: DelayList[a], l2: DelayList[b]): DelayList[c] \ ef =
        map(x -> f(fst(x), snd(x)), zip(l1, l2))

    ///
    /// Returns a `DelayList` where each element `e` is mapped to `(i, e)` where `i`
    /// is the index of `e`.
    ///
    /// Does not force the tail of `l`.
    ///
    pub def zipWithIndex(l: DelayList[a]): DelayList[(Int32, a)] =
        def loop(ll, i) = match ll {
            case ENil         => ENil
            case ECons(x, xs) => LCons(((i, x)), lazy loop(      xs, i + 1))
            case LCons(x, xs) => LCons(((i, x)), lazy loop(force xs, i + 1))
            case LList(xs)    => LList(          lazy loop(force xs, i))
        };
        loop(l, 0)

    ///
    /// Returns `l` as an `Array`.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def toArray(rc: Region[r], l: DelayList[a]): Array[a, r] \ r =
        let a = Array.empty(rc, length(l));
        forEach(match (i, y) -> Array.put(y, i, a), zipWithIndex(l));
        a

    ///
    /// Returns `l` as a Vector.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def toVector(l: DelayList[a]): Vector[a] = region rc {
        let arr = Array.empty(rc, length(l));
        forEach(match (i, x) -> Array.put(x, i, arr), zipWithIndex(l));
        Array.toVector(arr)
    }

    ///
    /// Returns `l` as an `Iterator`.
    ///
    /// Does not force any elements of the list.
    ///
    @Experimental @Lazy
    pub def iterator(rc: Region[r], l: DelayList[a]): Iterator[a, r, r] \ r =
        let cursor = Ref.fresh(rc, l);
        let next = () -> match head(Ref.get(cursor)) {
            case None    => None
            case Some(x) =>
                Ref.put(tail(Ref.get(cursor)), cursor);
                Some(x)
        };
        Iterator.iterate(rc, next)

    ///
    /// Returns an iterator over `l` zipped with the indices of the elements.
    ///
    pub def enumerator(rc: Region[r], l: DelayList[a]): Iterator[(Int32, a), r, r] \ r =
        iterator(rc, l) |> Iterator.zipWithIndex

    ///
    /// Returns `l` as a `List`.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def toList(l: DelayList[a]): List[a] =
        def loop(ll, k) = match ll {
            case ENil         => k(Nil)
            case ECons(x, xs) => loop(      xs, ks -> k(x :: ks))
            case LCons(x, xs) => loop(force xs, ks -> k(x :: ks))
            case LList(xs)    => loop(force xs, k)
        };
        loop(l, identity)

    ///
    /// Returns `l` as a `List`.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def toMutList(rc: Region[r], l: DelayList[a]): MutList[a, r] \ r = region rc2 {
        Array.toMutList(rc, toArray(rc2, l)) // `Array.toMutList` respects the invariant of `MutList`.
    }

    ///
    /// Returns `l` as a MutDeque.
    ///
    pub def toMutDeque(rc: Region[r], l: DelayList[a]): MutDeque[a, r] \ r =
        let d = MutDeque.empty(rc);
        forEach(x -> MutDeque.pushBack(x, d), l);
        d

    ///
    /// Returns the association list `l` as a map.
    ///
    /// If `l` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def toMap(l: DelayList[(a, b)]): Map[a, b] with Order[a] =
        def loop(ll, acc) = match ll {
            case ENil              => acc
            case ECons((k, v), xs) => loop(      xs, Map.insert(k, v, acc))
            case LCons((k, v), xs) => loop(force xs, Map.insert(k, v, acc))
            case LList(xs)         => loop(force xs, acc)
        };
        loop(l, Map.empty())

    ///
    /// Returns `l` as a `Set`.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def toSet(l: DelayList[a]): Set[a] with Order[a] =
        def loop(ll, acc) = match ll {
            case ENil         => acc
            case ECons(x, xs) => loop(      xs, Set.insert(x, acc))
            case LCons(x, xs) => loop(force xs, Set.insert(x, acc))
            case LList(xs)    => loop(force xs, acc)
        };
        loop(l, Set.empty())

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `l` with `sep` inserted between each element.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def join(sep: String, l: DelayList[a]): String with ToString[a] =
        Foldable.join(sep, l)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `l` according to `f` with `sep` inserted between each element.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def joinWith(f: a -> String \ ef, sep: String, l: DelayList[a]): String \ ef =
        Foldable.joinWith(f, sep, l)

    ///
    /// Helper function for `traverse` and `sequence`.
    ///
    /// Builds an "applicative DelayList" from a head of one applicative action and an
    /// applicative DelayList of the tail.
    ///
    @Experimental
    def consA(mx: f[a], ml: f[DelayList[a]]): f[DelayList[a]] with Applicative[f] =
        (((x, xs) -> ECons(x, xs)) `Functor.map` mx) `Applicative.ap` ml

    ///
    /// Returns the result of running all the actions in the DelayList `l`.
    ///
    @Experimental
    pub def sequence(l: DelayList[m[a]]): m[DelayList[a]] with Applicative[m] =
        def loop(ll, k) = match ll {
            case ENil          => k(Applicative.point(ENil))
            case ECons(mx, xs) => loop(      xs, ks -> k(consA(mx, ks)))
            case LCons(mx, xs) => loop(force xs, ks -> k(consA(mx, ks)))
            case LList(xs)     => loop(force xs, k)
        };
        loop(l, identity)

    ///
    /// Returns the result of applying the applicative mapping function `f` to all the elements of the
    /// DelayList `l`.
    ///
    @Experimental
    pub def traverse(f: a -> m[b] \ ef, l: DelayList[a]): m[DelayList[b]] \ ef with Applicative[m] =
        def loop(ll, k) = match ll {
            case ENil         => k(Applicative.point(ENil))
            case ECons(x, xs) => {let ans = f(x); loop(xs, ks -> k(consA(ans, ks)))}
            case LCons(x, xs) => {let ans = f(x); loop(force xs, ks -> k(consA(ans, ks)))}
            case LList(xs)    => loop(force xs, k)
        };
        loop(l, identity)

    ///
    /// Shuffles `l` using the Fisher–Yates shuffle.
    ///
    pub def shuffle(rnd: Random, l: DelayList[a]): DelayList[a] \ IO = region rc {
        toArray(rc, l) !> Array.shuffle(rnd) |> Array.toDelayList
    }

}
