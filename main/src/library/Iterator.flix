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


///
/// The type of the done function.
///
type alias Done = Unit -> Bool & Impure

///
/// The type of the next function.
///
type alias Next[a] = Unit -> a & Impure

///
/// An iterator is a stream of elements that are produced on demand.
///
pub enum Iterator[a] {
    case Iterator(Done, Next[a])
}

///
/// The type of the done function.
///
type alias RegionDone[ef: Region] = Unit -> Bool & ef

///
/// The type of the next function.
///
type alias RegionNext[a: Type, ef: Region] = Unit -> a & ef


pub enum RegionIterator[a: Type, r: Region] {
    case RegionIterator(RegionDone[r], RegionNext[a, r])
}

instance Newable[RegionIterator[a]] {
    pub def new(r: Region[r]): RegionIterator[a, r] \ { Read(r), Write(r) } = Iterator.new(r)
}

namespace Iterator {

    ///
    /// Returns an empty iterator.
    ///
    pub def new(_: Region[r]): RegionIterator[a, r] \ { Read(r), Write(r) } =
        let done = () -> true as \ Read(r);
        let next = () -> bug!("Empty Iterator!") as \ Read(r);
        RegionIterator(done, next) as \ { Read(r), Write(r) }

    ///
    /// Returns an empty iterator.
    ///
    pub def empty(): Iterator[a] =
        let done = () -> true as & Impure;
        let next = () -> bug!("Empty Iterator!") as & Impure;
        Iterator(done, next)

    ///
    /// Returns an iterator containing only a single element, `x`.
    ///
    pub def singleton(x: a): Iterator[a] & Impure =
        let d = ref false;
        let done = () -> deref d;
        let next = () -> {
            if (done()) {
                bug!("Empty Iterator!")
            }
            else {
                d := true;
                x
            }
        };
        Iterator(done, next)

    ///
    /// Returns an iterator containing only a single element, `x`.
    ///
    pub def regSingleton(r: Region[r], x: a): RegionIterator[a, r] \ Write(r) =
        let d = ref false @ r;
        let done = () -> deref d;
        let next = () -> {
            if (done()) {
                bug!("Empty Iterator!")
            }
            else {
                d := true;
                x
            }
        };
        RegionIterator(done, next)

    ///
    /// Returns `Some(x)` if `iter` is not empty. Returns `None` otherwise.
    ///
    /// Consumes the head element of `iter`.
    ///
    pub def next(iter: Iterator[a]): Option[a] & Impure =
        let Iterator(done, next) = iter;
        if (done())
            None
        else
            Some(next())

    ///
    /// Returns `Some(x)` if `iter` is not empty. Returns `None` otherwise.
    ///
    /// Consumes the head element of `iter`.
    ///
    pub def regNext(iter: RegionIterator[a, r]): Option[a] \ Read(r) =
        let RegionIterator(done, next) = iter;
        if (done())
            None
        else
            Some(next())

    ///
    /// Returns true if the iterator is empty.
    ///
    /// Does **not** consume any elements of the iterator.
    ///
    pub def isEmpty(iter: Iterator[a]): Bool & Impure =
        let Iterator(done, _) = iter;
        done()

    ///
    /// Returns true if the iterator is empty.
    ///
    /// Does **not** consume any elements of the iterator.
    ///
    pub def regIsEmpty(iter: RegionIterator[a, r]): Bool \ Read(r) =
        let RegionIterator(done, _) = iter;
        done()

    ///
    /// Returns an iterator of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty iterator if `b >= e`.
    ///
    @Lazy
    pub def range(b: Int32, e: Int32): Iterator[Int32] & Impure =
        let i = ref b;
        let done = () -> deref i >= e;
        let next = () -> {
                let res = deref i;
                if (res < e) {
                    i := res + 1;
                    res
                }
                else {
                    bug!("Empty Iterator!")
                }
            };
        Iterator(done, next)

    ///
    /// Returns an iterator of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty iterator if `b >= e`.
    ///
    @Lazy
    pub def regRange(r: Region[r], b: Int32, e: Int32): RegionIterator[Int32, r] \ Write(r) =
        let i = ref b @ r;
        let done = () -> deref i >= e;
        let next = () -> {
                let res = deref i;
                if (res < e) {
                    i := res + 1;
                    res
                }
                else {
                    bug!("Empty Iterator!")
                }
            };
        RegionIterator(done, next)

    ///
    /// Returns an iterator over an iterable with the element `x` repeated `n` times.
    ///
    /// Returns an empty iterator if `n < 0`.
    ///
    @Lazy
    pub def repeat(n: Int32, x: a): Iterator[a] & Impure =
        let i = ref 0;
        let done = () -> deref i >= n;
        let next = () -> {
              let j = deref i;
              if (j < n) {
                i := j + 1;
                x
              }
              else {
                bug!("Empty Iterator!")
              }
            };
        Iterator(done, next)

    ///
    /// Returns an iterator over an iterable with the element `x` repeated `n` times.
    ///
    /// Returns an empty iterator if `n < 0`.
    ///
    @Lazy
    pub def regRepeat(r: Region[r], n: Int32, x: a): RegionIterator[a, r] \ Write(r) =
        let i = ref 0 @ r;
        let done = () -> deref i >= n;
        let next = () -> {
              let j = deref i;
              if (j < n) {
                i := j + 1;
                x
              }
              else {
                bug!("Empty Iterator!")
              }
            };
        RegionIterator(done, next)

    ///
    /// Returns the sum of all elements in the iterator `iter`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def sum(iter: Iterator[Int32]): Int32 & Impure =
        foldLeft((acc, x) -> acc + x, 0, iter)

    ///
    /// Returns the sum of all elements in the iterator `iter`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regSum(iter: RegionIterator[Int32, r]): Int32 \ Read(r) =
        regFoldLeft((acc, x) -> acc + x, 0, iter)

    ///
    /// Returns the sum of all elements in the iterator `iter` according to the function `f`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def sumWith(f: a -> Int32 & ef, iter: Iterator[a]): Int32 & Impure =
        foldLeft((acc, x) -> acc + f(x), 0, iter)

    ///
    /// Returns the sum of all elements in the iterator `iter` according to the function `f`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regSumWith(f: a -> Int32 & ef, iter: RegionIterator[a, r]): Int32 \ { ef, Read(r) } =
        regFoldLeft((acc, x) -> acc + f(x), 0, iter)

    ///
    /// Returns the product of all elements in the iterator `iter`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def product(iter: Iterator[Int32]): Int32 & Impure =
        if (isEmpty(iter))
            0
        else
            foldLeft((acc, x) -> acc * x, 1, iter)

    ///
    /// Returns the product of all elements in the iterator `iter`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regProduct(iter: RegionIterator[Int32, r]): Int32 \ Read(r) =
        if (regIsEmpty(iter))
            0
        else
            regFoldLeft((acc, x) -> acc * x, 1, iter)

    ///
    /// Returns the product of all elements in the iterator `iter` according to the function `f`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def productWith(f: a -> Int32 & ef, iter: Iterator[a]): Int32 & Impure =
        if (isEmpty(iter))
            0
        else
            foldLeft((acc, x) -> acc * f(x), 1, iter)

    ///
    /// Returns the product of all elements in the iterator `iter` according to the function `f`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regProductWith(f: a -> Int32 & ef, iter: RegionIterator[a, r]): Int32 \ { ef, Read(r) } =
        if (regIsEmpty(iter))
            0
        else
            regFoldLeft((acc, x) -> acc * f(x), 1, iter)

    ///
    /// Returns the contents of `iter` as an array.
    ///
    /// Consumes the entire iterator.
    ///
    pub def toArray(iter: Iterator[a], r: Region[r]): Array[a, r] & Impure =
        let m = new MutList(r);
        foreach(a -> MutList.push!(a, m), iter);
        MutList.toArray(m, r)

    ///
    /// Returns the contents of `iter` as an array.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regToArray(r1: Region[r1], iter: RegionIterator[a, r2]): Array[a, r1] \ { Read(r2), Write(r1) } = region r0 {
        let m = new MutList(r0);
        regForeach(a -> MutList.push!(a, m), iter);
        MutList.toArray(m, r1)
    }

    ///
    /// Returns the contents of `iter` as a list.
    ///
    /// Consumes the entire iterator.
    ///
    pub def toList(iter: Iterator[a]): List[a] & Impure =
        foldRight((a, acc) -> a :: acc, Nil, iter)

    ///
    /// Returns the contents of `iter` as a list.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regToList(iter: RegionIterator[a, r]): List[a] \ Read(r) =
        regFoldRight((a, acc) -> a :: acc, Nil, iter)

    ///
    /// Returns the contents of `iter` as a map.
    ///
    /// Consumes the entire iterator.
    ///
    pub def toMap(iter: Iterator[(a, b)]): Map[a, b] & Impure with Order[a] =
        foldLeft((acc, ab) -> Map.insert(fst(ab), snd(ab), acc), Map.empty(), iter)

    ///
    /// Returns the contents of `iter` as a map.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regToMap(iter: RegionIterator[(a, b), r]): Map[a, b] \ Read(r) with Order[a] =
        regFoldLeft((acc, ab) -> Map.insert(fst(ab), snd(ab), acc), Map.empty(), iter)

    ///
    /// Returns the contents of `iter` as a `Some(Nel)` if `iter` is not empty. Returns None otherwise.
    ///
    /// Consumes the entire iterator.
    ///
    pub def toNel(iter: Iterator[a]): Option[Nel[a]] & Impure = match toList(iter) {
        case Nil     => None
        case x :: xs => Some(Nel(x, xs))
    }

    ///
    /// Returns the contents of `iter` as a nec.
    ///
    /// Consumes the entire iterator.
    ///
    pub def toNec(iter: Iterator[a]): Option[Nec[a]] & Impure =
        let f = (x, acc) -> match acc {
            case None      => Some(Nec.singleton(x))
            case Some(nec) => Some(Nec.cons(x, nec))
        };
        foldRight(f, None, iter)

    ///
    /// Returns the contents of `iter` as a MutDeque.
    ///
    /// Consumes the entire iterator.
    ///
    pub def toMutDeque(iter: Iterator[a], r: Region[r]): MutDeque[a, r] & Impure  =
        let d = new MutDeque(r);
        foreach(x -> MutDeque.pushBack(x, d), iter);
        d

    ///
    /// Returns the contents of `iter` as a set. Consumes the entire iterator.
    ///
    pub def toSet(iter: Iterator[a]): Set[a] & Impure with Order[a] =
        foldLeft((acc, a) -> Set.insert(a, acc), Set.empty(), iter)

    ///
    /// Returns a list of every element in `iter` that satisfies the predicate `f`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def filter(f: a -> Bool & ef, iter: Iterator[a]): List[a] & Impure =
        let Iterator(done, next) = iter;
        def loop(k) = {
            if (done())
                k(Nil)
            else
                let x = next();
                if (f(x)) loop(ks -> k(x :: ks)) else loop(k)
        };
        loop(identity)

    ///
    /// Returns a list with `f` applied to every element in `iter`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def map(f: a -> b & ef, iter: Iterator[a]): List[b] & Impure =
        let Iterator(done, next) = iter;
        def loop(k) = {
            if (done())
                k(Nil)
            else
                let x = f(next());
                loop(ks -> k(x :: ks))
        };
        loop(identity)

    ///
    /// Applies `f` to every element of `iter`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def foreach(f: a -> Unit & ef, iter: Iterator[a]): Unit & Impure =
        let Iterator(done, next) = iter;
        def loop() = {
            if (done())
                ()
            else {
                f(next());
                loop()
            }
        };
        loop()

    ///
    /// Applies `f` to every element of `iter`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regForeach(f: a -> Unit & ef, iter: RegionIterator[a, r]): Unit \ { ef, Read(r) } =
        let RegionIterator(done, next) = iter;
        def loop() = {
            if (done())
                ()
            else {
                f(next());
                loop()
            }
        };
        loop()

    ///
    /// Returns an iterator with every element of the iterator `iter` that
    /// satisfies the predicate `f`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// The original iterator `iter` should not be reused.
    ///
    @Lazy
    pub def filterL(f: a -> Bool, iter: Iterator[a]): Iterator[a] & Impure =
        let Iterator(done, next) = iter;

        // Allocate a reference to hold the next element.
        let cursor = ref None;

        let done1 = () -> match deref cursor {
            case None => match forward(f, done, next) {
                case None    => true
                case Some(x) =>
                    // An element was found. Store it in the reference.
                    cursor := Some(x);
                    false
            }
            case Some(_) => false
        };

        let next1 = () -> match deref cursor {
            case None    => bug!("Empty iterator")
            case Some(x) =>
                // An element is in the reference. Use it.
                cursor := None;
                x
        };
        Iterator(done1, next1)

    ///
    /// Fast forwards the given iterator until the next element that satisfies
    /// the given predicate `f`. Returns it (if it exists).
    ///
    def forward(f: a -> Bool & ef, done: Done, next: Next[a]): Option[a] & Impure =
        def loop() = {
            if (done())
                None
            else
                let e = next();
                if (f(e)) Some(e) else loop()
        };
        loop()

    ///
    /// Returns an iterator with every `f` lazily applied to each element in `iter`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    @Lazy
    pub def mapL(f: a -> b, iter: Iterator[a]): Iterator[b] =
        let Iterator(done, next) = iter;
        let next1 = () -> f(next());
        Iterator(done, next1)

    ///
    /// Returns an iterator with every occurrence of `from` replaced by `to` in `iter`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    @Lazy
    pub def replace(from: {from :: a}, to: {to :: a}, iter: Iterator[a]): Iterator[a] with Eq[a] =
        let Iterator(done, next) = iter;
        let next1 = () -> {
            let el = next();
            if (el == from.from) to.to else el
        };
        Iterator(done, next1)

    ///
    /// Returns `iterA` appended to `iterB`.
    ///
    /// Does *not* consume any elements from either iterator.
    ///
    /// The original iterators `iterA` and `iterB` should not be reused.
    ///
    @Lazy
    pub def append(iterA: Iterator[a], iterB: Iterator[a]): Iterator[a] =
        let Iterator(doneA, nextA) = iterA;
        let Iterator(doneB, nextB) = iterB;
        let doneAB = () -> doneA() and doneB();
        let nextAB = () -> if (doneA()) nextB() else nextA();
        Iterator(doneAB, nextAB)

    ///
    /// Returns an iterator where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `iterA` and `b` is the element at index `i` in `iterB`.
    ///
    /// Does *not* consume any elements from either iterator.
    ///
    /// If either `iterA` or `iterB` is depleted, then no further elements are added to the resulting iterator.
    ///
    /// The original iterators `iterA` and `iterB` should not be reused.
    ///
    /// An iterator should never be zipped with itself.
    ///
    @Lazy
    pub def zip(iterA: Iterator[a], iterB: Iterator[b]): Iterator[(a, b)] =
        let Iterator(doneA, nextA) = iterA;
        let Iterator(doneB, nextB) = iterB;
        let doneAB = () -> doneA() or doneB();
        let nextAB = () -> (nextA(), nextB());
        Iterator(doneAB, nextAB)

    ///
    /// Returns an iterator where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `iterA` and `b` is the element at index `i` in `iterB`.
    ///
    /// Does *not* consume any elements from either iterator.
    ///
    /// If either `iterA` or `iterB` becomes depleted, then no further elements are added to the resulting list.
    ///
    /// The original iterators `iterA` and `iterB` should not be reused.
    ///
    /// An iterator should never be zipped with itself.
    ///
    @Lazy
    pub def zipWithL(f: (a, b) -> c, iterA: Iterator[a], iterB: Iterator[b]): Iterator[c] =
        let Iterator(done, next) = zip(iterA, iterB);
        let next1 = () -> {
            let (x, y) = next();
            f(x, y)
        };
        Iterator(done, next1)

    ///
    /// Returns an iterator where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `iterA` and `b` is the element at index `i` in `iterB`.
    ///
    /// Consumes the entire iterator.
    ///
    /// If either `iterA` or `iterB` becomes depleted, then no further elements are added to the resulting list.
    ///
    /// The original iterators `iterA` and `iterB` should not be reused.
    ///
    /// An iterator should never be zipped with itself.
    ///
    pub def zipWith(f: (a, b) -> c & ef, iterA: Iterator[a], iterB: Iterator[b]): List[c] & Impure =
        zip(iterA, iterB) |> map(match (x, y) -> f(x, y))

    ///
    /// Applies `f` to a start value `s` and all elements in `iter` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, s: b, iter: Iterator[a]): b & Impure =
        let Iterator(done, next) = iter;
        def loop(acc) = {
            if (done())
                acc
            else
                loop(f(acc, next()))
        };
        loop(s)

    ///
    /// Applies `f` to a start value `s` and all elements in `iter` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regFoldLeft(f: (b, a) -> b & ef, s: b, iter: RegionIterator[a, r]): b \ { ef, Read(r) } =
        let RegionIterator(done, next) = iter;
        def loop(acc) = {
            if (done())
                acc
            else
                loop(f(acc, next()))
        };
        loop(s)

    ///
    /// Applies `f` to a start value `s` and all elements in `iter` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def foldRight(f: (a, b) -> b & ef, s: b, iter: Iterator[a]): b & Impure =
        let Iterator(done, next) = iter;
        def loop(k) = {
            if (done())
                k(s)
            else {
                let x = next();
                loop(ks -> k(f(x, ks)))
            }
        };
        loop(ks -> ks as & ef) // NB: Cast required because the identity continuation is pure,
                               // but the intermediate continuations are effect polymorphic
    ///
    /// Applies `f` to a start value `s` and all elements in `iter` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regFoldRight(f: (a, b) -> b & ef, s: b, iter: RegionIterator[a, r]): b \ { ef, Read(r) } =
        let RegionIterator(done, next) = iter;
        def loop(k) = {
            if (done())
                k(s)
            else {
                let x = next();
                loop(ks -> k(f(x, ks)))
            }
        };
        loop(ks -> ks as & ef) // NB: Cast required because the identity continuation is pure,
                               // but the intermediate continuations are effect polymorphic
    ///
    /// Applies `f` to a start value `z` and all elements in `iter` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    /// Consumes the entire iterator.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b & Impure) -> b & ef, z: b, iter: Iterator[a]): b & Impure =
        let Iterator(done, next) = iter;
        def loop() = {
            if (done())
                z
            else {
                let x = next();
                f(x, _ -> loop())
            }
        };
        loop()

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b & ef, iter: Iterator[a]): b & Impure with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), iter)

    ///
    /// Applies `f` to all elements in `iter` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    ///
    /// Returns `None` if `iter` is empty.
    ///
    /// Consumes the entire iterator.
    ///
    pub def reduceLeft(f: (a, a) -> a & ef, iter: Iterator[a]): Option[a] & Impure =
        let Iterator(done, next) = iter;
        if (done())
            None
        else
            Some(foldLeft(f, next(), iter))

    ///
    /// Returns an iterator over the results of `f`.
    ///
    /// If `f` returns `Ok(x)`, then the next element is `x`.
    ///
    /// If `f` returns `Err(e)`, then the iterator is depleted.
    ///
    @Lazy
    pub def unfoldWithOk(f: Unit -> Result[a, b] & ef): Iterator[a] & Impure =
        let cursor = ref None;
        let done = () -> match deref cursor {
            case None    => match f() {
                case Err(_) => true
                case Ok(x)  =>
                    cursor := Some(x);
                    false
            }
            case Some(_) => false
        };
        let next = () -> match deref cursor {
            case None    => bug!("Empty Iterator!")
            case Some(x) =>
                cursor := None;
                x
        };
        Iterator(done, next)

    ///
    /// Returns the longest prefix of `iter` that satisfies the predicate `f`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def takeWhile(f: a -> Bool & ef, iter: Iterator[a]): List[a] & Impure =
        let Iterator(done, next) = iter;
        def loop(k) = {
            if (done())
                k(Nil)
            else
                let x = next();
                if (f(x)) loop(ks -> k(x :: ks)) else k(Nil)
        };
        loop(identity)

    ///
    /// Returns `iter` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def dropWhile(f: a -> Bool & ef, iter: Iterator[a]): List[a] & Impure =
        let Iterator(done, next) = iter;
        match forward(x -> not f(x), done, next) {
            case None    => Nil
            case Some(i) => i :: toList(iter)
        }

    ///
    /// Returns the result of applying `f` to every element in `iter` and concatenating the results.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    @Lazy
    pub def flatMap(f: a -> Iterator[b] & ef, iter: Iterator[a]): Iterator[b] & Impure =
        let Iterator(done, next) = iter;
        let cursor = ref None;
        let done1 = () -> match deref cursor {
            case None     => match flatMapForward(f, done, next) {
                case None     => true
                case Some(it) =>
                    cursor := Some(it);
                    false
            }
            case Some(it) =>
                let Iterator(d, _) = it;
                if (d())
                    match flatMapForward(f, done, next) {
                        case None      => true
                        case Some(it1) =>
                            cursor := Some(it1);
                            false
                    }
                else
                    false
        };
        let next1 = () -> match deref cursor {
            case None     => bug!("Empty Iterator!")
            case Some(it) =>
                let Iterator(_, n) = it;
                n()
        };
        Iterator(done1, next1)

    ///
    /// Forwards the given iterator to the first non-empty iterator produced by `f`.
    ///
    def flatMapForward(f: a -> Iterator[b] & ef, done: Done, next: Next[a]): Option[Iterator[b]] & Impure =
        def loop() = {
            if (done())
                None
            else
                let Iterator(d, n) = f(next());
                if (d()) loop() else Some(Iterator(d, n))
        };
        loop()

    ///
    /// Returns an iterator with `a` inserted between every of `iter`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    @Lazy
    pub def intersperse(x: a, iter: Iterator[a]): Iterator[a] & Impure =
        let Iterator(done, next) = iter;
        let returnX = ref false;
        let next1 = () -> {
            if (deref returnX) {
                returnX := false;
                x
            }
            else {
                returnX := true;
                next()
            }
        };
        Iterator(done, next1)

    ///
    /// Returns the concatenation of the elements in `iterB` with the elements of `iterA` inserted between every two adjacent elements.
    ///
    /// That is, returns `b1 :: a1 ... an :: b2 :: ... bn-1 :: a1 :: ... :: an :: bn :: Nil`.
    ///
    /// Does *not* consume any elements from either iterator.
    ///
    @Lazy
    pub def intercalate(iterA: Iterator[a], iterB: Iterator[Iterator[a]]): Iterator[a] & Impure =
        let Iterator(doneA, nextA) = iterA;
        let Iterator(doneB, nextB) = iterB;
        let xs = ref Nil;
        let isReversed = ref false;
        let nextAA = () -> {
            let x = nextA();
            xs := x :: deref xs;
            x
        };
        let doneAA = () -> {
            if (doneA()) {
                if (deref isReversed)
                    ()
                else {
                    xs := List.reverse(deref xs);
                    isReversed := true
                };
                true
            }
            else
                false
        };
        let cursor = ref None;
        let mkCursor = i -> {
            if (deref isReversed)
                // `iterA` has been consumed, so construct a new one from `xs`
                cursor := Some(append(i, List.toIterator(deref xs)))
            else
                cursor := Some(append(i, Iterator(doneAA, nextAA)))
        };
        let done1 = () -> match deref cursor {
            case None    => match intercalateForward(doneB, nextB) {
                case None    => true
                case Some(i) =>
                    if (doneB())
                        cursor := Some(i)
                    else
                        mkCursor(i);
                    false
            }
            case Some(i) =>
                let Iterator(d, _) = i;
                if (d()) match intercalateForward(doneB, nextB) {
                    case None    => true
                    case Some(i1) =>
                        if (doneB())
                            cursor := Some(i1)
                        else
                            mkCursor(i1);
                        false
                }
                else
                    false
        };
        let next1 = () -> match deref cursor {
            case None    => bug!("Empty Iterator!")
            case Some(i) =>
                let Iterator(_, n) = i;
                n()
        };
        Iterator(done1, next1)

    ///
    /// Forwards the iterator the first non-empty iterator produced by `next`.
    ///
    def intercalateForward(done: Done, next: Next[Iterator[a]]): Option[Iterator[a]] & Impure =
        def loop() = {
            if (done())
                None
            else
                let Iterator(d, n) = next();
                if (d()) loop() else Some(Iterator(d, n))
        };
        loop()

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `iter` with `sep` inserted between each element.
    ///
    /// Consumes the entire iterator.
    ///
    pub def join(sep: String, iter: Iterator[a]): String & Impure with ToString[a] =
        joinWith(ToString.toString, sep, iter)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `iter` according to `f` with `sep` inserted between each element.
    ///
    /// Consumes the entire iterator.
    ///
    pub def joinWith(f: a -> String & ef, sep: String, iter: Iterator[a]): String & Impure = region r {
        use StringBuilder.append!;
        let lastSep = String.length(sep);
        let sb = new StringBuilder(r);
        foreach(x -> { append!(f(x), sb); append!(sep, sb) }, iter);
        StringBuilder.toString(sb) |> String.dropRight(lastSep)
    }

    ///
    /// Returns an iterator with every element of the iterator `iter` that
    /// produces `Some(_)` the from the function `f`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// The original iterator `iter` should not be reused.
    ///
    @Lazy
    pub def filterMapL(f: a -> Option[b], iter: Iterator[a]): Iterator[b] & Impure =
        let Iterator(done, next) = iter;

        // Allocate a reference to hold the next element.
        let cursor = ref None;

        let done1 = () -> match deref cursor {
            case None => match filterMapForward(f, done, next) {
                case None    => true
                case Some(x) =>
                    // An element was found. Store it in the reference.
                    cursor := Some(x);
                    false
            }
            case Some(_) => false
        };

        let next1 = () -> match deref cursor {
            case None    => bug!("Empty iterator")
            case Some(x) =>
                // An element is in the reference. Use it.
                cursor := None;
                x
        };
        Iterator(done1, next1)

    ///
    /// Fast forwards the given iterator until the next element that produces
    /// a `Some` given function `f`. Returns it if it exists, or `None`.
    ///
    def filterMapForward(f: a -> Option[b] & ef, done: Done, next: Next[a]): Option[b] & Impure =
        def loop() = {
            if (done())
                None
            else
                let e = next();
                match f(e) {
                    case None => loop()
                    case Some(b) => Some(b)
                }
        };
        loop()

    ///
    /// Returns `iter` without the first `n` elements.
    ///
    /// Returns an empty iterator if `n > length(l)`.
    /// Returns `iter` if `n < 0`.
    ///
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// The original iterator `iter` should not be reused.
    ///
    @Lazy
    pub def dropL(n: Int32, iter: Iterator[a]): Iterator[a] & Impure =
        if (n <= 0 )
            iter
        else {
            let Iterator(done, next) = iter;
            match dropForward(n, done, next) {
                case Some(iter1) => iter1
                case None        => empty()
            }
        }

    ///
    /// Fast forwards the given iterator n times
    ///
    def dropForward(n: Int32, done: Done, next: Next[a]): Option[Iterator[a]] & Impure =
        let skipPos = ref n;
        def loop() = {
            let pos = deref skipPos;
            if (pos <= 0)
                Some(Iterator(done, next))
            else if (done())
                None
            else {
                let _ = next();
                skipPos := pos -1;
                loop()
            }
        };
        loop()


    ///
    /// Cons element `e` at the front of iterator `iter`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// The original iterator `iter` should not be reused.
    ///
    def cons(e: a, iter: Iterator[a]): Iterator[a] & Impure =
        let Iterator(done, next) = iter;
        let front = ref Some(e);
        let done1 = () -> match deref front {
            case None    => done()
            case Some(_) => false
        };
        let next1 = () -> match deref front {
            case None    => next()
            case Some(a) => {
                front := None;
                a
            }
        };
        Iterator(done1, next1)

    ///
    /// Returns `iter` without the prefix of elements that satisfy
    /// the predicate `f` dropped.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// The original iterator `iter` should not be reused.
    ///
    @Lazy
    pub def dropWhileL(f: a -> Bool, iter: Iterator[a]): Iterator[a] & Impure =
        let Iterator(done, next) = iter;
        match dropWhileForward(f, done, next) {
            case None       => empty()
            case Some(iter1) => iter1
        }


    ///
    /// Fast forwards the given iterator while applying f is true
    ///
    def dropWhileForward(f: a -> Bool, done: Done, next: Next[a]): Option[Iterator[a]] & Impure =
        def loop() = {
            if (done())
                None
            else {
                let a = next();
                if (f(a))
                    loop()
                else
                    Some(cons(a, Iterator(done, next)))
            }
        };
        loop()

    ///
    /// Returns the contents of `iter` as a list.
    ///
    /// Consumes the entire iterator.
    ///
    /// If any stage of the iterator is `Err(e)` then `Err(e)` is returned (first fail).
    /// If the iterator has no errors then the result is wrapped with `Ok`.
    ///
    pub def toResultList(iter: Iterator[Result[a, e]]): Result[List[a], e] & Impure =
        let Iterator(done, next) = iter;
        def loop(fk, sk) = {
            if (done())
                sk(Nil)
            else
                match next() {
                    case Ok(a)  => loop(fk, ks -> sk(a :: ks))
                    case Err(e) => fk(e)
                }
        };
        loop(e -> Err(e), ks -> Ok(ks))

}
