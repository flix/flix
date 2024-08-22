/*
 * Copyright 2021 Jakob Schneider Villumsen, Stephen Tetley
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

pub enum Iterator[a: Type, ef: Eff, r: Region] {
    case Iterator(Region[r], Unit -> Iterator.Step[a] \ {r, ef})
}

mod Iterator {
    use Step.{Ans, Skip, Done}


    ///
    /// Represent the "steps" of an iterator.
    ///
    /// This is an expanded version of the `Option` type.
    /// `Ans` is the answer at an intermediate stage of iteration.
    /// `Done` signals that iteration is complete.
    /// `Skip` is essentially an implementation detail. It allows us
    /// to define `filter` and similar functions that "wither" Iterators
    /// without recursively running through the Iterator.
    ///
    pub enum Step[a] {
        case Ans(a)
        case Skip
        case Done
    }

    ///
    /// Returns an iterator built with the stepper function `f`.
    ///
    pub def iterate(rc: Region[r], f: Unit -> Option[a] \ ef): Iterator[a, ef, r] =
        let f1 = () -> match f() {
            case Some(a) => Ans(a)
            case None    => Done
        };
        let iterF = () -> checked_ecast(f1());
        Iterator(rc, iterF)

    ///
    /// Returns an empty iterator.
    ///
    pub def empty(rc: Region[r]): Iterator[a, r, r] =
        let f = constant(Done);
        Iterator(rc, f)

    ///
    /// Returns an iterator containing only a single element, `x`.
    ///
    pub def singleton(rc: Region[r], x: a): Iterator[a, r, r] \ r =
        repeat(rc, 1, x)

    ///
    /// Returns `Some(x)` if `iter` is not empty. Returns `None` otherwise.
    ///
    /// Consumes the head element of `iter`.
    ///
    pub def next(iter: Iterator[a, ef, r]): Option[a] \ { ef, r } =
        let Iterator(_, iterF) = iter;
        def loop() = match iterF() {
            case Ans(a) => Some(a)
            case Skip   => loop()
            case Done   => None
        };
        loop()

    ///
    /// Returns an iterator of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty iterator if `b >= e`.
    ///
    pub def range(rc: Region[r], b: Int32, e: Int32): Iterator[Int32, r, r] \ r =
        let pos = Ref.fresh(rc, b);
        if (e <= b)
            empty(rc)
        else {
            let iterF = () -> match (Ref.get(pos)) {
                case i if i < e => Ref.put(i + 1, pos); Ans(i)
                case _          => Done
            };
            Iterator(rc, iterF)
        }

    ///
    /// Returns an iterator over an iterable with the element `x` repeated `n` times.
    ///
    /// Returns an empty iterator if `n < 0`.
    ///
    pub def repeat(rc: Region[r], n: Int32, x: a): Iterator[a, r, r] \ r =
        let ix = Ref.fresh(rc, n);
        let iterF = () -> {
            let i = Ref.get(ix);
            if (i < 1)
                Done
            else {
                Ref.put(i - 1, ix);
                Ans(x)
            }
        };
        Iterator(rc, iterF)

    ///
    /// Returns the sum of all elements in the iterator `iter`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def sum(iter: Iterator[Int32, ef, r]): Int32 \ { ef, r } =
        foldLeft((acc, x) -> acc + x, 0, iter)

    ///
    /// Returns the sum of all elements in the iterator `iter` according to the function `f`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def sumWith(f: a -> Int32 \ ef2, iter: Iterator[a, ef1, r]): Int32 \ { ef1, ef2, r } =
        foldLeft((acc, x) -> acc + f(x), 0, iter)

    ///
    /// Returns the size of the iterator.
    ///
    /// Consumes the entire iterator.
    ///
    pub def size(iter: Iterator[a, ef1, r]): Int32 \ { ef1, r } =
        foldLeft((acc, _) -> acc + 1, 0, iter)

    ///
    /// Returns the contents of `iter` as an array.
    ///
    /// Consumes the entire iterator.
    ///
    pub def toArray(rc: Region[r1], iter: Iterator[a, ef, r2]): Array[a, r1] \ { ef, r2, r1 } = region rc1 {
        let m = MutList.empty(rc1);
        forEach(a -> MutList.push!(a, m), iter);
        MutList.toArray(rc, m)
    }

    ///
    /// Returns the contents of `iter` as an array.
    ///
    /// Consumes the entire iterator.
    ///
    pub def toVector(iter: Iterator[a, ef, r]): Vector[a] \ { ef, r } = region rc {
        let m = MutList.empty(rc);
        forEach(a -> MutList.push!(a, m), iter);
        MutList.toVector(m)
    }

    ///
    /// Returns the contents of `iter` as a list.
    ///
    /// Consumes the entire iterator.
    ///
    pub def toList(iter: Iterator[a, ef, r]): List[a] \ { ef, r } =
        foldRight((a, acc) -> a :: acc, Nil, iter)


    ///
    /// Returns the contents of `iter` as a chain.
    ///
    /// Consumes the entire iterator.
    ///
    pub def toChain(iter: Iterator[a, ef, r]): Chain[a] \ { ef, r } =
        foldLeft((acc, a) -> Chain.snoc(acc, a), Chain.empty(), iter)

    ///
    /// Returns the contents of `iter` as a map.
    ///
    /// Consumes the entire iterator.
    ///
    pub def toMap(iter: Iterator[(k, v), ef, r]): Map[k, v] \ { ef, r } with Order[k] =
        foldLeft((acc, a) -> (match (k, v) -> Map.insert(k, v, acc))(a), Map.empty(), iter)

    ///
    /// Returns the contents of `iter` as a `Some(Nel)` if `iter` is not empty. Returns None otherwise.
    ///
    /// Consumes the entire iterator.
    ///
    pub def toNel(iter: Iterator[a, ef, r]): Option[Nel[a]] \ { ef, r } =
        toList(iter) |> List.toNel

    ///
    /// Returns the contents of `iter` as a Nec.
    ///
    /// Consumes the entire iterator.
    ///
    pub def toNec(iter: Iterator[a, ef, r]): Option[Nec[a]] \ { ef, r } = match next(iter) {
        case Some(a) => foldLeft((acc, x) -> Nec.snoc(acc, x), Nec.singleton(a), iter) |> Some
        case None    => None
    }

    ///
    /// Returns the contents of `iter` as a MutDeque.
    ///
    /// Consumes the entire iterator.
    ///
    pub def toMutDeque(rc: Region[r1], iter: Iterator[a, ef, r2]): MutDeque[a, r1] \ { ef, r2, r1 } =
        let d = MutDeque.empty(rc);
        forEach(x -> MutDeque.pushBack(x, d), iter);
        d

    ///
    /// Returns the contents of `iter` as a set. Consumes the entire iterator.
    ///
    pub def toSet(iter: Iterator[a, ef, r]): Set[a] \ { ef, r } with Order[a] =
        foldLeft((acc, a) -> Set.insert(a, acc), Set.empty(), iter)

    ///
    /// Applies `f` to every element of `iter`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def forEach(f: a -> Unit \ ef2, iter: Iterator[a, ef1, r]): Unit \ { ef1, ef2, r } =
        let Iterator(_, iterF) = iter;
        def loop() = match iterF() {
            case Skip   => loop()
            case Ans(a) => f(a); loop()
            case Done   => ()
        };
        loop()

    ///
    /// Applies `f` to every element of `iter` along with that element's index.
    ///
    /// Consumes the entire iterator.
    ///
    pub def forEachWithIndex(f: (Int32, a) -> Unit \ ef2, iter: Iterator[a, ef1, r]): Unit \ { ef1, ef2, r } =
        let Iterator(_, iterF) = iter;
        def loop(i) = match iterF() {
            case Skip   => loop(i)
            case Ans(a) => f(i, a); loop(i+1)
            case Done   => ()
        };
        loop(0)

    ///
    /// Returns an iterator with every element of the iterator `iter` that
    /// satisfies the predicate `f`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// The original iterator `iter` should *not* be reused.
    ///
    pub def filter(f: a -> Bool \ ef2, iter: Iterator[a, ef1, r]): Iterator[a, ef1 + ef2, r] =
        let Iterator(rc, iterF) = iter;
        let step = x -> match x {
            case Ans(a) => if (f(a)) Ans(a) else Skip
            case Skip   => Skip
            case Done   => Done
        };
        let iterF1 = () -> step(iterF());
        Iterator(rc, iterF1)

    ///
    /// Returns an iterator with every `f` lazily applied to each element in `iter`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    pub def map(f: a -> b \ ef2, iter: Iterator[a, ef1, r]): Iterator[b, ef1 + ef2, r] =
        let Iterator(rc, iterF) = iter;
        let step = x -> match x {
            case Ans(a) => Ans(f(a))
            case Skip   => Skip
            case Done   => Done
        };
        let iterF1 = () -> {let x = iterF(); step(x)};
        Iterator(rc, iterF1)

    ///
    /// Returns an iterator with every `f` lazily applied to each element in `iter`
    /// together with its index.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    pub def mapWithIndex(f: (Int32, a) -> b \ ef2, iter: Iterator[a, ef1, r]): Iterator[b, ef1 + ef2, r] \ r =
        let Iterator(rc, iterF) = iter;
        let ix = Ref.fresh(rc, 0);
        let step = x -> match x {
            case Ans(a) => { let i = Ref.get(ix); Ref.put(i + 1, ix); Ans(f(i, a)) }
            case Skip   => Skip
            case Done   => Done
        };
        let iterF1 = () -> {let x = iterF(); step(x)};
        Iterator(rc, iterF1)

    ///
    /// Returns an iterator with every occurrence of `src` replaced by `dst` in `iter`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    pub def replace(src: {src = a}, dst: {dst = a}, iter: Iterator[a, ef, r]): Iterator[a, ef, r] with Eq[a] =
        map(a -> if (a == src#src) dst#dst else a, iter)

    ///
    /// Returns `iterB` appended to (the end of) `iterA`.
    ///
    /// Does *not* consume any elements from either iterator.
    ///
    /// The original iterators `iterA` and `iterB` should *not* be reused.
    ///
    pub def append(iter1: Iterator[a, ef1, r], iter2: Iterator[a, ef2, r]): Iterator[a, ef1 + ef2, r] =
        let Iterator(rc, iter1F) = iter1;
        let Iterator(_, iter2F) = iter2;
        def loop2(x) = match x {
            case Ans(a) => Ans(a)
            case Skip   => loop2(iter2F())
            case Done   => Done
        };
        def loop1(x) = match x {
            case Ans(a) => Ans(a)
            case Skip   => loop1(iter1F())
            case Done   => loop2(iter2F())
        };
        let iter3F = () -> loop1(iter1F());
        Iterator(rc, iter3F)

    ///
    /// Returns an iterator where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `iterA` and `b` is the element at index `i` in `iterB`.
    ///
    /// Does *not* consume any elements from either iterator.
    ///
    /// If either `iterA` or `iterB` is depleted, then no further elements are added to the resulting iterator.
    ///
    /// The original iterators `iterA` and `iterB` should *not* be reused.
    ///
    /// An iterator should *never* be zipped with itself.
    ///
    pub def zip(iterA: Iterator[a, ef1, r1], iterB: Iterator[b, ef2, r2]): Iterator[(a, b), r2 + ef1 + ef2, r1] =
        zipWith((a, b) -> (a, b), iterA, iterB)

    ///
    /// Returns an iterator where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `iterA` and `b` is the element at index `i` in `iterB`.
    ///
    /// Does *not* consume any elements from either iterator.
    ///
    /// If either `iterA` or `iterB` becomes depleted, then no further elements are added to the resulting list.
    ///
    /// The original iterators `iterA` and `iterB` should *not* be reused.
    ///
    /// An iterator should *never* be zipped with itself.
    ///
    pub def zipWith(f: (a, b) -> c \ ef3, iterA: Iterator[a, ef1, r1], iterB: Iterator[b, ef2, r2]): Iterator[c, r2 + ef1 + ef2 + ef3, r1] =
        let Iterator(rc, iter1F) = iterA;
        let Iterator(_,  iter2F) = iterB;
        let step = (l, r) -> match (l, r) {
            case (Ans(a), Ans(b)) => Ans(f(a, b))
            case (Ans(a), Skip)   => zipWithGoRight(b -> f(a, b), iter2F)
            case (Skip, Ans(b))   => zipWithGoLeft(iter1F, a -> f(a, b))
            case (Skip, Skip)     => Skip
            case (_, _)           => Done

        };
        let iter3F = () -> step(iter1F(), iter2F());
        Iterator(rc, iter3F)

    ///
    /// Helper function for `zipWith`.
    ///
    def zipWithGoLeft(iter: Unit -> Step[a] \ ef1, f: a -> c \ ef2): Step[c] \ { ef1, ef2 } = match iter() {
        case Ans(a) => Ans(f(a))
        case Skip   => zipWithGoLeft(iter, f)
        case Done   => Done
    }

    ///
    /// Helper function for `zipWith`.
    ///
    def zipWithGoRight(f: b -> c \ ef2, iter: Unit -> Step[b] \ ef1): Step[c] \ { ef1, ef2 } = match iter() {
        case Ans(b) => Ans(f(b))
        case Skip   => zipWithGoRight(f, iter)
        case Done   => Done
    }

    ///
    /// Returns an iterator where the element at index `i` is `(a, i)` where
    /// `a` is the element at index `i` in `iter`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// The original iterator `iter` should *not* be reused.
    ///
    pub def zipWithIndex(iter: Iterator[a, ef, r]): Iterator[(Int32, a), ef + r, r] \ r =
        let Iterator(rc, _) = iter;
        let ix = Ref.fresh(rc, 0);
        map(x -> {let i = Ref.get(ix); Ref.put(i + 1, ix); (i, x)}, iter)

    ///
    /// Alias for `zipWithIndex`.
    ///
    pub def enumerator(iter: Iterator[a, ef, r]): Iterator[(Int32, a), ef + r, r] \ r =
        zipWithIndex(iter)

    ///
    /// Applies `f` to a start value `s` and all elements in `iter` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef2, s: b, iter: Iterator[a, ef1, r]): b \ { ef1, ef2, r } =
        let Iterator(_, iterF) = iter;
        def loop(acc) = match iterF() {
            case Skip   => loop(acc)
            case Ans(a) => loop(f(acc, a))
            case Done   => acc
        };
        loop(s)

    ///
    /// Applies `f` to a start value `s` and all elements in `iter` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def foldRight(f: (a, b) -> b \ ef2, s: b, iter: Iterator[a, ef1, r]): b \ { ef1, ef2, r } =
        let Iterator(_, iterF) = iter;
        def loop(k) = match iterF() {
            case Skip   => loop(k)
            case Ans(a) => loop(ks -> k(f(a, ks)))
            case Done   => k(s)

        };
        loop(eidentity)

    ///
    /// Applies `f` to a start value `z` and all elements in `iter` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    /// Consumes the entire iterator.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ { ef, r }) -> b \ ef, z: b, iter: Iterator[a, ef, r]): b \ { ef, r } =
        let Iterator(_, iterF) = iter;
        def loop() = match iterF() {
            case Skip   => loop()
            case Ans(a) => f(a, _ -> loop())
            case Done   => z
        };
        loop()

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef2, iter: Iterator[a, ef1, r]): b \ { ef1, ef2, r } with Monoid[b] =
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
    pub def reduceLeft(f: (a, a) -> a \ ef2, iter: Iterator[a, ef1, r]): Option[a] \ { ef1, ef2, r } =
        match next(iter) {
            case Some(x) => foldLeft(f, x, iter) |> Some
            case None    => None
        }

    ///
    /// Returns an iterator over the results of `f`.
    ///
    /// If `f` returns `Ok(x)`, then the next element is `x`.
    ///
    /// If `f` returns `Err(e)`, then the iterator is depleted.
    ///
    pub def unfoldWithOk(rc: Region[r], f: Unit -> Result[e, a] \ ef): Iterator[a, ef, r] =
        let iterF = () -> Result.toOption(f());
        Iterator.iterate(rc, iterF)

    ///
    /// Returns `iter` without the first `n` elements.
    ///
    /// Returns an empty iterator if `n` is larger than the number of elements in `iter`.
    /// Returns `iter` if `n < 0`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// The original iterator `iter` should *not* be reused.
    ///
    pub def drop(n: Int32, iter: Iterator[a, ef, r]): Iterator[a, ef + r, r] \ r =
        let Iterator(rc, iterF) = iter;
        let ix = Ref.fresh(rc, n);
        def loop() = {
            let i = Ref.get(ix);
            Ref.put(i - 1, ix);
            match iterF() {
                case Ans(a)  => if (i > 0) loop() else Ans(a)
                case Skip    => loop()
                case Done    => Done
            }
        };
        let iter1F = () -> loop();
        Iterator(rc, iter1F)

    ///
    /// Returns `iter` with the first `n` elements.
    ///
    /// Returns `iter` if `n` is larger than the number of elements in `iter`.
    /// Returns an empty iterator if `n < 0`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// The original iterator `iter` should *not* be reused.
    ///
    pub def take(n: Int32, iter: Iterator[a, ef, r]): Iterator[a, ef + r, r] \ r =
        let Iterator(rc, iterF) = iter;
        let ix = Ref.fresh(rc, n);
        def loop() = {
            let i = Ref.get(ix);
            if (i < 1)
                Done
            else
                match iterF() {
                    case Ans(a) => Ref.put(i - 1, ix); Ans(a)
                    case Skip   => Skip
                    case Done   => Done
                }
        };
        let iter1F = () -> loop();
        Iterator(rc, iter1F)

    ///
    /// Returns the longest prefix of `iter` that satisfies the predicate `f`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// The original iterator `iter` should *not* be reused.
    ///
    pub def takeWhile(f: a -> Bool \ ef2, iter: Iterator[a, ef1, r]): Iterator[a, ef1 + ef2, r]=
        let Iterator(rc, iterF) = iter;
        def loop(x) = match x {
            case Ans(a) => if (f(a)) Ans(a) else Done
            case Skip   => loop(iterF())
            case Done   => Done
        };
        let iter1F = () -> loop(iterF());
        Iterator(rc, iter1F)

    ///
    /// Returns `iter` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// The original iterator `iter` should *not* be reused.
    ///
    pub def dropWhile(f: a -> Bool \ ef2, iter: Iterator[a, ef1, r]): Iterator[a, r + ef1 + ef2, r] \ r =
        let Iterator(rc, iterF) = iter;
        let start = Ref.fresh(rc, true);
        def loop() = match iterF() {
            case Ans(a) =>
                if (Ref.get(start)) {
                    if (f(a)) Skip else { Ref.put(false, start); Ans(a) }
                } else {
                    Ans(a)
                }
            case Skip => Skip
            case Done => Done
        };
        let iter1F = () -> loop();
        Iterator(rc, iter1F)

    ///
    /// Returns the result of applying `f` to every element in `iter` and concatenating the results.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// Currently `f` has to generate an iterator with region `r`.
    ///
    pub def flatMap(f: a -> Iterator[b, ef2, r] \ ef3, ma: Iterator[a, ef1, r]): Iterator[b, r + ef1 + ef2 + ef3, r] \ r =
        let Iterator(rc, iterF) = ma;
        let innerIter = Ref.fresh(rc, polymorphicEmpty(rc));
        let inside = Ref.fresh(rc, false);
        def innerLoop() = {
            let Iterator(_, innerF) = Ref.get(innerIter);
            match innerF() {
                case Ans(b) => Ans(b)
                case Skip   => innerLoop()
                case Done   => Done
            }
        };
        def outerLoop() = {
            if (Ref.get(inside)) {
                match innerLoop() {
                    case Done => Ref.put(false, inside); outerLoop()
                    case ans  => ans
                }
            } else {
                match iterF() {
                    case Ans(a) =>
                        Ref.put(true, inside);
                        Ref.put(f(a), innerIter);
                        outerLoop()
                    case Skip   => outerLoop()
                    case Done   => Done
                }
            }
        };
        let iter1F = () -> outerLoop();
        Iterator(rc, iter1F)

    ///
    /// Helper for `flatMap`.
    ///
    /// This is a cheat to get the initial inner iterator (inside the `innerIter` reference) to
    /// typecheck and compile. Ideally we would use the public API function `empty` but its type
    /// is not general enough at the use-site when `innerIter` is re-assigned within `outerLoop`.
    ///
    def polymorphicEmpty(rc: Region[r]): Iterator[a, ef, r] =
        let f = constant(Done);
        Iterator(rc, f)


    ///
    /// Returns an iterator with `a` inserted between every of `iter`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    pub def intersperse(sep: a, iter: Iterator[a, ef, r]): Iterator[a, ef + r, r] \ r =
        let Iterator(rc, _) = iter;
        let start = Ref.fresh(rc, true);
        let step = x ->
            if (Ref.get(start)) {
                Ref.put(false, start);
                singleton(rc, x)
            } else {
                cons(sep, singleton(rc, x))
            };
        flatMap(step, iter)

    ///
    /// Returns the concatenation of the elements in `iter` with the elements of `sep` inserted between every two adjacent elements.
    ///
    /// That is, returns `b1 :: a1 ... an :: b2 :: ... bn-1 :: a1 :: ... :: an :: bn :: Nil`.
    ///
    /// Does *not* consume any elements from either iterator.
    ///
    /// The original iterators `sep` and `iter` should not be reused.
    ///
    pub def intercalate(sep: t[a], iter: Iterator[Iterator[a, ef, r], ef, r]): Iterator[a, ef + r, r] \ (r + Foldable.Aef[t]) with Foldable[t] =
        let Iterator(rc, _) = iter;
        let start = Ref.fresh(rc, true);
        let sepL = Foldable.toList(sep);
        let step = innerIter -> {
            if (Ref.get(start)) {
                Ref.put(false, start);
                innerIter
            } else {
                append(ofList(rc, sepL), innerIter)
            }
        };
        flatMap(step, iter)

    ///
    /// Helper for `intercalate`.
    ///
    /// Note - this is not the "best" return type. Ideally it should be `Iterator[a, r, r]` but
    /// then `intercalate` fails.
    ///
    def ofList(rc: Region[r], xs: List[a]): Iterator[a, {}, r] \ r =
        let ls = Ref.fresh(rc, xs);
        let next = () -> {
            match (Ref.get(ls)) {
                case Nil     => Done
                case x :: rs => Ref.put(rs, ls); Ans(x)
            }
        };
        Iterator(rc, next)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `iter` with `sep` inserted between each element.
    ///
    /// Consumes the entire iterator.
    ///
    pub def join(sep: String, iter: Iterator[a, ef, r]): String \ { ef, r } with ToString[a] =
        joinWith(ToString.toString, sep, iter)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `iter` according to `f` with `sep` inserted between each element.
    ///
    /// Consumes the entire iterator.
    ///
    pub def joinWith(f: a -> String \ ef2, sep: String, iter: Iterator[a, ef1, r]): String \ { ef1, ef2, r } = region rc {
        let sb = StringBuilder.empty(rc);
        match next(iter) {
            case Some(a) => {
                StringBuilder.append!(f(a), sb);
                forEach(x -> {StringBuilder.append!(sep, sb); StringBuilder.append!(f(x), sb)}, iter)
            }
            case None    => ()
        };
        StringBuilder.toString(sb)
    }

    ///
    /// Returns an iterator with every element of the iterator `iter` that
    /// produces `Some(_)` the from the function `f`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// The original iterator `iter` should *not* be reused.
    ///
    pub def filterMap(f: a -> Option[b] \ ef2, iter: Iterator[a, ef1, r]): Iterator[b, ef1 + ef2, r] =
        let Iterator(rc, iterF) = iter;
        let step = x -> match x {
            case Ans(a) => match (f(a)) {
                case Some(a1) => Ans(a1)
                case None     => Skip
            }
            case Skip   => Skip
            case Done   => Done
        };
        let iterF1 = () -> step(iterF());
        Iterator(rc, iterF1)

    ///
    /// Returns an iterator with the element `x` appended to the
    /// front of iterator `iter`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// The original iterator `iter` should *not* be reused.
    ///
    pub def cons(x: a, iter: Iterator[a, ef, r]): Iterator[a, ef, r] \ r =
        let Iterator(rc, iterF) = iter;
        let first = Ref.fresh(rc, true);
        let f = () ->  {
            if (Ref.get(first)) {
                Ref.put(false, first);
                Ans(x)
            }
            else
                iterF()
        };
        let iter1F = () -> f();
        Iterator(rc, iter1F)

    ///
    /// Returns the concatenation of the nested iterators in `iter`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// The original iterator `iter` should *not* be reused.
    ///
    pub def flatten(iter: Iterator[Iterator[a, ef1, r], ef2, r]): Iterator[a, r + ef1 + ef2, r] \ r =
        flatMap(identity, iter)

}
