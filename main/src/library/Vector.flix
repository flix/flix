/*
 *  Copyright 2023 Magnus Madsen, Stephen Tetley
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import java.util.{List => JList}
import java.util.{Iterator => JIterator}

///
/// A Vector data type which is fundamentally like Array but is immutable.
///

instance Indexable[Vector[a]] {
    type Idx = Int32
    type Elm = a
    type Aef = OutOfBounds

    pub def get(t: Vector[a], i: Int32): a \ OutOfBounds = {
        if (0 <= i and i < Vector.length(t)) Vector.get(i, t)
        else do OutOfBounds.outOfBounds(
            "index ${i} is out of bounds for Vector of length ${Vector.length(t)}"
        )
    }
}

instance ToString[Vector[a]] with ToString[a] {
    pub def toString(v: Vector[a]): String = Vector.toString(v)
}

instance Hash[Vector[a]] with Hash[a] {
    pub def hash(v: Vector[a]): Int32 =
        Vector.foldLeft((acc, x) -> acc `Hash.combine` Hash.hash(x), Hash.magic(), v)
}

instance Eq[Vector[a]] with Eq[a] {
    pub def eq(v1: Vector[a], v2: Vector[a]): Bool = Vector.equals(v1, v2)
}

instance Order[Vector[a]] with Order[a] {
    pub def compare(v1: Vector[a], v2: Vector[a]): Comparison = Vector.compare(v1, v2)
}

instance Functor[Vector] {
    pub def map(f: a -> b \ ef, v: Vector[a]): Vector[b] \ ef = Vector.map(f, v)
}

instance Applicative[Vector] {
    pub def point(a: a) : Vector[a] = Vector.singleton(a)
    pub def ap(f: Vector[a -> b \ ef], v: Vector[a]) : Vector[b] \ ef = Vector.ap(f, v)
}

instance Monad[Vector] {
    pub def flatMap(f: a -> Vector[b] \ ef, v : Vector[a]) : Vector[b] \ ef = Vector.flatMap(f, v)
}

instance MonadZero[Vector] {
    pub def empty(): Vector[a] = Vector.empty()
}

instance MonadZip[Vector] {
    pub def zipWith(f: (a, b) -> c \ ef, xs: Vector[a], ys: Vector[b]): Vector[c] \ ef = Vector.zipWith(f, xs, ys)
    pub def zipWithA(f: (a, b) -> f[c] \ ef, xs: Vector[a], ys: Vector[b]): f[Vector[c]] \ ef with Applicative[f] = Vector.zipWithA(f, xs, ys)
    redef zip(v1: Vector[a], v2: Vector[b]): Vector[(a, b)] = Vector.zip(v1, v2)
    redef unzip(v: Vector[(a, b)]): (Vector[a], Vector[b]) = Vector.unzip(v)
}

instance Foldable[Vector] {
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, v: Vector[a]): b \ ef = Vector.foldLeft(f, s, v)
    pub def foldRight(f: (a, b) -> b \ ef, s: b, v: Vector[a]): b \ ef = Vector.foldRight(f, s, v)
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, s: b, v: Vector[a]): b \ ef = Vector.foldRightWithCont(f, s, v)
}

instance UnorderedFoldable[Vector] {
    pub def foldMap(f: a -> b \ ef, v: Vector[a]): b \ ef with CommutativeMonoid[b] = Vector.foldMap(f, v)
    redef isEmpty(v: Vector[a]): Bool = Vector.isEmpty(v)
    redef exists(f: a -> Bool \ ef, v: Vector[a]): Bool \ ef = Vector.exists(f, v)
    redef forAll(f: a -> Bool \ ef, v: Vector[a]): Bool \ ef = Vector.forAll(f, v)
    redef memberOf(x: a, v: Vector[a]): Bool with Eq[a] = Vector.memberOf(x, v)
}

instance Traversable[Vector] {
    pub def traverse(f: a -> m[b] \ ef, t: Vector[a]): m[Vector[b]] \ ef with Applicative[m] = Vector.traverse(f, t)
    redef sequence(t: Vector[m[a]]): m[Vector[a]] with Applicative[m] = Vector.sequence(t)
}

instance Filterable[Vector] {
    pub def filterMap(f: a -> Option[b] \ ef, x: Vector[a]): Vector[b] \ ef = Vector.filterMap(f, x)
    redef filter(f: a -> Bool \ ef, x: Vector[a]): Vector[a] \ ef = Vector.filter(f, x)
}

instance Witherable[Vector]

instance SemiGroup[Vector[a]] {
    pub def combine(x: Vector[a], y: Vector[a]): Vector[a] = Vector.append(x, y)
}

instance Monoid[Vector[a]] {
    pub def empty(): Vector[a] = Vector.empty()
}

instance Collectable[Vector[a]] {
    type Elm = a
    pub def collect(iter: Iterator[a, ef, r]): Vector[a] \ (ef + r) = Iterator.toVector(iter)
}

instance Iterable[Vector[a]] {
    type Elm = a
    pub def iterator(rc: Region[r], v: Vector[a]): Iterator[a, r, r] \ r = Vector.iterator(rc, v)
}

instance ToJava[Vector[a]] {
    type Out = JList
    type Aef = IO
    pub def toJava(v: Vector[a]): JList \ IO = Adaptor.toList(v)
}


instance FromJava[Vector[a]] {
    type In = JList
    pub def fromJava(l: JList): Vector[a] = region rc {
        (unsafe l.iterator()) |> Adaptor.fromIterator(rc, (Proxy.Proxy: Proxy[a])) |> Iterator.toVector
    }
}

mod Vector {

    ///
    /// Compares `a` and `b` lexicographically.
    ///
    pub def compare(a: Vector[a], b: Vector[a]): Comparison with Order[a] =
        let len = Int32.min(length(a), length(b));
        def loop(i) = {
            if (i < len) {
                let cmp = get(i, a) <=> get(i, b);
                if (cmp == Comparison.EqualTo)
                    loop(i + 1)
                else
                    cmp
            } else if (i < length(a)) {
                Comparison.GreaterThan
            } else if (i < length(b)) {
                Comparison.LessThan
            } else {
                Comparison.EqualTo
            }
        };
        loop(0)

    ///
    /// Returns a string representation of the given vector `v`.
    ///
    pub def toString(v: Vector[a]): String with ToString[a] = region rc {
        "Vector#{" + (Vector.iterator(rc, v) |> Iterator.join(", ")) + "}"
    }

    ///
    /// Version of `Array.updateSequence!` where sub is a vector rather than an array,
    /// so a extra copy of `sub` is avoided.
    ///
    def arrayUpdateSeqV!(i: Int32, sub: Vector[a], arr: Array[a, r]): Unit \ r  =
        let end = Array.length(arr);
        let subLen = length(sub);
        def loop(ri, wi) = {
            if (wi >= end or ri >= subLen)
                ()
            else if (wi < 0)
                loop(ri+1, wi+1)
            else {
                let x = get(ri, sub);
                Array.put(x, wi, arr);
                loop(ri+1, wi+1)
            }
        };
        loop(0, i)

    ///
    /// Returns an empty (length zero) vector.
    ///
    pub def empty(): Vector[a] = region rc {
        let arr = Array#{} @ rc;
        Array.toVector(arr)
    }

    ///
    /// Returns a singleton vector containing `x``.
    ///
    pub def singleton(x: a): Vector[a] =
        init(_ -> x, 1)

    ///
    /// Retrieves the value at position `i` in the vector `v`.
    ///
    pub def get(i: Int32, v: Vector[a]): a = $VECTOR_GET$(v, i)

    ///
    /// Optionally returns the element at position `i` in the vector `v`.
    ///
    pub def nth(i: Int32, v: Vector[a]): Option[a] =
        if (0 <= i and i < length(v))
            Some(get(i, v))
        else
            None

    ///
    /// Returns `true` if the given vector `v` is empty.
    ///
    pub def isEmpty(v: Vector[a]): Bool = length(v) == 0

    ///
    /// Returns `true` if the given vector `v` is non-empty.
    ///
    pub def nonEmpty(v: Vector[a]): Bool = not isEmpty(v)

    ///
    /// Returns the number of elements in the vector `v`.
    ///
    pub def length(v: Vector[a]): Int32 = $VECTOR_LENGTH$(v)

    ///
    /// Returns the number of elements in the vector `v`.
    ///
    pub def size(v: Vector[a]): Int32 = length(v)

    ///
    /// Returns a fresh array with the elements from the vector `v` from index `b` (inclusive) until index `e` (exclusive).
    ///
    pub def slice(start: {start = Int32}, end: {end = Int32}, v: Vector[a]): Vector[a] = region rc {
        let arr = toArray(rc, v);
        Array.slice(rc, start, end, arr) |> Array.toVector
    }

    ///
    /// Returns the vector `v` as an array.
    ///
    pub def toArray(rc: Region[r], v: Vector[a]): Array[a, r] \ r =
        let arr = Array.empty(rc, length(v));
        forEachWithIndex((i, x) -> Array.put(x, i, arr), v);
        arr


    ///
    /// Returns the vector `v` as a list.
    ///
    pub def toList(v: Vector[a]): List[a] =
        foldRight((x, acc) -> x :: acc, Nil, v)

    ///
    /// Optionally returns the vector `v` as a non-empty list.
    ///
    /// If `v` is empty return `None`, otherwise return the Nel wrapped in `Some`.
    ///
    pub def toNel(v: Vector[a]): Option[Nel[a]] =
        let step = (x, acc) -> match acc {
            case None    => Some(Nel.singleton(x))
            case Some(c) => Some(Nel.cons(x, c))
        };
        foldRight(step, None, v)

    ///
    /// Returns the vector `v` as a `DelayList`.
    ///
    pub def toDelayList(v: Vector[a]): DelayList[a] =
        foldRight((x, acc) -> DelayList.ECons(x, acc), DelayList.ENil, v)

    ///
    /// Returns the vector `v` as a chain.
    ///
    pub def toChain(v: Vector[a]): Chain[a] =
        foldRight(Chain.cons, Chain.empty(), v)

    ///
    /// Optionally returns the vector `v` as a non-empty chain.
    ///
    /// If `v` is empty return `None`, otherwise return the Nec wrapped in `Some`.
    ///
    pub def toNec(v: Vector[a]): Option[Nec[a]] =
        let step = (x, acc) -> match acc {
            case None    => Some(Nec.singleton(x))
            case Some(c) => Some(Nec.cons(x, c))
        };
        foldRight(step, None, v)

    ///
    /// Returns `v` as a MutDeque.
    ///
    pub def toMutDeque(rc: Region[r], v: Vector[a]): MutDeque[a, r] \ r   =
        let d = MutDeque.empty(rc);
        forEach(x -> MutDeque.pushBack(x, d), v);
        d

    ///
    /// Returns `Some(x)` if `x` is the first element of `v`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def head(v: Vector[a]): Option[a] =
        nth(0, v)

    ///
    /// Returns `Some(x)` if `x` is the last element of `v`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def last(v: Vector[a]): Option[a] =
        nth(length(v) - 1, v)

    ///
    /// Return a new vector, appending the elements `v2` after elements of `v1`.
    ///
    pub def append(v1: Vector[a], v2: Vector[a]): Vector[a] = region rc {
        let arr = Array.empty(rc, length(v1) + length(v2));
        arrayUpdateSeqV!(0, v1, arr);
        arrayUpdateSeqV!(length(v1), v2, arr);
        Array.toVector(arr)
    }


    ///
    /// Returns `true` if and only if `v` contains the element `x`.
    ///
    pub def memberOf(x: a, v: Vector[a]): Bool with Eq[a] =
        exists(y -> y == x, v)

    ///
    /// Optionally finds the smallest element of `v` according to the `Order` on `v`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def minimum(v: Vector[a]): Option[a] with Order[a] =
        reduceLeft(Order.min, v)

    ///
    /// Optionally finds the smallest element of `v` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, v: Vector[a]): Option[a] =
        reduceLeft(Order.minBy(cmp), v)

    ///
    /// Optionally finds the largest element of `v` according to the `Order` on `v`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def maximum(v: Vector[a]): Option[a] with Order[a] =
        reduceLeft(Order.max, v)

    ///
    /// Optionally finds the largest element of `v` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, v: Vector[a]): Option[a] =
        reduceLeft(Order.maxBy(cmp), v)

    ///
    /// Alias for `IndexOfLeft`
    ///
    pub def indexOf(x: a, v: Vector[a]): Option[Int32] with Eq[a] =
        indexOfLeft(x, v)

    ///
    /// Optionally returns the position of the first occurrence of `a` in `v`
    /// searching from left to right.
    ///
    pub def indexOfLeft(a: a, v: Vector[a]): Option[Int32] with Eq[a] =
        def loop(i) = {
            if (i >= length(v))
                -1
            else if (get(i, v) == a)
                i
            else
                loop(i + 1)
        };
        let i = loop(0);
        if (i < 0) None else Some(i)

    ///
    /// Optionally returns the position of the first occurrence of `a` in `v`
    /// searching from right to left.
    ///
    pub def indexOfRight(a: a, v: Vector[a]): Option[Int32] with Eq[a] =
        def loop(i) = {
            if (i < 0)
                -1
            else if (get(i, v) == a)
                i
            else
                loop(i - 1)
        };
        let i = loop(length(v) - 1);
        if (i < 0) None else Some(i)

    ///
    /// Return the positions of the all the occurrences of `a` in `v`.
    ///
    pub def indices(a: a, v: Vector[a]): Vector[Int32] with Eq[a] =
        findIndices(b -> a == b, v)

    ///
    /// Alias for `findLeft`.
    ///
    pub def find(f: a -> Bool \ ef, v: Vector[a]): Option[a] \ ef =
        findLeft(f, v)

    ///
    /// Optionally returns the first element of `v` that satisfies the predicate `f` when searching from left to right.
    ///
    pub def findLeft(f: a -> Bool \ ef, v: Vector[a]): Option[a] \ ef =
        match findIndexOfLeft(f, v) {
            case None    => None
            case Some(i) => Some(get(i, v))
        }

    ///
    /// Optionally returns the first element of `v` that satisfies the predicate `f` when searching from right to left.
    ///
    pub def findRight(f: a -> Bool \ ef, v: Vector[a]): Option[a] \ ef =
        match findIndexOfRight(x -> f(x), v) {
            case None    => None
            case Some(i) => Some(get(i, v))
        }

    ///
    /// Returns a vector of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty vector if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): Vector[Int32] =
        let len = e - b;
        init(ix -> b + ix, len)

    ///
    /// Returns a vector with the element `x` repeated `n` times.
    ///
    /// Returns an empty vector if `n <= 0`.
    ///
    pub def repeat(n: Int32, x: a): Vector[a] =
        init(_ -> x, n)

    ///
    /// Alias for `scanLeft`.
    ///
    pub def scan(f: (b, a) -> b \ ef, s: b, v: Vector[a]): Vector[b] \ ef =
        scanLeft(f, s, v)

    ///
    /// Accumulates the result of applying `f` to `v` going left to right.
    ///
    /// That is, the result is of the form: `[s , f(s, x1), f(f(s, x1), x2),  ...]`.
    ///
    pub def scanLeft(f: (b, a) -> b \ ef, s: b, v: Vector[a]): Vector[b] \ ef = region rc {
        let arr = Array.empty(rc, length(v) + 1);
        let acc = Ref.fresh(rc, s);
        Array.put(s, 0, arr);
        let step = (i, x) -> {
            let s1 = f(Ref.get(acc), x);
            Ref.put(s1, acc);
            Array.put(s1, i+1, arr)
        };
        forEachWithIndex(step, v);
        Array.toVector(arr)
    }

    ///
    /// Accumulates the result of applying `f` to `xs` going right to left.
    ///
    /// That is, the result is of the form: `[..., f(xn-1, f(xn, s)), f(xn, s), s]`.
    ///
    pub def scanRight(f: (a, b) -> b \ ef, s: b, v: Vector[a]): Vector[b] \ ef = region rc {
        let len = length(v);
        let arr = Array.empty(rc, len + 1);
        let acc = Ref.fresh(rc, s);
        Array.put(s, len, arr);
        def loop(i) = {
            if (i < 0)
                ()
            else {
                let s1 = f(get(i, v), Ref.get(acc));
                Ref.put(s1, acc);
                Array.put(s1, i, arr);
                loop(i - 1)
            }
        };
        loop(len - 1);
        Array.toVector(arr)
    }

    ///
    /// Returns the result of applying `f` to every element in `v`.
    ///
    /// The result is a new vector.
    ///
    pub def map(f: a -> b \ ef, v: Vector[a]): Vector[b] \ ef =
        init(i -> f(get(i, v)), length(v))

    ///
    /// Returns the result of applying `f` to every element in `v` along with that element's index.
    ///
    /// That is, the result is of the form: `[ f(a[0], 0), f(a[1], 1), ... ]`.
    ///
    pub def mapWithIndex(f: (Int32, a) -> b \ ef, v: Vector[a]): Vector[b] \ ef =
        init(i -> f(i, get(i, v)), length(v))

    ///
    /// Returns the result of running all the actions in the list `v` going from left
    /// to right.
    ///
    pub def sequence(v: Vector[m[a]]): m[Vector[a]] with Applicative[m] = region rc {
        let len = length(v);
        let arr = Array.empty(rc, len);
        def loop(i, k) = {
            if (i == len)
                k(Applicative.point(arr))
            else {
                let mx = get(i, v);
                loop(i+1, karr -> k(putA!(mx, i, karr)))
            }
        };
        Functor.map(Array.toVector, loop(0, x -> checked_ecast(x)))
    }

    ///
    /// Returns the result of applying the applicative mapping function `f` to all the elements of the
    /// vector `v` going from left to right.
    ///
    pub def traverse(f: a -> m[b] \ ef, v: Vector[a]): m[Vector[b]] \ ef with Applicative[m] = region rc {
        let len = length(v);
        let arr = Array.empty(rc, len);
        def loop(i, k) = {
            if (i == len)
                k(Applicative.point(arr))
            else {
                let x = get(i, v);
                loop(i+1, karr -> k(putA!(f(x), i, karr)))
            }
        };
        Functor.map(Array.toVector, loop(0, x -> checked_ecast(x)))
    }

    ///
    /// Helper for `sequence`, `traverse` and `zipWithA`.
    ///
    def putA!(mx: m[a], i: Int32, marr: m[Array[a, r]]): m[Array[a, r]] \ r with Applicative[m] =
        use Functor.{<$>};
        use Applicative.{<*>};
        (((x, arr) -> {Array.put(x, i, arr); arr}) <$> mx) <*> marr

    ///
    /// Generalize `zipWith` to an applicative functor `f`.
    ///
    pub def zipWithA(f: (a, b) -> m[c] \ ef, v1: Vector[a], v2: Vector[b]): m[Vector[c]] \ ef with Applicative[m] = region rc {
        let len = Int32.min(length(v1), length(v2));
        let arr = Array.empty(rc, len);
        def loop(i, k) = {
            if (i == len)
                k(Applicative.point(arr))
            else {
                let x = get(i, v1);
                let y = get(i, v2);
                loop(i+1, karr -> k(putA!(f(x, y), i, karr)))
            }
        };
        Functor.map(Array.toVector, loop(0, x -> checked_ecast(x)))
    }

    ///
    /// Apply every function from `f` to every argument from `v` and return a list with all results.
    /// For `f = f1, f2, ...` and `x = x1, x2, ...` the results appear in the order
    /// `f1(x1), f1(x2), ..., f2(x1), f2(x2), ...`.
    ///
    pub def ap(f: Vector[a -> b \ ef], v: Vector[a]) : Vector[b] \ ef =
        map(g -> map(g, v), f) |> flatten

    ///
    /// Returns the result of applying `f` to every element in `v` and concatenating the results.
    ///
    pub def flatMap(f: a -> Vector[b] \ ef, v: Vector[a]): Vector[b] \ ef =
        init(i -> f(get(i, v)), length(v)) |> flatten

    ///
    /// Returns the reverse of `v`.
    ///
    pub def reverse(v: Vector[a]): Vector[a] =
        let len = length(v);
        init(ix -> get(len - (ix+1), v), len)

    ///
    /// Rotate the contents of vector `v` by `n` steps to the left.
    ///
    pub def rotateLeft(n: Int32, v: Vector[a]): Vector[a] =
        let len = length(v);
        if (len < 1)
            empty()
        else
            if (n < 0)
                rotateRightHelper(Int32.abs(n), v)
            else
                rotateLeftHelper(n, v)

    ///
    /// Helper function for `rotateLeft` and `rotateRight`.
    ///
    /// Precondition: `n` must be positive.
    ///
    /// This is an explicit helper to avoid code duplication.
    ///
    def rotateLeftHelper(n: Int32, v: Vector[a]): Vector[a] =
        let len = length(v);
        let f = ix -> {let readIx = Int32.modulo(ix + n, len); get(readIx, v)};
        init(f, len)


    ///
    /// Rotate the contents of vector `v` by `n` steps to the right.
    ///
    pub def rotateRight(n: Int32, v: Vector[a]): Vector[a] =
        if (length(v) < 1)
            empty()
        else
            if (n < 0)
                rotateLeftHelper(Int32.abs(n), v)
            else
                rotateRightHelper(n, v)

    ///
    /// Helper function for `rotateRight` and `rotateLeft`.
    ///
    /// Precondition: `n` must be positive.
    ///
    /// This is an explicit helper to avoid code duplication.
    ///
    def rotateRightHelper(n: Int32, v: Vector[a]): Vector[a] =
        let len = length(v);
        let f = ix -> {let readIx = Int32.modulo(ix - n, len); get(readIx, v)};
        init(f, len)

    ///
    /// Returns a copy of `v` with the element at index `i` replaced by `x`.
    ///
    /// Returns a copy of `v` if `i < 0` or `i > length(xs)-1`.
    ///
    pub def update(i: Int32, x: a, v: Vector[a]): Vector[a] =
        let f = ix -> if (ix == i) x else get(ix, v);
        init(f, length(v))

    ///
    /// Returns `b` with the `n` elements starting at index `i` replaced with the elements of `a`.
    ///
    /// If any of the indices `i, i+1, i+2, ... , i+n-1` are out of range in `b` then no patching is done at these indices.
    /// If `a` becomes depleted then no further patching is done.
    /// If patching occurs at index `i+j` in `b`, then the element at index `j` in `a` is used.
    ///
    pub def patch(i: Int32, n: Int32, a: Vector[a], b: Vector[a]): Vector[a] =
        let len1 = length(a);
        let size = if (n > len1) len1 else n;
        let sub = slice(start = 0, end = size, a);
        updateSequence(i, sub, b)

    ///
    /// Returns a copy of `v` with `sep` inserted between every two adjacent elements.
    ///
    pub def intersperse(sep: a, v: Vector[a]): Vector[a] =
        let len1 = length(v);
        let len2 = len1 + len1 - 1;
        let f = ix -> match ix {
            case 0                         => get(0, v)
            case n if Int32.remainder(n, 2) != 0 => sep
            case n                         => {let i = n / 2; get(i, v)}
        };
        init(f, len2)

    ///
    /// Returns the concatenation of the elements in `vs` with the elements
    /// of `sep` inserted between every two adjacent elements.
    ///
    pub def intercalate(sep: Vector[a], vs: Vector[Vector[a]]): Vector[a] = region rc {
        let count = length(vs);
        let sepLength = length(sep);
        let sepCount = if (count < 2) 0 else count - 1;
        let len = sumLengths(vs) + (sepCount * sepLength);
        let pos = Ref.fresh(rc, 0);
        let arr = Array.empty(rc, len);
        let f = (i, v) -> {
            if (i == 0) {
                arrayUpdateSeqV!(0, v, arr);
                Ref.put(length(v), pos)
            } else {
                let ix = Ref.get(pos);
                arrayUpdateSeqV!(ix, sep, arr);
                let ix1 = ix + sepLength;
                arrayUpdateSeqV!(ix1, v, arr);
                Ref.put(ix1 + length(v), pos)
            }
        };
        forEachWithIndex(f, vs);
        Array.toVector(arr)
    }

    ///
    /// Sum the lengths of a vector of vectors.
    ///
    /// Helper function for `intercalate` and `flatten`.
    ///
    def sumLengths(vs: Vector[Vector[a]]): Int32 =
        foldLeft((acc, a) -> acc + length(a), 0, vs)

    ///
    /// Returns the transpose of `vs`.
    ///
    /// Returns a non-transposed copy of `vs` if the dimensions of the elements of `vs` are mismatched.
    ///
    pub def transpose(vs: Vector[Vector[a]]): Vector[Vector[a]] =
        let ilen = length(vs);
        if (ilen == 0)
            empty()
        else {
            let jlen = length(get(0, vs));
            if (jlen == 0 or nonUniform(jlen, vs))
                // Non-transposing nested copy
                init(i -> slice(start = 0, end = length(get(i, vs)), get(i, vs)), ilen)
            else
                init(i -> init(j -> get(i, get(j, vs)), ilen), jlen)
        }

    ///
    /// Helper function for `transpose`.
    ///
    def nonUniform(l: Int32, vs: Vector[Vector[a]]): Bool =
        exists(x -> length(x) != l, vs)

    ///
    /// Returns a copy of `v` with every occurrence of `src` replaced by `dst`.
    ///
    pub def replace(src: {src = a}, dst: {dst = a}, v: Vector[a]): Vector[a] with Eq[a] =
        map(e -> if (e == src#src) dst#dst else e, v)

    ///
    /// Returns `true` if and only if `a` is a prefix of `b`.
    ///
    pub def isPrefixOf(a: Vector[a], b: Vector[a]): Bool with Eq[a] =
        let len1 = length(a);
        if (len1 > length(b))
            false
        else
            def loop(i) = {
                if (i >= len1)
                    true
                else if (get(i, a) != get(i, b))
                    false
                else
                    loop(i + 1)
            };
            loop(0)

    ///
    /// Returns `true` if and only if `a` is a infix of `b`.
    ///
    pub def isInfixOf(a: Vector[a], b: Vector[a]): Bool with Eq[a] =
        let len1 = length(a);
        let len2 = length(b);
        if (len1 > len2)
            false
        else if (len1 == 0)
            true
        else
            isInfixOfSearch(a, b, len1, len2, 0)

    ///
    /// Helper function for `isInfixOf` - scan `b` to find a match with first element of `b`.
    ///
    /// Precondition: len1 (length of `a`) > 0
    ///
    def isInfixOfSearch(a: Vector[a], b: Vector[a], len1: Int32, len2: Int32, j: Int32): Bool with Eq[a] =
        if (j >= len2)
            false
        else if (get(0, a) == get(j, b))
            isInfixOfCheck(a, b, len1, len2, 1, j+1)
        else
            isInfixOfSearch(a, b, len1, len2, j+1)

    ///
    /// Helper function for `isInfixOf` - `a` has started matching, scan to see if it all matches.
    ///
    def isInfixOfCheck(a: Vector[a], b: Vector[a], len1: Int32, len2: Int32, i: Int32, j: Int32): Bool with Eq[a] =
        if (i >= len1)
            // `a` exhausted, so success
            true
        else if (j >= len2)
            // `b` exhausted, `a` still trying to match, so failure
            false
        else if(get(i, a) == get(j, b))
            isInfixOfCheck(a, b, len1, len2, i+1, j+1)
        else
            isInfixOfSearch(a, b, len1, len2, j+1)

    ///
    /// Returns `true` if and only if `a` is a suffix of `b`.
    ///
    pub def isSuffixOf(a: Vector[a], b: Vector[a]): Bool with Eq[a] =
        let len1 = length(a);
        let len2 = length(b);
        if (len1 > len2)
            false
        else
            def loop(i, j) = {
                if (i < 0)
                    true
                else if (get(i, a) != get(j, b))
                    false
                else
                    loop(i - 1, j - 1)
            };
            loop(len1 - 1, len2 - 1)

    ///
    /// Returns the result of applying `combine` to all the elements in `v`, using `empty` as the initial value.
    ///
    pub def fold(v: Vector[a]): a with Monoid[a] = foldLeft(Monoid.combine, Monoid.empty(), v)

    ///
    /// Applies `f` to a start value `s` and all elements in `v` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, a[0]), a[1])..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, v: Vector[a]): b \ ef =
        let len = length(v);
        def loop(i, acc) = {
            if (i >= len)
                acc
            else
                loop(i + 1, f(acc, get(i, v)))
        };
        loop(0, s)

    ///
    /// Applies `f` to a start value `s` and all elements in `v` going from right to left.
    ///
    /// That is, the result is of the form: `f(a[0], ...f(a[n-1], f(a[n], s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, s: b, v: Vector[a]): b \ ef =
        def loop(i, acc) = {
            if (i < 0)
                acc
            else
                loop(i - 1, f(get(i, v), acc))
        };
        loop(length(v) - 1, s)

    ///
    /// Applies `f` to a start value `z` and all elements in `v` going from right to left.
    ///
    /// That is, the result is of the form: `f(a[0], ...f(a[n-1], f(a[n], z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, z: b, v: Vector[a]): b \ ef =
        def loop(i) = {
            if (i == length(v))
                z
            else
                f(get(i, v), _ -> loop(i + 1))
        };
        loop(0)

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, v: Vector[a]): b \ ef with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), v)

    ///
    /// Applies `f` to all elements in `v` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def reduceLeft(f: (a, a) ->  a \ ef, v: Vector[a]): Option[a] \ ef =
        let len = length(v);
        def loop(i, acc) = {
            if (i >= len)
                acc
            else
                loop(i + 1, f(acc, get(i, v)))
        };
        if (len == 0)
            None
        else
            Some(loop(1, get(0, v)))

    ///
    /// Applies `f` to all elements in `v` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def reduceRight(f: (a, a) -> a \ ef, v: Vector[a]): Option[a] \ ef =
        let len = length(v);
        def loop(i, acc) = {
            if (i < 0)
                acc
            else
                loop(i - 1, f(get(i, v), acc))
        };
        if (len == 0)
            None
        else
            Some(loop(len - 2, get(len - 1, v)))

    ///
    /// Returns the number of elements in `v` that satisfy the predicate `f`.
    ///
    pub def count(f: a -> Bool \ ef, v: Vector[a]): Int32 \ ef =
        foldLeft((b, x) -> if (f(x)) b + 1 else b, 0, v)

    ///
    /// Returns the sum of all elements in the vector `v`.
    ///
    pub def sum(v: Vector[Int32]): Int32 =
        foldLeft((acc, x) -> acc + x, 0, v)

    ///
    /// Returns the sum of all elements in the vector `v` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 \ ef, v: Vector[a]): Int32 \ ef =
        foldLeft((acc, x) -> acc + f(x), 0, v)

    ///
    /// Returns the concatenation of all the vectors in the vector `vs`.
    ///
    pub def flatten(vs: Vector[Vector[a]]): Vector[a] = region rc {
        let len = sumLengths(vs);
        let pos = Ref.fresh(rc, 0);
        let arr = Array.empty(rc, len);
        forEach(v -> {
            let i = Ref.get(pos);
            Ref.put(i + length(v), pos);
            arrayUpdateSeqV!(i, v, arr)
        }, vs);
        Array.toVector(arr)
    }

    ///
    /// Returns `true` if and only if at least one element in `v` satisfies the predicate `f`.
    ///
    /// Returns `false` if `v` is empty.
    ///
    pub def exists(f: a -> Bool \ ef, v: Vector[a]): Bool \ ef =
        let len = length(v);
        def loop(i) = {
            if (i >= len)
                false
            else if (f(get(i, v)))
                true
            else
                loop(i + 1)
        };
        loop(0)

    ///
    /// Returns `true` if and only if all elements in `v` satisfy the predicate `f`.
    ///
    /// Returns `true` if `v` is empty.
    ///
    pub def forAll(f: a -> Bool \ ef, v: Vector[a]): Bool \ ef =
        let len = length(v);
        def loop(i) = {
            if (i >= len)
                true
            else if (f(get(i, v)))
                loop(i + 1)
            else
                false
        };
        loop(0)

    ///
    /// Returns an array of every element in `arr` that satisfies the predicate `f`.
    ///
    pub def filter(f: a -> Bool \ ef, v: Vector[a]): Vector[a] \ ef = region rc {
        let m = MutList.empty(rc);
        forEach(a -> if (f(a)) MutList.push!(a, m) else (), v);
        MutList.toVector(m)
    }

    ///
    /// Returns a pair of vectors `(v1, v2)`.
    ///
    /// `v1` contains all elements of `v` that satisfy the predicate `f`.
    /// `v2` contains all elements of `v` that do not satisfy the predicate `f`.
    ///
    pub def partition(f: a -> Bool \ ef, v: Vector[a]): (Vector[a], Vector[a]) \ ef =
        let step = { (x, acc) ->
            let (a1, a2) = acc;
            if (f(x)) (x :: a1, a2) else (a1, x :: a2)
        };
        let (xs, ys) = foldRight(step, (Nil, Nil), v);
        (List.toVector(xs), List.toVector(ys))

    ///
    /// Returns a pair of vectors `(v1, v2)`.
    ///
    /// `v1` is the longest prefix of `v` that satisfies the predicate `f`.
    /// `v2` is the remainder of `v`.
    ///
    pub def span(f: a -> Bool \ ef, v: Vector[a]): (Vector[a], Vector[a]) \ ef =
        match findIndexOfLeft(x -> not (f(x)), v) {
            case None    => (takeLeft(length(v), v), empty())
            case Some(i) => (takeLeft(i, v), dropLeft(i, v))
        }

    ///
    /// Alias for `dropLeft`.
    ///
    pub def drop(n: Int32, v: Vector[a]): Vector[a] =
        dropLeft(n, v)

    ///
    /// Returns a copy of vector `v`, dropping the first `n` elements.
    ///
    /// Returns an empty vector if `n > length(v)`.
    ///
    pub def dropLeft( n: Int32, v: Vector[a]): Vector[a] =
        let len = length(v);
        if (n > len)
            empty()
        else {
            let start = if (n < 0) 0 else n;
            slice(start = start, end = len, v)
        }

    ///
    /// Returns a copy of vector `v`, dropping the last `n` elements.
    ///
    /// Returns an empty vector if `n > length(v)`.
    ///
    pub def dropRight(n: Int32, v: Vector[a]): Vector[a] =
        let len = length(v);
        if (n >= len)
            empty()
        else {
            let end = if (n < 0) len else len - n;
            slice(start = 0, end = end, v)
        }

    ///
    /// Alias for `dropWhileLeft`.
    ///
    pub def dropWhile(f: a -> Bool \ ef, v: Vector[a]): Vector[a] \ ef =
        dropWhileLeft(f, v)

    ///
    /// Returns copy of vector `v` without the longest prefix that satisfies the predicate `f`.
    ///
    pub def dropWhileLeft(f: a -> Bool \ ef, v: Vector[a]): Vector[a] \ ef =
        match findIndexOfLeft(x -> not (f(x)), v) {
            case None    => empty()
            case Some(i) => dropLeft(i, v)
        }

    ///
    /// Returns copy of vector `v` without the longest suffix that satisfies the predicate `f`.
    ///
    pub def dropWhileRight(f: a -> Bool \ ef, v: Vector[a]): Vector[a] \ ef =
        match findIndexOfRight(x -> not (f(x)), v) {
            case None    => empty()
            case Some(i) => slice(start = 0, end = i + 1, v)
        }

    ///
    /// Alias for `takeLeft`.
    ///
    pub def take(n: Int32, v: Vector[a]): Vector[a] =
        takeLeft(n, v)

    ///
    /// Returns a fresh vector taking first `n` elements of `v`.
    ///
    /// Returns a copy of `v` if `n > length(v)`.
    ///
    pub def takeLeft(n: Int32, v: Vector[a]): Vector[a] =
        if (n <= 0)
            empty()
        else {
            let len = length(v);
            let end = if (n > len) len else n;
            slice(start = 0, end = end, v)
        }

    ///
    /// Returns a fresh vector taking last `n` elements of `v`.
    ///
    /// Returns a copy `v` if `n > length(v)`.
    ///
    pub def takeRight(n: Int32, v: Vector[a]): Vector[a] =
        if (n <= 0)
            empty()
        else {
            let len = length(v);
            let start = if (n > len) 0 else len - n;
            slice( start = start, end = len, v)
        }

    ///
    /// Alias for `takeWhileLeft`.
    ///
    pub def takeWhile(f: a -> Bool \ ef, a: Vector[a]): Vector[a] \ ef =
        takeWhileLeft(f, a)

    ///
    /// Returns the longest prefix of `v` that satisfies the predicate `f`.
    ///
    pub def takeWhileLeft(f: a -> Bool \ ef, v: Vector[a]): Vector[a] \ ef =
        match findIndexOfLeft(x -> not (f(x)), v) {
            case None    => slice(start = 0, end = length(v), v)
            case Some(i) => takeLeft(i, v)
        }

    ///
    /// Returns the longest suffix of `v` that satisfies the predicate `f`.
    ///
    pub def takeWhileRight(f: a -> Bool \ ef, v: Vector[a]): Vector[a] \ ef =
        match findIndexOfRight(x -> not (f(x)), v) {
            case None    => slice(start = 0, end = length(v), v)
            case Some(i) => slice(start = i + 1, end = length(v), v)
        }

    ///
    /// Split the vector `v` at the position `n` returning the left and right parts.
    /// Position `n` is included in the right part.
    ///
    /// Example: `splitAt(2, Vector#{1, 2, 3, 4})` returns `(Vector#{1, 2}, Vector#{3, 4})`
    ///
    /// Returns `(v, Vector#{})` if `n > length(xs)`.
    /// Returns `(Vector#{}, v)` if `n < 0`.
    ///
    pub def splitAt(n: Int32, v: Vector[a]): (Vector[a], Vector[a]) =
        (Vector.take(n, v), Vector.drop(n, v))

    ///
    /// Partitions `v` into subvectors such that for any two elements `x` and `y` in a subvector, `f(x, y)` is true.
    ///
    /// A subvector is created by iterating through the remaining elements of `v` from left to right and adding an
    /// element to the subvector if and only if doing so creates no conflicts with the elements already in the subvector.
    ///
    /// The function `f` must be pure.
    ///
    pub def groupBy(f: (a, a) -> Bool, v: Vector[a]): Vector[Vector[a]] =
        let xs = toList(v);
        groupByHelper(f, xs, Nil) |> List.toVector

    ///
    /// Helper function for `groupBy`.
    ///
    def groupByHelper(f: (a, a) -> Bool, xs: List[a], ac: List[Vector[a]]): List[Vector[a]] = match xs {
        case Nil     => List.reverse(ac)
        case x :: rs =>
            let (r1, r2) = extractHelper(f, rs, x :: Nil, Nil);
            groupByHelper(f, r2, r1 :: ac)
    }

    ///
    /// Helper function for `groupBy`.
    ///
    def extractHelper(f: (a, a) -> Bool, xs: List[a], ps: List[a], ns: List[a]): (Vector[a], List[a]) = match xs {
        case Nil => {
            let a = List.reverse(ps);
            (List.toVector(a), List.reverse(ns))
        }
        case x :: rs =>
            if (agreeHelper(f, x, ps))
                extractHelper(f, rs, x :: ps, ns)
            else
                extractHelper(f, rs, ps, x :: ns)
    }

    ///
    /// Helper function for `groupBy`.
    ///
    def agreeHelper(f: (a, a) -> Bool, a: a, xs: List[a]): Bool = match xs {
        case Nil => true
        case x :: rs =>
            if ((f(x, a) and f(a, x)) == false)
                false
            else
                agreeHelper(f, a, rs)
    }

    ///
    /// Returns a vector where the element at index `i` is `(x, y)` where
    /// `x` is the element at index `i` in `a` and `y` is the element at index `i` in `b`.
    ///
    /// If either `a` or `b` becomes depleted, then no further elements are added to the resulting vector.
    ///
    pub def zip(a: Vector[a], b: Vector[b]): Vector[(a, b)] =
        let len = Int32.min(length(a), length(b));
        init(i -> (get(i, a), get(i, b)), len)

    ///
    /// Returns a vector where the element at index `i` is `f(x, y)` where
    /// `x` is the element at index `i` in `a` and `y` is the element at index `i` in `b`.
    ///
    /// If either `a` or `b` becomes depleted, then no further elements are added to the resulting vector.
    ///
    pub def zipWith(f: (a, b) -> c \ ef, a: Vector[a], b: Vector[b]): Vector[c] \ ef =
        let len = Int32.min(length(a), length(b));
        init(i -> f(get(i, a), get(i, b)), len)


    ///
    /// Returns a pair of vectors, the first containing all first components in `v`
    /// and the second containing all second components in `v`.
    ///
    pub def unzip(v: Vector[(a, b)]): (Vector[a], Vector[b]) = region rc {
        let len = length(v);
        let arr = Array.empty(rc, len);
        let brr = Array.empty(rc, len);
        forEachWithIndex((i, x) -> {
            let (l, r) = x;
            Array.put(l, i, arr);
            Array.put(r, i, brr)
            }, v);
        (Array.toVector(arr), Array.toVector(brr))
    }

    ///
    /// Alias for `foldLeft2`.
    ///
    pub def fold2(f: (c, a, b) -> c \ ef, c: c, a: Vector[a], b: Vector[b]): c \ ef =
        foldLeft2(f, c, a, b)

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `a` and `b`
    /// starting with the initial value `c` and going from left to right.
    ///
    pub def foldLeft2(f: (c, a, b) ->  c \ ef, c: c, a: Vector[a], b: Vector[b]): c \ ef =
        let lena = length(a);
        let lenb = length(b);
        def loop(i, acc) = {
            if (i >= lena or i >= lenb)
                acc
            else
                loop(i + 1, f(acc, get(i, a), get(i, b)))
        };
        loop(0, c)

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `a` and `b`
    /// starting with the initial value `c` and going from right to left.
    ///
    pub def foldRight2(f: (a, b, c) -> c \ ef, c: c, a: Vector[a], b: Vector[b]): c \ ef =
        def loop(i, j, acc) = {
            if (i < 0 or j < 0)
                acc
            else
                loop(i - 1, j - 1, f(get(i, a), get(j, b), acc))
        };
        let starta = length(a) - 1;
        let startb = length(b) - 1;
        loop(starta, startb, c)

    ///
    /// Collects the successful results of applying the partial function `f` to every element in `v`.
    ///
    pub def filterMap(f: a -> Option[b] \ ef, v: Vector[a]): Vector[b] \ ef =
        foldRight((x, xs) -> match f(x) {
            case None    => xs
            case Some(b) => b :: xs }, Nil, v) |> List.toVector

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `v`.
    ///
    /// Returns `None` if every element of `xs` is `None`.
    ///
    pub def findMap(f: a -> Option[b] \ ef, v: Vector[a]): Option[b] \ ef =
        let len = length(v);
        def loop(i) = {
            if (i >= len)
                None
            else
                let x = f(get(i, v));
                match x {
                    case Some(a) => Some(a)
                    case None    => loop(i + 1)
                }
        };
        loop(0)

    ///
    /// Returns the vector `v` as a set.
    ///
    pub def toSet(v: Vector[a]): Set[a] with Order[a] =
        foldRight(Set.insert, Set.empty(), v)

    ///
    /// Returns the association vector `v` as a map.
    ///
    /// If `v` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(v: Vector[(a, b)]): Map[a, b] with Order[a] =
        foldRight((x, m) -> Map.insert(fst(x), snd(x), m), Map.empty(), v)

    ///
    /// Returns the array `a` as a MutList.
    ///
    pub def toMutList(rc: Region[r], v: Vector[a]): MutList[a, r] \ r =
        toArray(rc, v) |> Array.toMutList(rc)

    ///
    /// Alias for `findIndexOfLeft`.
    ///
    pub def findIndexOf(f: a -> Bool \ ef, v: Vector[a]): Option[Int32] \ ef =
        findIndexOfLeft(f, v)

    ///
    /// Optionally returns the position of the first element in `v` satisfying `f`.
    ///
    pub def findIndexOfLeft(f: a -> Bool \ ef, v: Vector[a]): Option[Int32] \ ef =
        let len = length(v);
        if (len < 1)
            None
        else {
            def loop(i) = {
                if (i >= len)
                    -1
                else if (f(get(i, v)))
                    i
                else
                    loop(i + 1)
            };
            let i = loop(0);
            if (i < 0) None else Some(i)
        }

    ///
    /// Optionally returns the position of the first element in `v` satisfying `f`
    /// searching from right to left.
    ///
    pub def findIndexOfRight(f: a -> Bool \ ef, v: Vector[a]): Option[Int32] \ ef =
        let len = length(v);
        def loop(i) = {
            if (i < 0)
                -1
            else if (f(get(i, v)))
                i
            else
                loop(i - 1)
        };
        let i = loop(len - 1);
        if (i < 0) None else Some(i)

    ///
    /// Returns the positions of the all the elements in `v` satisfying `f`.
    ///
    pub def findIndices(f: a -> Bool \ ef, v: Vector[a]): Vector[Int32] \ ef = region rc {
        let l = MutList.empty(rc);
        forEachWithIndex((i, x) -> if (f(x)) MutList.push!(i, l) else (), v);
        MutList.toVector(l)
    }

    ///
    /// Build an vector of length `len` by applying `f` to the successive indices.
    ///
    pub def init(f: Int32 -> a \ ef, len: Int32): Vector[a] \ ef = region rc {
        let arr = if (len > 0) Array.empty(rc, len) else Array#{} @ rc;
        def loop(i) = {
            if (i < len) {
                Array.put(f(i), i, arr);
                loop(i + 1)
            } else ()
        };
        loop(0);
        Array.toVector(arr)
    }

    ///
    /// Returns `true` if arrays `a` and `b` have the same elements in the same order, i.e. are structurally equal.
    ///
    pub def equals(a: Vector[a], b: Vector[a]): Bool with Eq[a] =
        let alen = length(a);
        let blen = length(b);
        def loop(i) = {
            if (i >= alen)
                true
            else if (get(i, a) != get(i, b))
                false
            else
                loop(i + 1)
        };
        if (alen == blen)
            loop(0)
        else
            false

    ///
    /// Returns an iterator over `v`
    ///
    pub def iterator(rc: Region[r], v: Vector[a]): Iterator[a, r, r] \ r =
        Iterator.range(rc, 0, length(v)) |> Iterator.map(i -> get(i, v))

    ///
    /// Returns an iterator over `v` zipped with the indices of the elements.
    ///
    /// Modifying `a` while using an iterator has undefined behavior and is dangerous.
    ///
    pub def enumerator(rc: Region[r], v: Vector[a]): Iterator[(Int32, a), r, r] \ r =
        iterator(rc, v) |> Iterator.zipWithIndex


    ///
    /// Apply the effectful function `f` to all the elements in the vector `v`.
    ///
    pub def forEach(f: a -> Unit \ ef, v: Vector[a]): Unit \ ef =
        let len = length(v);
        def loop(i) = {
            if (i >= len)
                ()
            else {
                f(get(i, v));
                loop(i + 1)
            }
        };
        loop(0)

    ///
    /// Apply the effectful function `f` to all the elements in the vector `v`.
    ///
    pub def forEachWithIndex(f: (Int32, a) -> Unit \ ef, v: Vector[a]): Unit \ ef =
        let len = length(v);
        def loop(i) = {
            if (i >= len)
                ()
            else {
                f(i, get(i, v));
                loop(i + 1)
            }
        };
        loop(0)

    ///
    /// Returns a copy of `v` with the elements starting at index `i` replaced by `sub`.
    ///
    pub def updateSequence(i: Int32, sub: Vector[a], v: Vector[a]): Vector[a] =
        let end = i + length(sub);
        let len = length(v);
        let f = ix -> if (ix >= i and ix < end) get(ix - i, sub) else get(ix, v);
        init(f, len)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `v` with `sep` inserted between each element.
    ///
    pub def join(sep: String, v: Vector[a]): String with ToString[a] =
        joinWith(ToString.toString, sep, v)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `v` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String \ ef, sep: String, v: Vector[a]): String \ ef = region rc {
        use StringBuilder.append!;
        let lastSep = String.length(sep);
        let sb = StringBuilder.empty(rc);
        forEach(x -> { append!(f(x), sb); append!(sep, sb) }, v);
        StringBuilder.toString(sb) |> String.dropRight(lastSep)
    }

    ///
    /// Returns a sorted copy of vector `v`, where the elements are ordered from low to high according to
    /// their `Order` instance.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `v`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sort(v: Vector[a]): Vector[a] with Order[a] =
        sortWith(Order.compare, v)

    ///
    /// Returns a sorted copy of vector `v`, where the elements are ordered from low to high according to
    /// the `Order` instance for the values obtained by applying `f` to each element.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `v`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortBy(f: a -> b, v: Vector[a]): Vector[a] with Order[b] =
        sortWith(Order.compare `on` f, v)

    ///
    /// Returns a sorted copy of vector `v`, where the elements are ordered from low to high according to
    /// the comparison function `cmp`.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `v`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWith(cmp: (a, a) -> Comparison, v: Vector[a]): Vector[a] = region rc {
        let arr = toArray(rc, v);
        Array.sortWith!(cmp, arr);
        Array.toVector(arr)
    }

    ///
    /// Shuffles `v` using the Fisher–Yates shuffle.
    ///
    pub def shuffle(v: Vector[a]): Vector[a] \ NonDet = region rc {
        let arr = toArray(rc, v);
        Array.shuffle(arr);
        Array.toVector(arr)
    }

}
