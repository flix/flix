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


namespace Iterator {

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
    /// Returns an iterator of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty iterator if `b >= e`.
    ///
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
    /// Returns an iterator over an iterable with the element `x` repeated `n` times.
    ///
    /// Returns an empty iterator if `n < 0`.
    ///
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
    /// Returns an array with the elements of `iter`.
    ///
    pub def toArray(iter: Iterator[a]): Array[a] & Impure =
        let Iterator(done, next) = iter;
        toArrayAcc(done, next, MutList.new())

    ///
    /// Helper function for `toArray`.
    ///
    def toArrayAcc(done: Done, next: Next[a], v: MutList[a]): Array[a] & Impure =
        if (done()) {
            MutList.toArray(v)
        }
        else {
            MutList.push!(next(), v);
            toArrayAcc(done, next, v)
        }

    ///
    /// Returns the contents of `iter` as a list. Consumes the entire iterator.
    ///
    pub def toList(iter: Iterator[a]): List[a] & Impure =
        let Iterator(done, next) = iter;
        toListAcc(done, next, ks -> ks)

    ///
    /// Helper function for `toList`.
    ///
    def toListAcc(done: Done, next: Next[a], k: List[a] -> List[a]): List[a] & Impure =
        if (done()) {
            k(Nil)
        }
        else {
            let x = next();
            toListAcc(done, next, ks -> k(x :: ks))
        }

    ///
    /// Returns the contents of `iter` as a map. Consumes the entire iterator.
    ///
    pub def toMap(iter: Iterator[(a, b)]): Map[a, b] & Impure with Order[a] =
        let Iterator(done, next) = iter;
        toMapAcc(done, next, Map.empty())

    ///
    /// Helper function for `toMap`.
    ///
    def toMapAcc(done: Done, next: Next[(a, b)], m: Map[a, b]): Map[a, b] & Impure with Order[a] =
        if (done())
            m
        else
            let (k, v) = next();
            toMapAcc(done, next, Map.insert(k, v, m))

    ///
    /// Returns the contents of `iter` as a `Some(Nel)` if `iter` is not empty. Returns None otherwise.
    ///
    pub def toNel(iter: Iterator[a]): Option[Nel[a]] & Impure = match toList(iter) {
        case Nil     => None
        case x :: xs => Some(Nel(x, xs))
    }

    ///
    /// Returns the contents of `iter` as a set. Consumes the entire iterator.
    ///
    pub def toSet(iter: Iterator[a]): Set[a] & Impure with Order[a] =
        let Iterator(done, next) = iter;
        toSetAcc(done, next, Set.empty())

    ///
    /// Helper function for `toSet`.
    ///
    def toSetAcc(done: Done, next: Next[a], s: Set[a]): Set[a] & Impure with Order[a] =
        if (done())
            s
        else
            toSetAcc(done, next, Set.insert(next(), s))

    ///
    /// Returns a list of every element in `iter` that satisfies the predicate `f`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def filter(f: a -> Bool & ef, iter: Iterator[a]): List[a] & Impure =
        let Iterator(done, next) = iter;
        filterAcc(f, done, next, ks -> ks)

    ///
    /// Helper function for `filter`.
    ///
    def filterAcc(f: a -> Bool & ef, done: Done, next: Next[a], k: List[a] -> List[a]): List[a] & Impure =
        if (done())
            k(Nil)
        else
            let x = next();
            let include = f(x);
            if (include) filterAcc(f, done, next, ks -> k(x :: ks)) else filterAcc(f, done, next, k)

    ///
    /// Returns a list with `f` applied to every element in `iter`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def map(f: a -> b & ef, iter: Iterator[a]): List[b] & Impure =
        let Iterator(done, next) = iter;
        mapAcc(f, done, next, ks -> ks)

    ///
    /// Helper function for `map`.
    ///
    def mapAcc(f: a -> b & ef, done: Done, next: Next[a], k: List[b] -> List[b]): List[b] & Impure =
        if (done())
            k(Nil)
        else
            let x = f(next());
            mapAcc(f, done, next, ks -> k(x :: ks))

    ///
    /// Applies `f` to every element of `iter`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def foreach(f: a -> Unit & ef, iter: Iterator[a]): Unit & Impure =
        let Iterator(done, next) = iter;
        foreachHelper(f, done, next)

    ///
    /// Helper function for `foreach`.
    ///
    def foreachHelper(f: a -> Unit & ef, done: Done, next: Next[a]): Unit & Impure =
        if (done())
            ()
        else
            let _ = f(next());
            foreachHelper(f, done, next)

    ///
    /// Returns an iterator with every element of the iterator `iter` that
    /// satisfies the predicate `f`.
	///
	/// Does *not* consume any elements from the iterator.
	///
	/// The original iterator `iter` should not be reused.
    ///
	pub def lazyFilter(f: a -> Bool, iter: Iterator[a]): Iterator[a] & Impure =
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
        if (done())
            None
        else {
            let e = next();
            if (f(e)) Some(e) else forward(f, done, next)
        }

    ///
    /// Returns an iterator with every `f` lazily applied to each element in `iter`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    pub def lazyMap(f: a -> b, iter: Iterator[a]): Iterator[b] =
        let Iterator(done, next) = iter;
        let next1 = () -> f(next());
        Iterator(done, next1)

    ///
    /// Returns an iterator with every occurrence of `x` replaced by `y` in `iter`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    pub def replace(x: a, y: a, iter: Iterator[a]): Iterator[a] with Eq[a] =
        let Iterator(done, next) = iter;
        let next1 = () -> {
            let el = next();
            if (el == x) y else el
        };
        Iterator(done, next1)
}
