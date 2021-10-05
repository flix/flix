/*
 * Copyright 2021 Jakob Schneider Villumsen, Magnus Madsen
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




namespace DemandList {

/*
    pub def example1(): Int32 =
        let l1 = range(1, 10);
        let l2 = filter(x -> x > 5, l1);
        sum(l2)

    pub def example2(): Option[Int32] =
        let l1 = range(1, 10);
        let l2 = filter(x -> x > 5, l1);
        head(l2)

    pub def example3(): Option[Int32] & Impure =
        let l1 = range(1, 10);
        let l2 = filter(x -> x > 5 as & Impure, l1);
        head(l2)
*/

    ///
    /// Represents a lazy list.
    ///
    /// If the Boolean type `f` is equiv. to true then all elements are forced.
    ///
    pub enum DemandList[a: Type, f: Bool] {
        case ENil,
        case ECons(a, DemandList[a, f]),
        case LCons(a, Lazy[DemandList[a, f]]),
        case LList(Lazy[DemandList[a, f]])
    }

    type alias Forced = true

    ///
    /// Returns `Some(x)` if `x` is the first element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Does not force the tail of `l`.
    ///
    pub def head(l: DemandList[a, _]): Option[a] = match l {
        case ENil        => None
        case ECons(x, _) => Some(x)
        case LCons(x, _) => Some(x)
        case LList(xs)   => head(force xs)
    }

    ///
    /// Returns a lazy list of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty lazy list if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): DemandList[Int32, _] =
        LList(lazy rangeHelper(b, e))

    ///
    /// Helper function for `range`.
    ///
    def rangeHelper(b: Int32, e:Int32): DemandList[Int32, _] =
        if (b >= e)
            ENil
        else
            LCons(b, lazy rangeHelper(b + 1, e))

    ///
    /// Returns the length of `l`.
    ///
    /// Forces the entire list `l`.
    ///
    pub def length(l: DemandList[a, Forced]): Int32 =
        lengthAcc(l, 0)

    ///
    /// Helper function for `length`
    ///
    def lengthAcc(l: DemandList[a, Forced], acc: Int32): Int32 = match l {
        case ENil         => acc
        case ECons(_, xs) => lengthAcc(      xs, acc + 1)
        case LCons(_, xs) => lengthAcc(force xs, acc + 1)
        case LList(xs)    => lengthAcc(force xs, acc)
    }

    ///
    /// Returns the sum of all elements in the list `l`.
    ///
    pub def sum(l: DemandList[Int32, Forced]): Int32 =
        sumAcc(l, 0)

    ///
    /// Helper function for `sum`.
    ///
    def sumAcc(l: DemandList[Int32, Forced], acc: Int32): Int32 = match l {
        case ENil         => acc
        case ECons(x, xs) => sumAcc(      xs, x + acc)
        case LCons(x, xs) => sumAcc(force xs, x + acc)
        case LList(xs)    => sumAcc(force xs,     acc)
    }


    ///
    /// Returns a lazy list of every element in `l` that satisfies the predicate `f`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    pub def filter(f: a -> Bool & ef, l: DemandList[a, d]): DemandList[a, ef and d] & ef =
        if (reify ef)
            filterL(f as a -> Bool & Pure,   l) as DemandList[a, ef and d] & ef
        else
            filterE(f as a -> Bool & Impure, l) as DemandList[a, ef and d] & ef

    ///
    /// Returns a lazy list of every element in `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    def filterL(f: a -> Bool, l: DemandList[a, f]): DemandList[a, f] = match l {
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
    def filterE(f: a -> Bool & Impure, l: DemandList[a, f]): DemandList[a, f] & Impure =
        filterEAcc(f, l as DemandList[a, Forced], ENil) |> reverse

    ///
    /// Helper function for `filterE`
    ///
    def filterEAcc(f: a -> Bool & Impure, l: DemandList[a, f], acc: DemandList[a, f]): DemandList[a, f] & Impure = match l {
        case ENil         => acc
        case ECons(x, xs) => if (f(x)) filterEAcc(f,       xs, ECons(x, acc)) else filterEAcc(f,       xs, acc)
        case LCons(x, xs) => if (f(x)) filterEAcc(f, force xs, ECons(x, acc)) else filterEAcc(f, force xs, acc)
        case LList(xs)    => filterEAcc(f, force xs, acc)
    }

    ///
    /// Reverses the list `l`.
    ///
    pub def reverse(l: DemandList[a, Forced]): DemandList[a, _] =
        reverseAcc(l, ENil)

    ///
    /// Helper function for `reverse`.
    ///
    def reverseAcc(l: DemandList[a, Forced], acc: DemandList[a, f]): DemandList[a, f] = match l {
        case ENil         => acc
        case ECons(x, xs) => LList(lazy reverseAcc(      xs, ECons(x, acc)))
        case LCons(x, xs) => LList(lazy reverseAcc(force xs, ECons(x, acc)))
        case LList(xs)    => LList(lazy reverseAcc(force xs,          acc))
    }

}