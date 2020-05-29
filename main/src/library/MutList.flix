/*
 * Copyright 2019 Magnus Madsen
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
/// Represents a mutable list.
///
pub enum MutList[a] {
    case MutList(Array[a], Int32)
}

namespace MutList {

    /*
     * Creation operations.
     */

    ///
    /// Returns a new empty mutable list with a default capacity.
    ///
    pub def new(): MutList[a] = newWithCapacity(0)

    ///
    /// Returns a new empty mutable list with the given `capacity`.
    ///
    pub def newWithCapacity(capacity: Int32): MutList[a] = ???

    /*
     * Immutable operations.
     */

    ///
    /// Returns the capacity of the given mutable list `v`.
    ///
    pub def capacity(v: MutList[a]): Int32 = ???

    ///
    /// Returns `true` if the given mutable list `v` is empty.
    ///
    pub def isEmpty(v: MutList[a]): Bool = ???

    ///
    /// Returns `true` if the given element `x` is a member of the given mutable list `v`.
    ///
    pub def memberOf(v: MutList[a], x: a): Bool = ???

    ///
    /// Returns the number of elements in the given mutable list `v`.
    ///
    pub def length(v: MutList[a]): Int32 = ???

    ///
    /// Returns the number of elements in the given mutable list `v` that satisfies the given predicate `f`.
    ///
    /// Returns `0` if the given mutable list `v` is empty.
    ///
    pub def count(f: a -> Bool, v: MutList[a]): Int32 = ???

    ///
    /// Returns `true` if the given predicate `f` holds for all elements of the given mutable list `v`.
    ///
    /// Returns `true` if the given mutable list `v` is empty.
    ///
    pub def forall(f: a -> Bool, v: MutList[a]): Bool = ???

    ///
    /// Returns `true` if the given predicate `f` holds for at least one element of the given mutable list `v`.
    ///
    /// Returns `false` if the given mutable list `v` is empty.
    ///
    pub def exists(f: a -> Bool, v: MutList[a]): Bool = ???

    ///
    /// Optionally returns the first element of the given mutable list `v`.
    ///
    /// Returns `None` if the given mutable list `v` is empty.
    ///
    pub def head(v: MutList[a]): Option[a] = ???

    ///
    /// Optionally returns the last element of the given mutable list `v`.
    ///
    /// Returns `None` if the given mutable list `v` is empty.
    ///
    pub def last(v: MutList[a]): Option[a] = ???

    ///
    /// Alias for `findLeft`.
    ///
    pub def find(v: MutList[a], f: a -> Bool): Option[a] = findLeft(v, f)

    ///
    /// Optionally returns the left-most element in the given mutable list `v` that satisfies the given predicate `f`.
    ///
    /// Returns `None` if no element satisfies the given predicate `f`.
    /// Returns `None` if the given mutable list `v` is empty.
    ///
    pub def findLeft(v: MutList[a], f: a -> Bool): Option[a] = ???

    ///
    /// Optionally returns the right-most element in the given mutable list `v` that satisfies the given predicate `f`.
    ///
    /// Returns `None` if no element satisfies the given predicate `f`.
    /// Returns `None` if the given mutable list `v` is empty.
    ///
    pub def findRight(v: MutList[a], f: a -> Bool): Option[a] = ???

    ///
    /// Returns the index (position) of the given element `x` in the given mutable list `v`.
    ///
    /// Returns `-1` if the element does not occur in the given mutable list `v`.
    ///
    pub def indexOf(v: MutList[a], x: a): Int32 = ???

    // TODO: foldLeft
    // TODO: foldRight

    // TODO: reduceLeft
    // TODO: reduceRight

    // TODO: Additional operations from Array.

    /*
     * Mutating operations.
     */

    ///
    /// Removes all elements from the given mutable list `v`.
    ///
    /// Does not shrink its capacity.
    ///
    pub def clear(v: mut MutList[a]): Unit = ???

    ///
    /// Returns a shallow copy of the given mutable list `v`.
    ///
    pub def copy(v: MutList[a]): MutList[a] = ???

    ///
    /// Optionally removes and returns the last element in the given mutable list `v`.
    ///
    pub def pop(v: mut MutList[a]): Option[a] = ???

    ///
    /// Inserts the given element `x` at the end of the given mutable list `v`.
    ///
    pub def push(v: mut MutList[a], x: a): Unit = ???

    ///
    /// Inserts the given element `x` at the given position `i` in the given mutable list `v`.
    ///
    /// Shifts elements as necessary. Possibly expensive operation.
    ///
    /// If the given index `i` exceeds the length of the mutable list, the element is inserted at the last position.
    ///
    pub def insert(v: mut MutList[a], x: a, i: Int32): Unit = ???

    ///
    /// Removes the element at the given position `i` in the given mutable list `v`.
    ///
    /// Shifts elements as necessary. Possibly expensive operation.
    ///
    /// If the given index `i` exceeds the length of the mutable list, no element is removed.
    ///
    pub def remove(v: mut MutList[a], i: Int32): Unit = ???

    ///
    /// Inserts all the elements in the given mutable list `v2` into the mutable list `v1`.
    ///
    pub def append(v1: mut MutList[a], v2: MutList[a]): Unit = ???

    ///
    /// Removes all elements from the given mutable list `v` that do not satisfy the given predicate `f`.
    ///
    pub def retain(v: mut MutList[a], f: a -> Bool): Unit = ???

    ///
    /// Inserts the element `x` between all elements in the given mutable list `v`.
    ///
    /// Does not insert the element before or after the last element(s).
    ///
    pub def intersperse(v: mut MutList[a], x: a): Unit = ???

    ///
    /// Replaces all occurrences of the `x` with `y` in the given mutable list `v`.
    ///
    pub def replace(v: mut MutList[a], x: a, y: a): Unit = ???

    ///
    /// Reverses the order of the elements in the given mutable list `v`.
    ///
    pub def reverse(v: mut MutList[a]): Unit = ???

    ///
    /// Shrinks the given mutable list `v` to its actual size.
    ///
    pub def shrink(v: mut MutList[a]): Unit = ???

    ///
    /// Truncates the given mutable list `v` to the given length `l`.
    ///
    /// That is, after the operation, the mutable list has length at most `l`.
    ///
    /// If the given length `l` is negative, all elements are removed.
    ///
    pub def truncate(v: mut MutList[a], l: Int32): Unit = ???

    ///
    /// Increases the capacity of the given mutable list `v` by at least `n`.
    ///
    /// That is, after the call, the mutable list is guaranteed to have space for at least `n` additional elements.
    ///
    /// The content of the mutable list is unchanged.
    ///
    pub def reserve(v: mut MutList[a], n: Int32): Unit = ???

}

