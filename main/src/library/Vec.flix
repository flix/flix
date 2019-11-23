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

// TODO: There is already a built-in type named "Vector". We should deal with this unfortunate name clash.

///
/// Represents a mutable vector.
///
pub enum Vec[a] {
    case Vec(Array[a], Int32)
}

namespace Vec {

    /*
     * Creation operations.
     */

    ///
    /// Returns a new empty vector with a default capacity.
    ///
    pub def new(): Vec[a] = newWithCapacity(0)

    ///
    /// Returns a new empty vector with the given `capacity`.
    ///
    pub def newWithCapacity(capacity: Int32): Vec[a] = ???

    /*
     * Immutable operations.
     */

    ///
    /// Returns the capacity of the given vector `v`.
    ///
    pub def capacity(v: Vec[a]): Int32 = ???

    ///
    /// Returns `true` if the given vector `v` is empty.
    ///
    pub def isEmpty(v: Vec[a]): Bool = ???

    ///
    /// Returns `true` if the given element `x` is a member of the given vector `v`.
    ///
    pub def memberOf(v: Vec[a], x: a): Bool = ???

    ///
    /// Returns the number of elements in the given vector `v`.
    ///
    pub def length(v: Vec[a]): Int32 = ???

    ///
    /// Returns the number of elements in the given vector `v` that satisfies the given predicate `f`.
    ///
    /// Returns `0` if the given vector `v` is empty.
    ///
    pub def count(f: a -> Bool, v: Vec[a]): Int32 = ???

    ///
    /// Returns `true` if the given predicate `f` holds for all elements of the given vector `v`.
    ///
    /// Returns `true` if the given vector `v` is empty.
    ///
    pub def forall(f: a -> Bool, v: Vec[a]): Bool = ???

    ///
    /// Returns `true` if the given predicate `f` holds for at least one element of the given vector `v`.
    ///
    /// Returns `false` if the given vector `v` is empty.
    ///
    pub def exists(f: a -> Bool, v: Vec[a]): Bool = ???

    ///
    /// Optionally returns the first element of the given vector `v`.
    ///
    /// Returns `None` if the given vector `v` is empty.
    ///
    pub def head(v: Vec[a]): Option[a] = ???

    ///
    /// Optionally returns the last element of the given vector `v`.
    ///
    /// Returns `None` if the given vector `v` is empty.
    ///
    pub def last(v: Vec[a]): Option[a] = ???

    ///
    /// Alias for `findLeft`.
    ///
    pub def find(v: Vec[a], f: a -> Bool): Option[a] = findLeft(v, f)

    ///
    /// Optionally returns the left-most element in the given vector `v` that satisfies the given predicate `f`.
    ///
    /// Returns `None` if no element satisfies the given predicate `f`.
    /// Returns `None` if the given vector `v` is empty.
    ///
    pub def findLeft(v: Vec[a], f: a -> Bool): Option[a] = ???

    ///
    /// Optionally returns the right-most element in the given vector `v` that satisfies the given predicate `f`.
    ///
    /// Returns `None` if no element satisfies the given predicate `f`.
    /// Returns `None` if the given vector `v` is empty.
    ///
    pub def findRight(v: Vec[a], f: a -> Bool): Option[a] = ???

    ///
    /// Returns the index (position) of the given element `x` in the given vector `v`.
    ///
    /// Returns `-1` if the element does not occur in the given vector `v`.
    ///
    pub def indexOf(v: Vec[a], x: a): Int32 = ???

    // TODO: foldLeft
    // TODO: foldRight

    // TODO: reduceLeft
    // TODO: reduceRight

    /*
     * Mutating operations.
     */

    ///
    /// Removes all elements from the given vector `v`.
    ///
    /// Does not shrink its capacity.
    ///
    pub def clear(v: mut Vec[a]): Unit = ???

    ///
    /// Returns a shallow copy of the given vector `v`.
    ///
    pub def copy(v: Vec[a]): Vec[a] = ???

    ///
    /// Optionally removes and returns the last element in the given vector `v`.
    ///
    pub def pop(v: mut Vec[a]): Option[a] = ???

    ///
    /// Inserts the given element `x` at the end of the given vector `v`.
    ///
    pub def push(v: mut Vec[a], x: a): Unit = ???

    ///
    /// Inserts the given element `x` at the given position `i` in the given vector `v`.
    ///
    /// Shifts elements as necessary. Possibly expensive operation.
    ///
    /// If the given index `i` exceeds the length of the vector, the element is inserted at the last position.
    ///
    pub def insert(v: mut Vec[a], x: a, i: Int32): Unit = ???

    ///
    /// Removes the element at the given position `i` in the given vector `v`.
    ///
    /// Shifts elements as necessary. Possibly expensive operation.
    ///
    /// If the given index `i` exceeds the length of the vector, no element is removed.
    ///
    pub def remove(v: mut Vec[a], i: Int32): Unit = ???

    ///
    /// Inserts all the elements in the given vector `v2` into the vector `v1`.
    ///
    pub def append(v1: mut Vec[a], v2: Vec[a]): Unit = ???

    ///
    /// Removes all elements from the given vector `v` that do not satisfy the given predicate `f`.
    ///
    pub def retain(v: mut Vec[a], f: a -> Bool): Unit = ???

    ///
    /// Inserts the element `x` between all elements in the given vector `v`.
    ///
    /// Does not insert the element before or after the last element(s).
    ///
    pub def intersperse(v: mut Vec[a], x: a): Unit = ???

    ///
    /// Replaces all occurrences of the `x` with `y` in the given vector `v`.
    ///
    pub def replace(v: mut Vec[a], x: a, y: a): Unit = ???

    ///
    /// Reverses the order of the elements in the given vector `v`.
    ///
    pub def reverse(v: mut Vec[a]): Unit = ???

    ///
    /// Shrinks the given vector `v` to its actual size.
    ///
    pub def shrink(v: mut Vec[a]): Unit = ???

    ///
    /// Truncates the given vector `v` to the given length `l`.
    ///
    /// That is, after the operation, the vector has length at most `l`.
    ///
    /// If the given length `l` is negative, all elements are removed.
    ///
    pub def truncate(v: mut Vec[a], l: Int32): Unit = ???

    ///
    /// Increases the capacity of the given vector `v` by at least `n`.
    ///
    /// That is, after the call, the vector is guaranteed to have space for at least `n` additional elements.
    ///
    /// The content of the vector is unchanged.
    ///
    pub def reserve(v: mut Vec[a], n: Int32): Unit = ???

}

