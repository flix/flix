/*
 * Copyright 2016 Liam Palmer
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

namespace List {

    /**
     * Returns the number of elements in `xs`.
     */
    def size[a](xs: List[a]): Int32 = match xs with {
        case Nil => 0
        case y :: ys => size(ys) + 1
    }

    /**
     * Returns the first element of `xs`.
     * Throws an exception if `xs` is empty.
     */
    def head[a](xs: List[a]): a = match xs with {
        case Nil => ???
        case y :: ys => y
    }

    /**
     * Returns `xs` with the first element removed.
     * Throws an exception if `xs` is empty.
     */
    def tail[a](xs: List[a]): List[a] = match xs with {
        case Nil => ???
        case y :: ys => ys
    }

    /**
     * Returns `xs` with `x` prepended to the beginning.
     */
    def prepend[a](xs: List[a], x: a): List[a] = x :: xs

    /**
     * Returns `xs` with `x` appended to the end.
     */
    def append[a](xs: List[a], x: a): List[a] = match xs with {
        case Nil => x :: xs
        case y :: ys => y :: append(ys, x)
    }

    /**
     * Returns `xs` with `x` inserted at position `index`.
     * The element at position `index` (if any) along with any subsequent
     * elements to the right are shifted to the right.
     * Throws an exception if index < 0 or index > size()
     */
    def add[a](xs: List[a], x: a, index: Int32): List[a] = match xs with {
        case Nil => if (index == 0) x :: xs else ???
        case y :: ys => if (index == 0) x :: xs else y :: add(ys, x, index - 1)
    }

    /**
     * Returns `xs1` concatenated to `xs2`.
     */
    def concat[a](xs1: List[a], xs2: List[a]): List[a] = match xs1 with {
        case Nil => xs2
        case y :: ys => y :: concat(ys, xs2)
    }

    /**
     * Returns the element at position `index` in `xs`.
     * Throws an exception if index < 0 or index > size() - 1
     */
    def get[a](xs: List[a], index: Int32): a = match xs with {
        case Nil => ???
        case y :: ys => if (index == 0) y else get(ys, index - 1)
    }

    /**
     * Returns true if `xs` contains element `x`, false otherwise.
     */
    def contains[a](xs: List[a], x: a): Bool = match xs with {
        case Nil => false
        case y :: ys => if (y == x) true else contains(ys, x)
    }

    /**
     * Returns true if `xs1` contains every element in `xs2`, false otherwise.
     */
    def containsAll[a](xs1: List[a], xs2: List[a]): Bool = match xs2 with {
        case Nil => true
        case y :: ys => if (contains(xs1, y)) containsAll(xs1, ys) else false
    }

    /**
     * Returns true if `xs1` and `xs2` contain the same elements in the
     * same order, false otherwise.
     */
    def equals[a](xs1: List[a], xs2: List[a]): Bool = switch {
        case size(xs1) != size(xs2) => false
        case size(xs1) == 0         => true
        case size(xs1) != 0         => if (head(xs1) == head(xs2)) equals(tail(xs1), tail(xs2)) else false
    }

    /**
     * Returns true if `xs` is empty, false otherwise.
     */
    def isEmpty[a](xs: List[a]): Bool = match xs with {
        case Nil => true
        case y :: ys => false
    }

    /**
     * Returns the index of the first occurrence of element `x` in `xs`.
     * Returns -1 is `xs` does not contain `x`
     */
    def indexOf[a](xs: List[a], x: a): Int32 = indexOfHelper(xs, x, 0)

    def indexOfHelper[a](xs: List[a], x: a, index: Int32): Int32 = match xs with {
        case Nil => -1
        case y :: ys => if (x == y) index else indexOfHelper(ys, x, index + 1)
    }
}
