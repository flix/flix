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

namespace Array {

    ///
    /// Returns a fresh array of length `l` filled with the element `x`.
    ///
    /// Equivalent to the expression `[x; l]`.
    ///
    pub def new(x: a, l: Int): Array[a] & Impure = [x; l]

    ///
    /// Returns the element at position `i` in the array `a`.
    ///
    /// Equivalent to the expression `a[i]`.
    ///
    pub def get(a: Array[a], i: Int): a & Impure = a[i]

    ///
    /// Stores the value `x` at position `i` in the array `a`.
    ///
    /// Equivalent to the expression `a[i] = x`.
    ///
    pub def put(a: Array[a], i: Int, x: a): Array[a] & Impure = a[i] = x; a

    ///
    /// Returns the length of the array `a`.
    ///
    pub def length(a: Array[a]): Int = a.length

    ///
    /// Returns a fresh array with the elements from the array `a` from index `b` (inclusive) until index `e` (exclusive).
    ///
    /// Equivalent to the expression `a[b..e]`.
    ///
    pub def slice(a: Array[a], b: Int, e: Int): Array[a] & Impure = a[b..e]

    ///
    /// Returns the array `a` as a list.
    ///
    pub def toList(a: Array[a]): List[a] & Impure = toListHelper(a, 0)

    ///
    /// Helper for toList.
    ///
    def toListHelper(a: Array[a], i: Int): List[a] & Impure =
        if (i == length(a))
            Nil
        else
            a[i] :: toListHelper(a, i + 1)

}
