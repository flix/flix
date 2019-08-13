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
    /// Returns the element a position `i` in the array `a`.
    ///
    /// Equivalent to the expression `a[i]`.
    ///
    pub def get[a](a: Array[a], i: Int): a = a[i]

    ///
    /// Stores the value `x` at position `i` in the array `a`.
    ///
    /// Equivalent to the expression `a[i] = x`.
    ///
    pub def put[a](a: Array[a], i: Int, x: a): Array[a] = a[i] = x; a

    ///
    /// Returns the length of the array `a`.
    ///
    pub def length[a](a: Array[a]): Int = length[a]

    ///
    /// Returns a fresh array with the elements from the array `a` from index `b` (inclusive) until index `e` (exclusive).
    ///
    /// Equivalent to the expression `a[b..e]`.
    ///
    pub def slice[a](a: Array[a], b: Int, e: Int): Array[a] = a[b..e]

//
//
//    case class ArrayLit(elms: List[TypedAst.Expression], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
//
//    case class ArrayNew(elm: TypedAst.Expression, len: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
//
}

