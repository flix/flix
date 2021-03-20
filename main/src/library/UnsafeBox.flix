/*
 *  Copyright 2021 Magnus Madsen
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
pub enum UnsafeBox[a] {
    case UnsafeBox(a, (a, a) -> Comparison, a -> String)
}

instance Eq[UnsafeBox[a]] {
    pub def eq(x: UnsafeBox[a], y: UnsafeBox[a]): Bool = match (x, y) {
        case (UnsafeBox(v1, cmp, _), UnsafeBox(v2, _, _)) => cmp(v1, v2) == Comparison.EqualTo
    }
}

instance Order[UnsafeBox[a]] {

    ///
    /// Compares the values in the boxes `x` and `y`.
    ///
    pub def compare(x: UnsafeBox[a], y: UnsafeBox[a]): Comparison = match (x, y) {
        case (UnsafeBox(v1, cmp, _), UnsafeBox(v2, _, _)) => cmp(v1, v2)
    }

    pub def less(x: UnsafeBox[a], y: UnsafeBox[a]): Bool = match (x <=> y) {
        case LessThan => true
        case _ => false
    }
    pub def lessEqual(x: UnsafeBox[a], y: UnsafeBox[a]): Bool = match (x <=> y) {
        case LessThan => true
        case EqualTo => true
        case _ => false
    }
    pub def greater(x: UnsafeBox[a], y: UnsafeBox[a]): Bool = match (x <=> y) {
        case GreaterThan => true
        case _ => false
    }
    pub def greaterEqual(x: UnsafeBox[a], y: UnsafeBox[a]): Bool = match (x <=> y) {
        case GreaterThan => true
        case EqualTo => true
        case _ => false
    }
}

instance ToString[UnsafeBox[a]] {
    pub def toString(x: UnsafeBox[a]): String = match x {
        case UnsafeBox(v, _, toString) => toString(v)
    }
}

namespace UnsafeBox {

    ///
    /// A type alias for the top element of the Java type lattice.
    ///
    type alias Any = ##java.lang.Object

    ///
    /// Boxes the given value `x`.
    ///
    pub def box[a: Order : ToString](x: a): UnsafeBox[Any] =
        UnsafeBox(x, Order.compare, ToString.toString) as UnsafeBox[Any]

    ///
    /// Lifts the given function `f` to one on boxed values.
    ///
    pub def lift1[a, t : Order : ToString](f: a -> t): UnsafeBox[Any] -> UnsafeBox[Any] =
        match (UnsafeBox(v1, _, _)) -> {
            box(f(v1 as a))
        }

    ///
    /// Lifts the given function `f` to one on boxed values.
    ///
    pub def lift2[a, b, t : Order : ToString](f: (a, b) -> t): (UnsafeBox[Any], UnsafeBox[Any]) -> UnsafeBox[Any] =
        (x: UnsafeBox[Any], y: UnsafeBox[Any]) -> match (x, y) {
            case (UnsafeBox(v1, _, _), UnsafeBox(v2, _, _)) => box(f(v1 as a, v2 as b))
        }

}
