/*
 * Copyright 2020 Esben Bjerre
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

use Core/Cmp.Ordering;
use Core/Cmp.Ordering.{LessThan, Equal, GreaterThan};

///
/// A type class for types with a total order.
///
pub class Ord[a] {

    /// Returns `true` if and only if `x` < `y`.
    pub def less(x: a, y: a): Bool

    /// Returns `true` if and only if `x` <= `y`.
    pub def lessEqual(x: a, y: a): Bool

    /// Returns `true` if and only if `x` > `y`.
    pub def greater(x: a, y: a): Bool

    /// Returns `true` if and only if `x` >= `y`.
    pub def greaterEqual(x: a, y: a): Bool

    ///
    /// Returns `LessThan` if `x` < `y`, `Equal` if `x` == `y` or `GreaterThan` `if `x` > `y`.
    ///
    pub def compare(x: a, y: a): Ordering

}

instance Ord[Unit] {

    def less(x: Unit, y: Unit): Bool = false
    def lessEqual(x: Unit, y: Unit): Bool = true
    def greater(x: Unit, y: Unit): Bool = false
    def greaterEqual(x: Unit, y: Unit): Bool = true
    pub def compare(x: Unit, y: Unit): Ordering = Equal

}

instance Ord[Bool] {

    def less(x: Bool, y: Bool): Bool = false
    def lessEqual(x: Bool, y: Bool): Bool = true
    def greater(x: Bool, y: Bool): Bool = false
    def greaterEqual(x: Bool, y: Bool): Bool = true
    pub def compare(x: Bool, y: Bool): Ordering = match (x, y) {
        case (true, true) => Equal
        case (false, false) => Equal
        case (false, true) => LessThan
        case (true, false) => GreaterThan
    }

}

instance Ord[Char] {

    def less(x: Char, y: Char): Bool = $CHAR_LT$(x, y)
    def lessEqual(x: Char, y: Char): Bool = $CHAR_LE$(x, y)
    def greater(x: Char, y: Char): Bool = $CHAR_GT$(x, y)
    def greaterEqual(x: Char, y: Char): Bool = $CHAR_GE$(x, y)
    pub def compare(x: Char, y: Char): Ordering = match (x <= y, y <= x) {
        case (true, true) => Equal
        case (true, false) => LessThan
        case (false, true) => GreaterThan
    }

}

instance Ord[Float32] {

    def less(x: Float32, y: Float32): Bool = $FLOAT32_LT$(x, y)
    def lessEqual(x: Float32, y: Float32): Bool = $FLOAT32_LE$(x, y)
    def greater(x: Float32, y: Float32): Bool = $FLOAT32_GT$(x, y)
    def greaterEqual(x: Float32, y: Float32): Bool = $FLOAT32_GE$(x, y)
    pub def compare(x: Float32, y: Float32): Ordering = match (x <= y, y <= x) {
        case (true, true) => Equal
        case (true, false) => LessThan
        case (false, true) => GreaterThan
    }

}

instance Ord[Float64] {

    def less(x: Float64, y: Float64): Bool = $FLOAT64_LT$(x, y)
    def lessEqual(x: Float64, y: Float64): Bool = $FLOAT64_LE$(x, y)
    def greater(x: Float64, y: Float64): Bool = $FLOAT64_GT$(x, y)
    def greaterEqual(x: Float64, y: Float64): Bool = $FLOAT64_GE$(x, y)
    pub def compare(x: Float64, y: Float64): Ordering = match (x <= y, y <= x) {
        case (true, true) => Equal
        case (true, false) => LessThan
        case (false, true) => GreaterThan
    }

}

instance Ord[Int8] {

    def less(x: Int8, y: Int8): Bool = $INT8_LT$(x, y)
    def lessEqual(x: Int8, y: Int8): Bool = $INT8_LE$(x, y)
    def greater(x: Int8, y: Int8): Bool = $INT8_GT$(x, y)
    def greaterEqual(x: Int8, y: Int8): Bool = $INT8_GE$(x, y)
    pub def compare(x: Int8, y: Int8): Ordering = match (x <= y, y <= x) {
        case (true, true) => Equal
        case (true, false) => LessThan
        case (false, true) => GreaterThan
    }

}

instance Ord[Int16] {

    def less(x: Int16, y: Int16): Bool = $INT16_LT$(x, y)
    def lessEqual(x: Int16, y: Int16): Bool = $INT16_LE$(x, y)
    def greater(x: Int16, y: Int16): Bool = $INT16_GT$(x, y)
    def greaterEqual(x: Int16, y: Int16): Bool = $INT16_GE$(x, y)
    pub def compare(x: Int16, y: Int16): Ordering = match (x <= y, y <= x) {
        case (true, true) => Equal
        case (true, false) => LessThan
        case (false, true) => GreaterThan
    }

}

instance Ord[Int32] {

    def less(x: Int32, y: Int32): Bool = $INT32_LT$(x, y)
    def lessEqual(x: Int32, y: Int32): Bool = $INT32_LE$(x, y)
    def greater(x: Int32, y: Int32): Bool = $INT32_GT$(x, y)
    def greaterEqual(x: Int32, y: Int32): Bool = $INT32_GE$(x, y)
    pub def compare(x: Int32, y: Int32): Ordering = match (x <= y, y <= x) {
        case (true, true) => Equal
        case (true, false) => LessThan
        case (false, true) => GreaterThan
    }

}

instance Ord[Int64] {

    def less(x: Int64, y: Int64): Bool = $INT64_LT$(x, y)
    def lessEqual(x: Int64, y: Int64): Bool = $INT64_LE$(x, y)
    def greater(x: Int64, y: Int64): Bool = $INT64_GT$(x, y)
    def greaterEqual(x: Int64, y: Int64): Bool = $INT64_GE$(x, y)
    pub def compare(x: Int64, y: Int64): Ordering = match (x <= y, y <= x) {
        case (true, true) => Equal
        case (true, false) => LessThan
        case (false, true) => GreaterThan
    }

}

instance Ord[BigInt] {

    def less(x: BigInt, y: BigInt): Bool = $BIGINT_LT$(x, y)
    def lessEqual(x: BigInt, y: BigInt): Bool = $BIGINT_LE$(x, y)
    def greater(x: BigInt, y: BigInt): Bool = $BIGINT_GT$(x, y)
    def greaterEqual(x: BigInt, y: BigInt): Bool = $BIGINT_GE$(x, y)
    pub def compare(x: BigInt, y: BigInt): Ordering = match (x <= y, y <= x) {
        case (true, true) => Equal
        case (true, false) => LessThan
        case (false, true) => GreaterThan
    }

}

instance Ord[(a1, a2)] with [a1 : Ord, a2: Ord] {

    def less(x: (a1, a2), y: (a1, a2)): Bool = match (x <=> y) {
        case LessThan => true
        case _ => false
    }
    def lessEqual(x: (a1, a2), y: (a1, a2)): Bool = match (x <=> y) {
        case LessThan => true
        case Equal => true
        case _ => false
    }
    def greater(x: (a1, a2), y: (a1, a2)): Bool = match (x <=> y) {
        case GreaterThan => true
        case _ => false
    }
    def greaterEqual(x: (a1, a2), y: (a1, a2)): Bool = match (x <=> y) {
        case GreaterThan => true
        case Equal => true
        case _ => false
    }

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    def compare(t1: (a1, a2), t2: (a1, a2)): Ordering =
        let (x1, x2) = t1;
        let (y1, y2) = t2;
            let cmp1 = x1 <=> y1;
            if (cmp1 != Equal)
                cmp1
            else
                x2 <=> y2

}

instance Ord[(a1, a2, a3)] with [a1 : Ord, a2: Ord, a3 : Ord] {

    def less(x: (a1, a2, a3), y: (a1, a2, a3)): Bool = match (x <=> y) {
        case LessThan => true
        case _ => false
    }
    def lessEqual(x: (a1, a2, a3), y: (a1, a2, a3)): Bool = match (x <=> y) {
        case LessThan => true
        case Equal => true
        case _ => false
    }
    def greater(x: (a1, a2, a3), y: (a1, a2, a3)): Bool = match (x <=> y) {
        case GreaterThan => true
        case _ => false
    }
    def greaterEqual(x: (a1, a2, a3), y: (a1, a2, a3)): Bool = match (x <=> y) {
        case GreaterThan => true
        case Equal => true
        case _ => false
    }

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    def compare(t1: (a1, a2, a3), t2: (a1, a2, a3)): Ordering =
        let (x1, x2, x3) = t1;
        let (y1, y2, y3) = t2;
            let cmp1 = x1 <=> y1;
            if (cmp1 != Equal)
                cmp1
            else
                let cmp2 = x2 <=> y2;
                if (cmp2 != Equal)
                    cmp2
                else
                    x3 <=> y3

}

instance Ord[(a1, a2, a3, a4)] with [a1 : Ord, a2: Ord, a3 : Ord, a4 : Ord] {

    def less(x: (a1, a2, a3, a4), y: (a1, a2, a3, a4)): Bool = match (x <=> y) {
        case LessThan => true
        case _ => false
    }
    def lessEqual(x: (a1, a2, a3, a4), y: (a1, a2, a3, a4)): Bool = match (x <=> y) {
        case LessThan => true
        case Equal => true
        case _ => false
    }
    def greater(x: (a1, a2, a3, a4), y: (a1, a2, a3, a4)): Bool = match (x <=> y) {
        case GreaterThan => true
        case _ => false
    }
    def greaterEqual(x: (a1, a2, a3, a4), y: (a1, a2, a3, a4)): Bool = match (x <=> y) {
        case GreaterThan => true
        case Equal => true
        case _ => false
    }

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    def compare(t1: (a1, a2, a3, a4), t2: (a1, a2, a3, a4)): Ordering =
        let (x1, x2, x3, x4) = t1;
        let (y1, y2, y3, y4) = t2;
            let cmp1 = x1 <=> y1;
            if (cmp1 != Equal)
                cmp1
            else
                let cmp2 = x2 <=> y2;
                if (cmp2 != Equal)
                    cmp2
                else
                    let cmp3 = x3 <=> y3;
                    if (cmp3 != Equal)
                        cmp3
                    else
                        x4 <=> y4

}