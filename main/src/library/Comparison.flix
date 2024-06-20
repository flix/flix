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

pub enum Comparison with Eq, ToString, Sendable {
    case LessThan
    case EqualTo
    case GreaterThan
}

mod Comparison {

    ///
    /// Converts a `Comparison` value to an integer.
    /// The return value is:
    /// `-1` if `c` is `LessThan`.
    /// `0` if `c` is `EqualTo`.
    /// `+1` if `c` is `GreaterThan`.
    ///
    pub def toInt32(c: Comparison): Int32 = match c {
        case LessThan => -1
        case EqualTo => 0
        case GreaterThan => +1
    }

    ///
    /// Converts an integer to a `Comparison` value.
    /// The return value is:
    /// `LessThan` if `i < 0`.
    /// `EqualTo` if `i == 0`.
    /// `GreaterThan` if `i > 0`.
    ///
    pub def fromInt32(i: Int32): Comparison =
        if (i < 0) LessThan
        else if (i == 0) EqualTo
        else GreaterThan

}
