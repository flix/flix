/*
 * Copyright 2020 Esben Bjerre
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Comparison {

    pub enum Comparison with Eq, ToString {
        case LessThan
        case EqualTo
        case GreaterThan
    }

    ///
    /// Converts a `Comparison` value to an integer.
    /// The return value is:
    /// `-1` if `c` is `LessThan`.
    /// `0` if `c` is `EqualTo`.
    /// `+1` if `c` is `GreaterThan`.
    ///
    pub def toInt32(c: Comparison): Int32 = match c {
        case LessThan    => -1
        case EqualTo     => 0
        case GreaterThan => 1
    }

    ///
    /// Converts an integer to a `Comparison` value.
    /// The return value is:
    /// `LessThan` if `i < 0`.
    /// `EqualTo` if `i == 0`.
    /// `GreaterThan` if `i > 0`.
    ///
    pub def fromInt32(i: Int32): Comparison =
        if (i < 0)
            LessThan
        else if (i == 0)
            EqualTo
        else
            GreaterThan

}
