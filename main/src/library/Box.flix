/*
 * Copyright 2024 Joseph Tan
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

import java.lang.Boolean
import java.lang.Character
import java.lang.Float
import java.lang.Double
import java.lang.Byte
import java.lang.Short
import java.lang.Integer
import java.lang.Long

///
/// A trait for primitive types that can be boxed and unboxed.
///
trait Box[a] {
    type Boxed

    ///
    /// Returns the boxed representation of the given `x`.
    ///
    pub def box(x: a): Box.Boxed[a]

    ///
    /// Returns the unboxed representation of the given `x`.
    ///
    pub def unbox(x: Box.Boxed[a]): a
}

instance Box[Bool] {
    type Boxed = Boolean
    pub def box(x: Bool): Boolean = Boolean.valueOf(x)
    pub def unbox(x: Boolean): Bool = x.booleanValue()
}

instance Box[Char] {
    type Boxed = Character
    pub def box(x: Char): Character = Character.valueOf(x)
    pub def unbox(x: Character): Char = x.charValue()
}

instance Box[Float32] {
    type Boxed = Float
    pub def box(x: Float32): Float = Float.valueOf(x)
    pub def unbox(x: Float): Float32 = x.floatValue()
}

instance Box[Float64] {
    type Boxed = Double
    pub def box(x: Float64): Double = Double.valueOf(x)
    pub def unbox(x: Double): Float64 = x.doubleValue()
}

instance Box[Int8] {
    type Boxed = Byte
    pub def box(x: Int8): Byte = Byte.valueOf(x)
    pub def unbox(x: Byte): Int8 = x.byteValue()
}

instance Box[Int16] {
    type Boxed = Short
    pub def box(x: Int16): Short = Short.valueOf(x)
    pub def unbox(x: Short): Int16 = x.shortValue()
}

instance Box[Int32] {
    type Boxed = Integer
    pub def box(x: Int32): Integer = Integer.valueOf(x)
    pub def unbox(x: Integer): Int32 = x.intValue()
}

instance Box[Int64] {
    type Boxed = Long
    pub def box(x: Int64): Long = Long.valueOf(x)
    pub def unbox(x: Long): Int64 = x.longValue()
}
