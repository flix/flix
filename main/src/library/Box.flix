/*
 * Copyright 2024 Joseph Tan
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

import java.lang.Integer

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

// Add all instances, please keep them in the same order as in e.g. Eq

instance Box[Unit] {
    type Boxed = ???
    pub def box(x: Int32): Integer = ???
    pub def unbox(x: Integer): Int32 = ???
}

instance Box[Bool] {
    type Boxed = ???
    pub def box(x: Int32): Integer = ???
    pub def unbox(x: Integer): Int32 = ???
}

instance Box[Char] {
    type Boxed = ???
    pub def box(x: Int32): Integer = ???
    pub def unbox(x: Integer): Int32 = ???
}

instance Box[Float32] {
    type Boxed = Integer
    pub def box(x: Int32): Integer = unsafe Integer.valueOf(x)
    pub def unbox(x: Integer): Int32 = unsafe x.intValue()
}

instance Box[Float64] {
    type Boxed = Integer
    pub def box(x: Int32): Integer = unsafe Integer.valueOf(x)
    pub def unbox(x: Integer): Int32 = unsafe x.intValue()
}

instance Box[BigDecimal] {
    type Boxed = Integer
    pub def box(x: Int32): Integer = unsafe Integer.valueOf(x)
    pub def unbox(x: Integer): Int32 = unsafe x.intValue()
}

instance Box[Int8] {
    type Boxed = Integer
    pub def box(x: Int32): Integer = unsafe Integer.valueOf(x)
    pub def unbox(x: Integer): Int32 = unsafe x.intValue()
}

instance Box[Int16] {
    type Boxed = Integer
    pub def box(x: Int32): Integer = unsafe Integer.valueOf(x)
    pub def unbox(x: Integer): Int32 = unsafe x.intValue()
}

instance Box[Int32] {
    type Boxed = Integer
    pub def box(x: Int32): Integer = unsafe Integer.valueOf(x)
    pub def unbox(x: Integer): Int32 = unsafe x.intValue()
}

instance Box[Int64] {
    type Boxed = Integer
    pub def box(x: Int32): Integer = unsafe Integer.valueOf(x)
    pub def unbox(x: Integer): Int32 = unsafe x.intValue()
}

instance Box[BigInt] {
    type Boxed = Integer
    pub def box(x: Int32): Integer = unsafe Integer.valueOf(x)
    pub def unbox(x: Integer): Int32 = unsafe x.intValue()
}

instance Box[String] {
    type Boxed = Integer
    pub def box(x: Int32): Integer = unsafe Integer.valueOf(x)
    pub def unbox(x: Integer): Int32 = unsafe x.intValue()
}
