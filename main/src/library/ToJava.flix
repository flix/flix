/*
 *  Copyright 2023 Stephen Tetley
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

import java.lang.Byte
import java.lang.Character
import java.lang.Double
import java.lang.Float
import java.lang.Integer
import java.lang.Long
import java.lang.Short
import java.lang.{String => JString}
import java.math.BigDecimal
import java.math.BigInteger

///
/// A trait for marshaling values to Java objects.
///
pub trait ToJava[t: Type] {
    type Out[t]: Type
    type Aef[t]: Eff = {}
    pub def toJava(t: t): ToJava.Out[t] \ ToJava.Aef[t]
}

mod ToJava {

    import java.lang.Object

    ///
    /// Marshal a type that supports `ToJava` to its respective to `ToJava.Out` type
    /// and upcast it to a `java.lang.Object`
    ///
    pub def toJavaObject(t: t): Object \ ToJava.Aef[t] with ToJava[t] =
        unchecked_cast(ToJava.toJava(t) as Object)

}

instance ToJava[Int8] {
    type Out = Byte
    pub def toJava(i: Int8): Byte = Int8.valueOf(i)
}

instance ToJava[Int16] {
    type Out = Short
    pub def toJava(i: Int16): Short = Int16.valueOf(i)
}

instance ToJava[Int32] {
    type Out = Integer
    pub def toJava(i: Int32): Integer = Int32.valueOf(i)
}

instance ToJava[Int64] {
    type Out = Long
    pub def toJava(i: Int64): Long = Int64.valueOf(i)
}

instance ToJava[Float32] {
    type Out = Float
    pub def toJava(d: Float32): Float = Float32.valueOf(d)
}

instance ToJava[Float64] {
    type Out = Double
    pub def toJava(d: Float64): Double = Float64.valueOf(d)
}

instance ToJava[BigInt] {
    type Out = BigInteger
    pub def toJava(i: BigInt): BigInteger = i
}

instance ToJava[BigDecimal] {
    type Out = BigDecimal
    pub def toJava(d: BigDecimal): BigDecimal = d
}

instance ToJava[Char] {
    type Out = Character
    pub def toJava(c: Char): Character = Char.valueOf(c)
}

instance ToJava[String] {
    type Out = JString
    pub def toJava(s: String): JString = s
}
