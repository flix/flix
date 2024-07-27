/*
 *  Copyright 2024 Stephen Tetley
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
/// A trait for marshaling values from Java objects.
///
pub trait FromJava[t: Type] {
    type In[t]: Type
    type Aef[t]: Eff = {}
    pub def fromJava(t: FromJava.In[t]): t \ FromJava.Aef[t]
}

instance FromJava[Int8] {
    type In = Byte
    pub def fromJava(i: Byte): Int8 = Int8.byteValue(i)
}

instance FromJava[Int16] {
    type In = Short
    pub def fromJava(i: Short): Int16 = Int16.shortValue(i)
}

instance FromJava[Int32] {
    type In = Integer
    pub def fromJava(i: Integer): Int32 = Int32.intValue(i)
}

instance FromJava[Int64] {
    type In = Long
    pub def fromJava(i: Long): Int64 = Int64.longValue(i)
}

instance FromJava[Float32] {
    type In = Float
    pub def fromJava(d: Float): Float32 = Float32.floatValue(d)
}

instance FromJava[Float64] {
    type In = Double
    pub def fromJava(d: Double): Float64 = Float64.doubleValue(d)
}

instance FromJava[BigInt] {
    type In = BigInteger
    pub def fromJava(i: BigInteger): BigInt = i
}

instance FromJava[BigDecimal] {
    type In = BigDecimal
    pub def fromJava(d: BigDecimal): BigDecimal = d
}

instance FromJava[Char] {
    type In = Character
    pub def fromJava(c: Character): Char = Char.charValue(c)
}

instance FromJava[String] {
    type In = JString
    pub def fromJava(s: JString): String = s
}
