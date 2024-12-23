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
import java.math.BigInteger

///
/// A trait for marshaling values to Flix objects.
///
pub trait ToFlix[t: Type] {
    type In[t]: Type
    type Aef[t]: Eff = {}
    pub def toFlix(t: ToFlix.In[t]): t \ ToFlix.Aef[t]
}

instance ToFlix[Int8] {
    type In = Byte
    pub def toFlix(i: Byte): Int8 = Int8.byteValue(i)
}

instance ToFlix[Int16] {
    type In = Short
    pub def toFlix(i: Short): Int16 = Int16.shortValue(i)
}

instance ToFlix[Int32] {
    type In = Integer
    pub def toFlix(i: Integer): Int32 = Int32.intValue(i)
}

instance ToFlix[Int64] {
    type In = Long
    pub def toFlix(i: Long): Int64 = Int64.longValue(i)
}

instance ToFlix[Float32] {
    type In = Float
    pub def toFlix(d: Float): Float32 = Float32.floatValue(d)
}

instance ToFlix[Float64] {
    type In = Double
    pub def toFlix(d: Double): Float64 = Float64.doubleValue(d)
}

instance ToFlix[BigInt] {
    type In = BigInteger
    pub def toFlix(i: BigInteger): BigInt = i
}

instance ToFlix[BigDecimal] {
    type In = BigDecimal
    pub def toFlix(d: BigDecimal): BigDecimal = d
}

instance ToFlix[Char] {
    type In = Character
    pub def toFlix(c: Character): Char = Char.charValue(c)
}

instance ToFlix[String] {
    type In = String
    pub def toFlix(s: String): String = s
}
