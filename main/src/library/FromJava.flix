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
    type In = ##java.lang.Short
    pub def fromJava(i: ##java.lang.Short): Int16 = Int16.shortValue(i)
}

instance FromJava[Int32] {
    type In = ##java.lang.Integer
    pub def fromJava(i: ##java.lang.Integer): Int32 = Int32.intValue(i)
}

instance FromJava[Int64] {
    type In = ##java.lang.Long
    pub def fromJava(i: ##java.lang.Long): Int64 = Int64.longValue(i)
}

instance FromJava[Float32] {
    type In = ##java.lang.Float
    pub def fromJava(d: ##java.lang.Float): Float32 = Float32.floatValue(d)
}

instance FromJava[Float64] {
    type In = ##java.lang.Double
    pub def fromJava(d: ##java.lang.Double): Float64 = Float64.doubleValue(d)
}

instance FromJava[BigInt] {
    type In = ##java.math.BigInteger
    pub def fromJava(i: ##java.math.BigInteger): BigInt = i
}

instance FromJava[BigDecimal] {
    type In = ##java.math.BigDecimal
    pub def fromJava(d: ##java.math.BigDecimal): BigDecimal = d
}

instance FromJava[Char] {
    type In = ##java.lang.Character
    pub def fromJava(c: ##java.lang.Character): Char = Char.charValue(c)
}

instance FromJava[String] {
    type In = ##java.lang.String
    pub def fromJava(s: ##java.lang.String): String = s
}
