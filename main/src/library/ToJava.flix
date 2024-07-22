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
    type Out = ##java.lang.Byte
    pub def toJava(i: Int8): ##java.lang.Byte = Int8.valueOf(i)
}

instance ToJava[Int16] {
    type Out = ##java.lang.Short
    pub def toJava(i: Int16): ##java.lang.Short = Int16.valueOf(i)
}

instance ToJava[Int32] {
    type Out = ##java.lang.Integer
    pub def toJava(i: Int32): ##java.lang.Integer = Int32.valueOf(i)
}

instance ToJava[Int64] {
    type Out = ##java.lang.Long
    pub def toJava(i: Int64): ##java.lang.Long = Int64.valueOf(i)
}

instance ToJava[Float32] {
    type Out = ##java.lang.Float
    pub def toJava(d: Float32): ##java.lang.Float = Float32.valueOf(d)
}

instance ToJava[Float64] {
    type Out = ##java.lang.Double
    pub def toJava(d: Float64): ##java.lang.Double = Float64.valueOf(d)
}

instance ToJava[BigInt] {
    type Out = ##java.math.BigInteger
    pub def toJava(i: BigInt): ##java.math.BigInteger = i
}

instance ToJava[BigDecimal] {
    type Out = ##java.math.BigDecimal
    pub def toJava(d: BigDecimal): ##java.math.BigDecimal = d
}

instance ToJava[Char] {
    type Out = ##java.lang.Character
    pub def toJava(c: Char): ##java.lang.Character = Char.valueOf(c)
}

instance ToJava[String] {
    type Out = ##java.lang.String
    pub def toJava(s: String): ##java.lang.String = s
}
