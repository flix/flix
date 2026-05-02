/*
 *  Copyright 2020 Matthew Lutze
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
/// A trait for types that can be constructed from strings.
///
pub trait FromString[a] {
    ///
    /// Optionally returns the value associated with the given string `s`.
    ///
    pub def fromString(x: String): Option[a]
}

instance FromString[Unit] {
    pub def fromString(x: String): Option[Unit] = match x {
        case "()" => Some(())
        case _    => None
    }
}

instance FromString[Bool] {
    pub def fromString(x: String): Option[Bool] = match x {
        case "true"  => Some(true)
        case "false" => Some(false)
        case _       => None
    }
}

instance FromString[Char] {
    pub def fromString(x: String): Option[Char] =
        if (String.length(x) == 1)
            Some(String.charAt(0, x))
        else
            None
}

instance FromString[Float32] {
    pub def fromString(x: String): Option[Float32] = Float32.fromString(x)
}

instance FromString[Float64] {
    pub def fromString(x: String): Option[Float64] = Float64.fromString(x)
}

instance FromString[BigDecimal] {
    pub def fromString(x: String): Option[BigDecimal] = BigDecimal.fromString(x)
}

instance FromString[Int8] {
    pub def fromString(x: String): Option[Int8] = Int8.fromString(x)
}

instance FromString[Int16] {
    pub def fromString(x: String): Option[Int16] = Int16.fromString(x)
}

instance FromString[Int32] {
    pub def fromString(x: String): Option[Int32] = Int32.fromString(x)
}

instance FromString[Int64] {
    pub def fromString(x: String): Option[Int64] = Int64.fromString(x)
}

instance FromString[BigInt] {
    pub def fromString(x: String): Option[BigInt] = BigInt.fromString(x)
}

instance FromString[String] {
    pub def fromString(x: String): Option[String] = Some(x)
}
