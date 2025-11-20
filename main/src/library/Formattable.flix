/*
 * Copyright 2025 Magnus Madsen
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

///
/// A trait for types that can be formatted as a `RichString`.
///
pub trait Formattable[a] {
    ///
    /// The associated effect of the Formattable which represents the effect of formatting.
    ///
    type Aef[a]: Eff = {}

    pub def format(x: a): RichString \ Formattable.Aef[a]
}

instance Formattable[Unit] {
    pub def format(x: Unit): RichString = RichString.fromString(ToString.toString(x))
}

instance Formattable[Bool] {
    pub def format(x: Bool): RichString = RichString.fromString(ToString.toString(x))
}

instance Formattable[Char] {
    pub def format(x: Char): RichString = RichString.fromString(ToString.toString(x))
}

instance Formattable[Int8] {
    pub def format(x: Int8): RichString = RichString.fromString(ToString.toString(x))
}

instance Formattable[Int16] {
    pub def format(x: Int16): RichString = RichString.fromString(ToString.toString(x))
}

instance Formattable[Int32] {
    pub def format(x: Int32): RichString = RichString.fromString(ToString.toString(x))
}

instance Formattable[Int64] {
    pub def format(x: Int64): RichString = RichString.fromString(ToString.toString(x))
}

instance Formattable[Float32] {
    pub def format(x: Float32): RichString = RichString.fromString(ToString.toString(x))
}

instance Formattable[Float64] {
    pub def format(x: Float64): RichString = RichString.fromString(ToString.toString(x))
}

instance Formattable[String] {
    pub def format(x: String): RichString = RichString.fromString(x)
}

instance Formattable[BigInt] {
    pub def format(x: BigInt): RichString = RichString.fromString(ToString.toString(x))
}

instance Formattable[BigDecimal] {
    pub def format(x: BigDecimal): RichString = RichString.fromString(ToString.toString(x))
}

instance Formattable[(a1, a2)] with Formattable[a1], Formattable[a2] {
    type Aef[(a1, a2)] = Formattable.Aef[a1] + Formattable.Aef[a2]

    pub def format(x: (a1, a2)): RichString \ Formattable.Aef[a1] + Formattable.Aef[a2] =
        let (v1, v2) = x;
        RichString.fromString("(") + Formattable.format(v1) + RichString.fromString(", ") + Formattable.format(v2) + RichString.fromString(")")
}
