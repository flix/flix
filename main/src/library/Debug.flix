/*
 * Copyright 2021 Magnus Madsen
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
/// A type class that provides an unsafe way to print Flix values.
///
/// The type class is automatically implemented by Flix.
///
lawless sealed class Debug[a] {
    ///
    /// Returns a string representation of the given value `x`.
    ///
    pub def format(_x: a): String = "<<No Debug>>"
}

instance Debug[Unit] {
    pub override def format(_x: Unit): String = "()"
}

instance Debug[Bool] {
    pub override def format(x: Bool): String = match x {
        case true  => "true"
        case false => "false"
    }
}

instance Debug[Char] {
    pub override def format(x: Char): String =
        import static java.lang.Character.toString(Char);
        "'${toString(x)}'" as & Pure
}

instance Debug[Float32] {
    pub override def format(x: Float32): String =
        import java.lang.Float:toString(Float32);
        "${toString(x)}f32" as & Pure
}

instance Debug[Float64] {
    pub override def format(x: Float64): String =
        import java.lang.Double:toString(Float64);
        "${toString(x)}f64" as & Pure
}

instance Debug[Int8] {
    pub override def format(x: Int8): String =
        import java.lang.Byte:toString(Int8);
        "${toString(x)}i8" as & Pure
}

instance Debug[Int16] {
    pub override def format(x: Int16): String =
        import java.lang.Short:toString(Int16);
        "${toString(x)}i16" as & Pure
}

instance Debug[Int32] {
    pub override def format(x: Int32): String =
        import java.lang.Integer:toString(Int32);
        "${toString(x)}i32" as & Pure
}

instance Debug[Int64] {
    pub override def format(x: Int64): String =
        import java.lang.Long:toString(Int64);
        "${toString(x)}i64" as & Pure
}

instance Debug[BigInt] {
    pub override def format(x: BigInt): String =
        import java.math.BigInteger.toString();
        "${toString(x)}ii" as & Pure
}

instance Debug[String] {
    pub override def format(x: String): String =
        "\"${x}\""
}

// TODO: Add tuple instances.

// TODO: Auto derive Enum instances.

// TODO: Array and other instances.

// TODO: Ref instances.

