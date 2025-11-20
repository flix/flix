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
/// An effect used for print debugging.
///
eff Debug

mod Debug {

    import java.lang.Byte
    import java.lang.Double
    import java.lang.Float
    import java.lang.Integer
    import java.lang.Long
    import java.lang.Object
    import java.lang.Short
    import java.lang.System
    import java.util.Arrays
    import java.util.Objects

    ///
    /// Prints `x` to the standard out.
    ///
    pub def dprint(x: a): Unit \ (Debug + Formattable.Aef[a]) with Formattable[a] =
        let rs = Formattable.format(x);
        let s = RichString.toString(rs);
        unchecked_cast({System.out.print(s)} as _ \ Debug)

    ///
    /// Prints `x` to the standard out followed by a new line.
    ///
    pub def dprintln(x: a): Unit \ (Debug + Formattable.Aef[a]) with Formattable[a] =
        let rs = Formattable.format(x);
        let s = RichString.toString(rs);
        unchecked_cast({System.out.println(s)} as _ \ Debug)

    ///
    /// Escapes the given string per Flix's escaping rules.
    ///
    @Internal
    def escape(s: String): String = {
        def replace(src: String, dst: String, subject: String): String = subject.replace(src, dst);
        s
            // NB: \\ must come first to avoid clobbering other cases
            |> replace("\\", "\\\\")
            |> replace("\n", "\\n")
            |> replace("\r", "\\r")
            |> replace("\"", "\\\"")
            |> replace("\'", "\\\'")
            |> replace("\t", "\\t")
    }

    ///
    /// Returns an automatic string representation of `x`.
    ///
    @Internal
    pub def stringify(x: a): String = {
        typematch x {
            case b: Bool => if (b) "true" else "false"
            case c: Char => "\'" + escape("${(c: Char)}") + "\'"
            case y: Float32 =>
                Float.toString(y) + "f32"
            case y: Float64 =>
                Double.toString(y)
            case y: BigDecimal =>
                if (Objects.isNull(y)) {
                    "null"
                } else {
                    y.toString() + "ff"
                }
            case y: Int8 =>
                Byte.toString(y) + "i8"
            case y: Int16 =>
                Short.toString(y) + "i16"
            case y: Int32 =>
                Integer.toString(y)
            case y: Int64 =>
                Long.toString(y) + "i64"
            case y: BigInt =>
                if (Objects.isNull(y)) {
                    "null"
                } else {
                    y.toString() + "ii"
                }
            case s: String =>
                if (Objects.isNull(s)) {
                    "null"
                } else {
                    "\"" + escape(s) + "\""
                }
            case y: Array[Bool, _] =>
                unchecked_cast(Arrays.toString(unchecked_cast(y as Array[Bool, Static])) as _ \ {})
            case y: Array[Char, _] =>
                unchecked_cast(Arrays.toString(unchecked_cast(y as Array[Char, Static])) as _ \ {})
            case y: Array[Float32, _] =>
                unchecked_cast(Arrays.toString(unchecked_cast(y as Array[Float32, Static])) as _ \ {})
            case y: Array[Float64, _] =>
                unchecked_cast(Arrays.toString(unchecked_cast(y as Array[Float64, Static])) as _ \ {})
            case y: Array[Int8, _] =>
                unchecked_cast(Arrays.toString(unchecked_cast(y as Array[Int8, Static])) as _ \ {})
            case y: Array[Int16, _] =>
                unchecked_cast(Arrays.toString(unchecked_cast(y as Array[Int16, Static])) as _ \ {})
            case y: Array[Int32, _] =>
                unchecked_cast(Arrays.toString(unchecked_cast(y as Array[Int32, Static])) as _ \ {})
            case y: Array[Int64, _] =>
                unchecked_cast(Arrays.toString(unchecked_cast(y as Array[Int64, Static])) as _ \ {})
            case y: Array[_, _] =>
                unchecked_cast(Arrays.deepToString(unchecked_cast(y as Array[Object, Static])) as _ \ {})
            case y: _ =>
                unchecked_cast(Objects.toString(unchecked_cast(y as Object)) as _ \ {})
        }
    }

}
