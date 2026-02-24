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

pub mod Debug {

    import java.lang.Byte
    import java.lang.Double
    import java.lang.Float
    import java.lang.Integer
    import java.lang.Long
    import java.lang.Object
    import java.lang.Short
    import java.lang.{String => JString}
    import java.lang.System
    import java.lang.{Class => JClass}
    import java.math.BigInteger
    import java.math.{BigDecimal => JBigDecimal}
    import java.util.Arrays
    import java.util.Objects

    ///
    /// Prints `x` to the standard out.
    ///
    pub def dprint(x: a): Unit \ (Debug + Formattable.Aef[a]) with Formattable[a] =
        let rs = Formattable.format(x);
        let s = RichString.toString(rs);
        unsafe IO as Debug { System.out.print(s) }

    ///
    /// Prints `x` to the standard out followed by a new line.
    ///
    pub def dprintln(x: a): Unit \ (Debug + Formattable.Aef[a]) with Formattable[a] =
        let rs = Formattable.format(x);
        let s = RichString.toString(rs);
        unsafe IO as Debug { System.out.println(s) }

    ///
    /// Escapes the given string per Flix's escaping rules.
    ///
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
    pub def stringify(x: a): String = unsafe IO {
        use Reflect.JvmValue;
        match Reflect.reflectValue(x) {
            case JvmValue.JvmBool(b)      => if (b) "true" else "false"
            case JvmValue.JvmChar(c)      => "'" + escape("${(c: Char)}") + "'"
            case JvmValue.JvmFloat32(y)   => Float.toString(y) + "f32"
            case JvmValue.JvmFloat64(y)   => Double.toString(y)
            case JvmValue.JvmInt8(y)      => Byte.toString(y) + "i8"
            case JvmValue.JvmInt16(y)     => Short.toString(y) + "i16"
            case JvmValue.JvmInt32(y)     => Integer.toString(y)
            case JvmValue.JvmInt64(y)     => Long.toString(y) + "i64"
            case JvmValue.JvmObject(obj)  =>
                if (Objects.isNull(obj)) {
                    "null"
                } else if (obj instanceof JString) {
                    "\"" + escape(unchecked_cast(obj as String)) + "\""
                } else if (obj instanceof BigInteger) {
                    unchecked_cast(obj as BigInteger).toString() + "ii"
                } else if (obj instanceof JBigDecimal) {
                    unchecked_cast(obj as JBigDecimal).toString() + "ff"
                } else {
                    objectToString(obj)
                }
        }
    }

    ///
    /// Returns a string representation of the given object,
    /// with special handling for arrays.
    ///
    def objectToString(obj: Object): String \ IO = {
        let clazz = obj.getClass();
        if (clazz.isArray()) {
            let componentType = clazz.getComponentType();
            if (componentType.isPrimitive()) {
                let name = componentType.getName();
                if (name == "boolean") {
                    Arrays.toString(unchecked_cast(obj as Array[Bool, Static]))
                } else if (name == "char") {
                    Arrays.toString(unchecked_cast(obj as Array[Char, Static]))
                } else if (name == "float") {
                    Arrays.toString(unchecked_cast(obj as Array[Float32, Static]))
                } else if (name == "double") {
                    Arrays.toString(unchecked_cast(obj as Array[Float64, Static]))
                } else if (name == "byte") {
                    Arrays.toString(unchecked_cast(obj as Array[Int8, Static]))
                } else if (name == "short") {
                    Arrays.toString(unchecked_cast(obj as Array[Int16, Static]))
                } else if (name == "int") {
                    Arrays.toString(unchecked_cast(obj as Array[Int32, Static]))
                } else if (name == "long") {
                    Arrays.toString(unchecked_cast(obj as Array[Int64, Static]))
                } else {
                    Objects.toString(obj)
                }
            } else {
                Arrays.deepToString(unchecked_cast(obj as Array[Object, Static]))
            }
        } else {
            Objects.toString(obj)
        }
    }

}
