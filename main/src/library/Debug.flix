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
    /// Prints the stringified representation of `x` with the given `prefix`.
    ///
    /// Note: For internal use only. The actual `dbg(exp)` expression is parsed and ultimately rewritten to call this function.
    ///
    @Internal
    pub def debugWithPrefix(prefix: String, x: a): a = unsafe {
        System.out.println("${prefix}%{x}");
        x
    }

    ///
    /// Escapes the given string per Flix's escaping rules.
    ///
    @Internal
    def escape(s: String): String = {
        def replace(src: String, dst: String, subject: String): String = unsafe subject.replace(src, dst);
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
    pub def stringify(x: a): String = typematch x {
        case _: Unit => "()"
        case y: BigDecimal =>
            if (unsafe Objects.isNull(y)) {
                "null"
            } else {
                unsafe y.toString() + "ff"
            }
        case y: BigInt =>
            if (unsafe Objects.isNull(y)) {
                "null"
            } else {
                unsafe y.toString() + "ii"
            }
        case s: String =>
            if (unsafe Objects.isNull(s)) {
                "null"
            } else {
                "\"" + escape(s) + "\""
            }
        case y: Array[Bool, _] =>
            unsafe Arrays.toString(unchecked_cast(y as Array[Bool, Static]))
        case y: Array[Char, _] =>
            unsafe Arrays.toString(unchecked_cast(y as Array[Char, Static]))
        case y: Array[Float32, _] =>
            unsafe Arrays.toString(unchecked_cast(y as Array[Float32, Static]))
        case y: Array[Float64, _] =>
            unsafe Arrays.toString(unchecked_cast(y as Array[Float64, Static]))
        case y: Array[Int8, _] =>
            unsafe Arrays.toString(unchecked_cast(y as Array[Int8, Static]))
        case y: Array[Int16, _] =>
            unsafe Arrays.toString(unchecked_cast(y as Array[Int16, Static]))
        case y: Array[Int32, _] =>
            unsafe Arrays.toString(unchecked_cast(y as Array[Int32, Static]))
        case y: Array[Int64, _] =>
            unsafe Arrays.toString(unchecked_cast(y as Array[Int64, Static]))
        case y: Array[Unit, _] =>
            unsafe Arrays.toString(unchecked_cast(y as Array[Object, Static]))
        case y: Array[BigInt, _] =>
            unsafe Arrays.toString(unchecked_cast(y as Array[Object, Static]))
        case y: Array[BigDecimal, _] =>
            unsafe Arrays.toString(unchecked_cast(y as Array[Object, Static]))
        case y: Array[String, _] =>
            unsafe Arrays.toString(unchecked_cast(y as Array[Object, Static]))
        case y: Array[Array[_, _], _] =>
            unsafe Arrays.deepToString(unchecked_cast(y as Array[Object, Static]))
        case y: Array[_, _] =>
            unsafe Arrays.toString(unchecked_cast(y as Array[Object, Static]))
        case _: _ => match JvmReflection.reflectValue(x) {
            case JvmValue.JvmChar(c) => "\'" + escape("${c}") + "\'"
            case JvmValue.JvmBool(b) => if (b) "true" else "false"
            case JvmValue.JvmInt8(i) => Byte.toString(i) + "i8"
            case JvmValue.JvmInt16(i) => Short.toString(i) + "i16"
            case JvmValue.JvmInt32(i) => Integer.toString(i)
            case JvmValue.JvmInt64(i) => Long.toString(i) + "i64"
            case JvmValue.JvmFloat32(i) => Float.toString(i) + "f32"
            case JvmValue.JvmFloat64(i) => Double.toString(i)
            case JvmValue.JvmObject(obj) => unsafe Objects.toString(obj)
        }
    }

}
