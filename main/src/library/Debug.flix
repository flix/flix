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
    pub def stringify(x: a): String = match $JVM_TYPE$(x) {
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
